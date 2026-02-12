//! Codegen traversal utilities for SSA functions.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::backend::regalloc::ValueAllocMap;
use crate::backend::regalloc::moves::MoveOp;
use crate::backend::regalloc::target::PhysReg;
use crate::ir::for_each_inst_use;
use crate::ir::layout::IrLayout;
use crate::ir::{
    Block, BlockId, Callee, ConstValue, Function, InstKind, Instruction, LocalId, Terminator,
    ValueId,
};
use crate::ir::{IrTypeCache, IrTypeId, IrTypeKind};
use crate::resolve::DefId;

use super::emitter::{CodegenEmitter, LocationResolver};
use super::graph::{CodegenBlockId, CodegenEmit, CodegenGraph};
use super::moves::MoveBlockId;

/// Callback interface used by the traversal to emit codegen steps.
pub trait CodegenSink {
    fn enter_block(&mut self, block: CodegenBlockId);
    fn emit_moves(&mut self, moves: &[MoveOp]);
    fn emit_inst(&mut self, inst: &Instruction);
    fn emit_terminator(&mut self, term: &Terminator);
    fn emit_branch(&mut self, target: CodegenBlockId);
    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        then_target: CodegenBlockId,
        else_target: CodegenBlockId,
    );
    fn emit_switch(
        &mut self,
        value: ValueId,
        cases: &[(ConstValue, CodegenBlockId)],
        default_target: CodegenBlockId,
    );
}

/// Emits code using a target emitter and allocation map.
#[allow(clippy::too_many_arguments)]
pub fn emit_graph_with_emitter(
    graph: &CodegenGraph,
    func: &Function,
    alloc_map: &ValueAllocMap,
    frame_size: u32,
    callee_saved: &[PhysReg],
    types: &mut IrTypeCache,
    def_names: &HashMap<DefId, String>,
    func_label: &str,
    emitter: &mut dyn CodegenEmitter,
) {
    struct EmitSink<'a> {
        emitter: &'a mut dyn CodegenEmitter,
        locs: LocationResolver<'a>,
        label_prefix: String,
    }

    impl<'a> CodegenSink for EmitSink<'a> {
        fn enter_block(&mut self, block: CodegenBlockId) {
            let base = match block {
                CodegenBlockId::Ssa(id) => format!("bb{}", id.0),
                CodegenBlockId::Move(id) => format!("mb{}", id.0),
            };
            let label = format!("{}_{}", self.label_prefix, base);
            self.emitter.begin_block(&label);
        }

        fn emit_moves(&mut self, moves: &[MoveOp]) {
            self.emitter.emit_moves(moves);
        }

        fn emit_inst(&mut self, inst: &Instruction) {
            self.emitter.emit_inst(inst, &self.locs);
        }

        fn emit_terminator(&mut self, term: &Terminator) {
            self.emitter.emit_terminator(term, &self.locs);
        }

        fn emit_branch(&mut self, target: CodegenBlockId) {
            self.emitter.emit_branch(target);
        }

        fn emit_cond_branch(
            &mut self,
            cond: ValueId,
            then_target: CodegenBlockId,
            else_target: CodegenBlockId,
        ) {
            self.emitter
                .emit_cond_branch(cond, then_target, else_target, &self.locs);
        }

        fn emit_switch(
            &mut self,
            value: ValueId,
            cases: &[(ConstValue, CodegenBlockId)],
            default_target: CodegenBlockId,
        ) {
            self.emitter
                .emit_switch(value, cases, default_target, &self.locs);
        }
    }

    let value_types = build_value_types(func);
    if cfg!(debug_assertions)
        && let Err(msg) = validate_value_types(func, &value_types)
    {
        panic!("backend codegen: {msg}");
    }
    let layouts = build_layouts(types, func, &value_types);
    let field_addr_folds = build_field_addr_folds(func, &value_types, &layouts, types);
    let const_zero_values = build_const_zero_values(func);
    let index_addr_folds = build_index_addr_folds(func, &const_zero_values);
    let const_zero_skips =
        build_const_zero_store_only(func, &value_types, &const_zero_values, types);
    let (local_offsets, locals_size) = build_local_offsets(func, &layouts, frame_size);
    let total_frame_size = frame_size.saturating_add(locals_size);
    let mut callee_saved = callee_saved.to_vec();
    if needs_sret(types, func.sig.ret) {
        let sret_reg = PhysReg(8);
        if !callee_saved.contains(&sret_reg) {
            callee_saved.push(sret_reg);
            callee_saved.sort_by_key(|reg| reg.0);
        }
    }

    let mut sink = EmitSink {
        emitter,
        locs: LocationResolver {
            map: alloc_map,
            value_types: &value_types,
            local_offsets: &local_offsets,
            types,
            layouts: &layouts,
            def_names,
            field_addr_folds: &field_addr_folds,
            index_addr_folds: &index_addr_folds,
            const_zero_values: &const_zero_values,
            const_zero_skips: &const_zero_skips,
        },
        label_prefix: format!(".L{}", func_label),
    };
    sink.emitter
        .begin_function(func_label, total_frame_size, &callee_saved);
    sink.emitter.emit_param_copies(graph.param_copies());
    sink.emitter.emit_moves(graph.entry_moves());
    emit_graph(graph, func, &mut sink);
    sink.emitter.end_function();
}

struct EmitContext<'a> {
    graph: &'a CodegenGraph,
    func: &'a Function,
    blocks_by_id: HashMap<BlockId, &'a Block>,
    move_blocks: HashMap<MoveBlockId, &'a [MoveOp]>,
}

impl<'a> EmitContext<'a> {
    fn new(graph: &'a CodegenGraph, func: &'a Function) -> Self {
        let mut blocks_by_id = HashMap::new();
        for block in &func.blocks {
            blocks_by_id.insert(block.id, block);
        }

        let mut move_blocks = HashMap::new();
        for block in graph.blocks() {
            if let CodegenBlockId::Move(id) = block.id {
                move_blocks.insert(id, block.moves.as_slice());
            }
        }

        Self {
            graph,
            func,
            blocks_by_id,
            move_blocks,
        }
    }

    fn ssa_block(&self, id: BlockId) -> &Block {
        self.blocks_by_id
            .get(&id)
            .copied()
            .unwrap_or_else(|| panic!("backend codegen: missing block {:?}", id))
    }

    fn move_block_moves(&self, id: MoveBlockId) -> &[MoveOp] {
        self.move_blocks
            .get(&id)
            .copied()
            .unwrap_or_else(|| panic!("backend codegen: missing move block {:?}", id))
    }

    fn edge_moves(&self, from: BlockId, to: BlockId) -> &[MoveOp] {
        self.graph.edge_moves(from, to).unwrap_or(&[])
    }

    fn edge_target(&self, from: BlockId, to: BlockId) -> CodegenBlockId {
        self.graph.edge_target(from, to)
    }
}

fn build_value_types(func: &Function) -> HashMap<ValueId, IrTypeId> {
    let mut map = HashMap::new();
    for block in &func.blocks {
        for param in &block.params {
            map.insert(param.value.id, param.value.ty);
        }
        for inst in &block.insts {
            if let Some(result) = &inst.result {
                map.insert(result.id, result.ty);
            }
        }
    }

    // Some branch arguments may refer to values not defined in a block yet; in
    // that case, infer their type from the target block parameter they feed.
    let mut blocks_by_id = HashMap::new();
    for block in &func.blocks {
        blocks_by_id.insert(block.id, block);
    }

    let mut fill_args = |target: BlockId, args: &[ValueId]| {
        let target_block = blocks_by_id.get(&target).unwrap_or_else(|| {
            panic!(
                "backend codegen: missing block {:?} for branch args",
                target
            )
        });
        if target_block.params.len() != args.len() {
            panic!(
                "backend codegen: block {:?} expects {} args, got {}",
                target,
                target_block.params.len(),
                args.len()
            );
        }
        for (idx, arg) in args.iter().enumerate() {
            if !map.contains_key(arg) {
                let param_ty = target_block.params[idx].value.ty;
                map.insert(*arg, param_ty);
            }
        }
    };

    for block in &func.blocks {
        match &block.term {
            Terminator::Br { target, args } => {
                fill_args(*target, args);
            }
            Terminator::CondBr {
                then_bb,
                then_args,
                else_bb,
                else_args,
                ..
            } => {
                fill_args(*then_bb, then_args);
                fill_args(*else_bb, else_args);
            }
            Terminator::Switch {
                cases,
                default,
                default_args,
                ..
            } => {
                for case in cases {
                    fill_args(case.target, &case.args);
                }
                fill_args(*default, default_args);
            }
            Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    map
}

fn build_layouts(
    types: &mut IrTypeCache,
    func: &Function,
    value_types: &HashMap<ValueId, IrTypeId>,
) -> HashMap<IrTypeId, IrLayout> {
    let mut layouts = HashMap::new();
    for ty in value_types
        .values()
        .copied()
        .chain(func.locals.iter().map(|l| l.ty))
    {
        collect_layouts(types, ty, &mut layouts);
    }
    layouts
}

fn build_field_addr_folds(
    func: &Function,
    value_types: &HashMap<ValueId, IrTypeId>,
    layouts: &HashMap<IrTypeId, IrLayout>,
    types: &IrTypeCache,
) -> HashMap<ValueId, (ValueId, u32)> {
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();
    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    let mut folds = HashMap::new();

    for block in func.blocks.iter() {
        for inst in block.insts.iter() {
            let InstKind::FieldAddr { base, index } = &inst.kind else {
                continue;
            };
            let Some(result) = &inst.result else {
                continue;
            };

            let Some(users) = uses.get(&result.id) else {
                continue;
            };
            if users.is_empty() {
                continue;
            }
            let mut ok = true;
            for (use_block, use_idx) in users {
                let use_inst = &func.blocks[*use_block].insts[*use_idx];
                match &use_inst.kind {
                    InstKind::Store { ptr, .. } if ptr == &result.id => {}
                    _ => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok {
                continue;
            }

            let Some(base_ty) = value_types.get(base).copied() else {
                continue;
            };
            let elem_ty = match types.kind(base_ty) {
                IrTypeKind::Ptr { elem } => *elem,
                _ => continue,
            };
            let Some(layout) = layouts.get(&elem_ty) else {
                continue;
            };
            let offset = layout.field_offsets().get(*index).copied().unwrap_or(0) as u32;
            folds.insert(result.id, (*base, offset));
        }
    }

    folds
}

fn build_index_addr_folds(
    func: &Function,
    const_zero_values: &HashSet<ValueId>,
) -> HashMap<ValueId, (ValueId, u32)> {
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();
    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    let mut folds = HashMap::new();

    for block in &func.blocks {
        for inst in &block.insts {
            let InstKind::IndexAddr { base, index } = inst.kind else {
                continue;
            };
            if !const_zero_values.contains(&index) {
                continue;
            }
            let Some(result) = &inst.result else {
                continue;
            };

            let Some(users) = uses.get(&result.id) else {
                continue;
            };
            if users.is_empty() {
                continue;
            }
            let mut ok = true;
            for (use_block, use_idx) in users {
                let use_inst = &func.blocks[*use_block].insts[*use_idx];
                match &use_inst.kind {
                    InstKind::Store { ptr, .. } if ptr == &result.id => {}
                    InstKind::Load { ptr } if ptr == &result.id => {}
                    _ => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok {
                continue;
            }

            // Index 0 folds to the base address with offset 0 when used only for loads/stores.
            folds.insert(result.id, (base, 0));
        }
    }

    folds
}

fn build_const_zero_values(func: &Function) -> HashSet<ValueId> {
    let mut zeros = HashSet::new();
    for block in &func.blocks {
        for inst in &block.insts {
            let Some(result) = &inst.result else {
                continue;
            };
            if let InstKind::Const { value } = &inst.kind {
                if let ConstValue::Int { value, .. } = value
                    && *value == 0
                {
                    zeros.insert(result.id);
                }
                if let ConstValue::Bool(false) = value {
                    zeros.insert(result.id);
                }
            }
        }
    }
    zeros
}

fn build_const_zero_store_only(
    func: &Function,
    value_types: &HashMap<ValueId, IrTypeId>,
    const_zero_values: &HashSet<ValueId>,
    types: &IrTypeCache,
) -> HashSet<ValueId> {
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();
    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    let mut elidable = HashSet::new();
    'value: for value in const_zero_values {
        let Some(ty) = value_types.get(value) else {
            continue;
        };
        match types.kind(*ty) {
            IrTypeKind::Bool | IrTypeKind::Int { .. } => {}
            _ => continue,
        }

        let Some(users) = uses.get(value) else {
            continue;
        };
        if users.is_empty() {
            continue;
        }

        for (block_idx, inst_idx) in users {
            let inst = &func.blocks[*block_idx].insts[*inst_idx];
            match &inst.kind {
                InstKind::Store {
                    value: store_value, ..
                } if store_value == value => {}
                _ => continue 'value,
            }
        }

        elidable.insert(*value);
    }

    elidable
}

fn collect_layouts(
    types: &mut IrTypeCache,
    ty: IrTypeId,
    layouts: &mut HashMap<IrTypeId, IrLayout>,
) {
    if layouts.contains_key(&ty) {
        return;
    }

    let layout = types.layout(ty);
    layouts.insert(ty, layout);

    let kind = types.kind(ty).clone();
    match kind {
        IrTypeKind::Ptr { elem } => {
            collect_layouts(types, elem, layouts);
        }
        IrTypeKind::Array { elem, .. } => {
            collect_layouts(types, elem, layouts);
        }
        IrTypeKind::Tuple { fields } => {
            for field in fields {
                collect_layouts(types, field, layouts);
            }
        }
        IrTypeKind::Struct { fields } => {
            for field in fields {
                collect_layouts(types, field.ty, layouts);
            }
        }
        IrTypeKind::Fn { params, ret } => {
            for param in params {
                collect_layouts(types, param, layouts);
            }
            collect_layouts(types, ret, layouts);
        }
        IrTypeKind::Unit | IrTypeKind::Bool | IrTypeKind::Int { .. } | IrTypeKind::Blob { .. } => {}
    }
}

fn build_local_offsets(
    func: &Function,
    layouts: &HashMap<IrTypeId, IrLayout>,
    spill_size: u32,
) -> (HashMap<LocalId, u32>, u32) {
    let mut offsets = HashMap::new();
    let mut cursor: u32 = spill_size;

    for local in &func.locals {
        let layout = layouts
            .get(&local.ty)
            .unwrap_or_else(|| panic!("backend codegen: missing layout for {:?}", local.ty));
        let align = layout.align().max(1) as u32;
        cursor = align_to(cursor, align);
        cursor = cursor.saturating_add(layout.size() as u32);
        offsets.insert(local.id, cursor);
    }

    let locals_size = align_to(cursor.saturating_sub(spill_size), 8);
    (offsets, locals_size)
}

fn align_to(value: u32, align: u32) -> u32 {
    debug_assert!(align != 0);
    (value + align - 1) & !(align - 1)
}

fn needs_sret(types: &mut IrTypeCache, ty: IrTypeId) -> bool {
    match types.kind(ty) {
        IrTypeKind::Unit | IrTypeKind::Bool | IrTypeKind::Int { .. } | IrTypeKind::Ptr { .. } => {
            false
        }
        _ => types.layout(ty).size() as u32 > 16,
    }
}

fn validate_value_types(
    func: &Function,
    value_types: &HashMap<ValueId, IrTypeId>,
) -> Result<(), String> {
    for block in &func.blocks {
        for inst in &block.insts {
            let mut uses = Vec::new();
            match &inst.kind {
                InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
                InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
                    uses.push(*lhs);
                    uses.push(*rhs);
                }
                InstKind::UnOp { value, .. }
                | InstKind::IntTrunc { value, .. }
                | InstKind::IntExtend { value, .. }
                | InstKind::Cast { value, .. }
                | InstKind::FieldAddr { base: value, .. }
                | InstKind::Load { ptr: value } => {
                    uses.push(*value);
                }
                InstKind::IndexAddr { base, index } => {
                    uses.push(*base);
                    uses.push(*index);
                }
                InstKind::Store { ptr, value } => {
                    uses.push(*ptr);
                    uses.push(*value);
                }
                InstKind::MemCopy { dst, src, len } => {
                    uses.push(*dst);
                    uses.push(*src);
                    uses.push(*len);
                }
                InstKind::MemSet { dst, byte, len } => {
                    uses.push(*dst);
                    uses.push(*byte);
                    uses.push(*len);
                }
                InstKind::Call { callee, args } => {
                    if let Callee::Value(value) = callee {
                        uses.push(*value);
                    }
                    uses.extend(args.iter().copied());
                }
                InstKind::Drop { ptr } => {
                    uses.push(*ptr);
                }
            }

            for value in uses {
                if !value_types.contains_key(&value) {
                    return Err(format!(
                        "missing type for {:?} in block {:?} inst {:?}",
                        value, block.id, inst.kind
                    ));
                }
            }
        }

        let mut term_uses = Vec::new();
        match &block.term {
            Terminator::Br { args, .. } => {
                term_uses.extend(args.iter().copied());
            }
            Terminator::CondBr {
                cond,
                then_args,
                else_args,
                ..
            } => {
                term_uses.push(*cond);
                term_uses.extend(then_args.iter().copied());
                term_uses.extend(else_args.iter().copied());
            }
            Terminator::Switch {
                value,
                cases,
                default_args,
                ..
            } => {
                term_uses.push(*value);
                for case in cases {
                    term_uses.extend(case.args.iter().copied());
                }
                term_uses.extend(default_args.iter().copied());
            }
            Terminator::Return { value } => {
                if let Some(value) = value {
                    term_uses.push(*value);
                }
            }
            Terminator::Unreachable => {}
        }

        for value in term_uses {
            if !value_types.contains_key(&value) {
                return Err(format!(
                    "missing type for {:?} in block {:?} terminator {:?}",
                    value, block.id, block.term
                ));
            }
        }
    }

    Ok(())
}

/// Walks the codegen graph in RPO order and emits instructions.
pub fn emit_graph(graph: &CodegenGraph, func: &Function, sink: &mut dyn CodegenSink) {
    let ctx = EmitContext::new(graph, func);
    let order = rpo(graph);
    let mut visited = HashSet::new();

    for block_id in order {
        if !visited.insert(block_id) {
            continue;
        }

        sink.enter_block(block_id);

        match block_id {
            CodegenBlockId::Ssa(id) => emit_ssa_block(&ctx, id, sink),
            CodegenBlockId::Move(id) => emit_move_block(&ctx, id, sink),
        }
    }
}

fn emit_edge_branch(ctx: &EmitContext<'_>, from: BlockId, to: BlockId, sink: &mut dyn CodegenSink) {
    let moves = ctx.edge_moves(from, to);
    if !moves.is_empty() {
        sink.emit_moves(moves);
    }
    let target = ctx.edge_target(from, to);
    sink.emit_branch(target);
}

fn edge_target_for_cond(ctx: &EmitContext<'_>, from: BlockId, to: BlockId) -> CodegenBlockId {
    let moves = ctx.edge_moves(from, to);
    if !moves.is_empty() {
        panic!(
            "backend codegen: conditional edge {:?} -> {:?} has inline moves",
            from, to
        );
    }
    ctx.edge_target(from, to)
}

fn emit_ssa_block(ctx: &EmitContext<'_>, block: BlockId, sink: &mut dyn CodegenSink) {
    for item in ctx.graph.block_stream(ctx.func, block) {
        match item {
            CodegenEmit::PreMoves(moves) | CodegenEmit::PostMoves(moves) => {
                sink.emit_moves(moves);
            }
            CodegenEmit::Inst(inst) => sink.emit_inst(inst),
        }
    }

    let term = ctx.ssa_block(block).term.clone();
    match &term {
        Terminator::Br { target, .. } => {
            emit_edge_branch(ctx, block, *target, sink);
        }
        Terminator::CondBr {
            cond,
            then_bb,
            else_bb,
            ..
        } => {
            let then_target = edge_target_for_cond(ctx, block, *then_bb);
            let else_target = edge_target_for_cond(ctx, block, *else_bb);
            sink.emit_cond_branch(*cond, then_target, else_target);
        }
        Terminator::Switch {
            value,
            cases,
            default,
            ..
        } => {
            let mut case_labels = Vec::with_capacity(cases.len());
            for case in cases {
                let target = edge_target_for_cond(ctx, block, case.target);
                case_labels.push((case.value.clone(), target));
            }
            let default_target = edge_target_for_cond(ctx, block, *default);
            sink.emit_switch(*value, &case_labels, default_target);
        }
        _ => sink.emit_terminator(&term),
    }
}

fn emit_move_block(ctx: &EmitContext<'_>, block: MoveBlockId, sink: &mut dyn CodegenSink) {
    let moves = ctx.move_block_moves(block);
    sink.emit_moves(moves);

    // Move blocks are single-successor by construction.
    let succs = ctx.graph.succs(CodegenBlockId::Move(block));
    let target = succs
        .first()
        .copied()
        .unwrap_or_else(|| panic!("backend codegen: move block {:?} has no successor", block));
    sink.emit_branch(target);
}

fn rpo(graph: &CodegenGraph) -> Vec<CodegenBlockId> {
    let mut order = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = VecDeque::new();
    stack.push_back(graph.entry());

    while let Some(block) = stack.pop_back() {
        if !visited.insert(block) {
            continue;
        }
        order.push(block);
        for succ in graph.succs(block) {
            stack.push_back(*succ);
        }
    }

    order
}
