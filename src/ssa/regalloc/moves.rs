//! Move planning for SSA register allocation.
//!
//! This module collects the concrete moves required by control-flow edges
//! (block parameters) and by the call ABI, then orders them so parallel
//! moves don't clobber values that are still needed.
//!
//! # Overview
//!
//! SSA form uses block parameters to pass values between basic blocks. When
//! lowering to machine code, these become parallel moves that must be
//! sequenced carefully to avoid overwriting sources before they're read.
//!
//! Similarly, function calls require moving arguments into ABI-specified
//! locations (registers or stack slots) before the call, and moving the
//! result out of the return register afterward.
//!
//! # Parallel Move Resolution
//!
//! Given a set of parallel moves like `{r1 <- r2, r2 <- r1}`, we must break
//! cycles using a scratch register. The algorithm:
//!
//! 1. Emit moves whose destinations are not used as sources by other moves
//! 2. When all remaining moves form cycles, break one cycle by saving a
//!    source to the scratch register, then continue
//!
//! Some move pairs (e.g., stack-to-stack) implicitly use the scratch register
//! as an intermediary, which must be accounted for in the ordering.

use std::collections::{HashMap, HashSet};

use crate::regalloc::target::TargetSpec;
use crate::ssa::IrTypeCache;
use crate::ssa::model::ir::{BlockId, Callee, Function, InstKind, Terminator, ValueId};

use super::{Location, ValueAllocMap};

// ============================================================================
// Data Structures
// ============================================================================

/// A single move operation between two allocated locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveOp {
    pub src: Location,
    pub dst: Location,
    /// Size in bytes of the value being moved.
    pub size: u32,
}

/// Copies parameter data from an incoming pointer into a local stack slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParamCopy {
    pub src: Location,
    pub dst: crate::ssa::regalloc::StackSlotId,
    pub size: u32,
}

/// Moves required on a control-flow edge between two blocks.
///
/// These moves transfer arguments passed at a branch/jump to the
/// corresponding block parameters of the target block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeMove {
    pub from: BlockId,
    pub to: BlockId,
    pub moves: Vec<MoveOp>,
}

/// Moves required around a call instruction.
///
/// - `pre_moves`: Move arguments into ABI locations before the call
/// - `post_moves`: Move the return value from the ABI register afterward
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallMove {
    pub block: BlockId,
    pub inst_index: usize,
    pub pre_moves: Vec<MoveOp>,
    pub post_moves: Vec<MoveOp>,
}

/// Complete move plan for a function.
///
/// Contains all edge moves (for block parameters) and call moves
/// (for ABI compliance) that the code generator must emit.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MovePlan {
    pub entry_moves: Vec<MoveOp>,
    pub param_copies: Vec<ParamCopy>,
    pub edge_moves: Vec<EdgeMove>,
    pub call_moves: Vec<CallMove>,
}

impl MovePlan {
    /// Resolves all parallel move lists, ordering them to avoid clobbers.
    ///
    /// Uses the first scratch register to break any register cycles that
    /// would otherwise cause a move to overwrite a value still needed as
    /// a source.
    pub fn resolve_parallel_moves(&mut self, scratch_regs: &[crate::regalloc::target::PhysReg]) {
        if scratch_regs.is_empty() {
            return;
        }
        let scratch = scratch_regs[0];

        resolve_move_list(&mut self.entry_moves, scratch);

        for edge in &mut self.edge_moves {
            resolve_move_list(&mut edge.moves, scratch);
        }

        for call in &mut self.call_moves {
            resolve_move_list(&mut call.pre_moves, scratch);
            resolve_move_list(&mut call.post_moves, scratch);
        }
    }
}

// ============================================================================
// Move Plan Construction
// ============================================================================

/// Builds a complete move plan for a function.
///
/// Scans the function for:
/// - Entry block parameters requiring moves from ABI locations
/// - Block edges with arguments (branches, conditional branches, switches)
/// - Call instructions requiring ABI-compliant argument/result moves
///
/// Returns a `MovePlan` containing all required moves, which can then be
/// resolved with `resolve_parallel_moves` before code generation.
pub fn build_move_plan(
    func: &Function,
    alloc_map: &ValueAllocMap,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
    param_reg_count: usize,
) -> MovePlan {
    let mut plan = MovePlan::default();
    let value_types = build_value_types(func);

    // Build a map of block ID -> parameter value IDs for edge move generation
    let mut block_params: HashMap<BlockId, Vec<ValueId>> = HashMap::new();
    for block in &func.blocks {
        let params = block.params.iter().map(|param| param.value.id).collect();
        block_params.insert(block.id, params);
    }

    // Plan entry moves for function parameters
    if let Some(entry) = func.blocks.first() {
        plan_entry_moves(entry, alloc_map, types, target, param_reg_count, &mut plan);
    }

    for block in &func.blocks {
        // Collect call moves for each call instruction
        for (inst_index, inst) in block.insts.iter().enumerate() {
            if let InstKind::Call { callee, args } = &inst.kind {
                if let Some(call_move) = plan_call_moves(
                    block.id,
                    inst_index,
                    callee,
                    args,
                    inst.result.as_ref(),
                    alloc_map,
                    &value_types,
                    types,
                    target,
                    param_reg_count,
                ) {
                    plan.call_moves.push(call_move);
                }
            }
        }

        // Collect edge moves for each control-flow edge with arguments
        plan_terminator_edge_moves(
            block,
            &block_params,
            alloc_map,
            &value_types,
            types,
            &mut plan,
        );
    }

    plan
}

/// Plans moves for entry block parameters.
///
/// Entry parameters arrive in ABI-specified locations (registers or incoming
/// stack slots) and must be moved to their allocated locations. Register-typed
/// params become entry moves; aggregate params become memory copies.
fn plan_entry_moves(
    entry: &crate::ssa::model::ir::Block,
    alloc_map: &ValueAllocMap,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
    param_reg_count: usize,
    plan: &mut MovePlan,
) {
    let mut assigner = AbiAssigner::new(target, param_reg_count, ArgStackKind::Incoming);

    for param in entry.params.iter() {
        let param_id = param.value.id;
        let ty = param.value.ty;
        let alloc = alloc_map
            .get(&param_id)
            .copied()
            .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", param_id));

        let pass = arg_pass_info(types, ty);
        let src_locs = assigner.next_locs(pass);

        match pass.kind {
            PassKind::Reg => {
                let src = src_locs
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("ssa regalloc: missing ABI src for param"));
                if src != alloc {
                    let size = move_size_for(types, ty, src, alloc);
                    plan.entry_moves.push(MoveOp {
                        src,
                        dst: alloc,
                        size,
                    });
                }
            }
            PassKind::ByValueAgg => {
                let Location::Stack(slot) = alloc else {
                    panic!(
                        "ssa regalloc: aggregate param must be stack-backed, got {:?}",
                        alloc
                    );
                };
                for_each_agg_chunk(pass.size, |offset, size, idx| {
                    let Some(src) = src_locs.get(idx) else { return };
                    let dst = Location::StackOffset(slot, offset);
                    if *src != dst {
                        plan.entry_moves.push(MoveOp {
                            src: *src,
                            dst,
                            size,
                        });
                    }
                });
            }
            PassKind::ByRef => {
                // Aggregate params passed by reference are copied into their stack slot at entry.
                let Location::Stack(slot) = alloc else {
                    panic!(
                        "ssa regalloc: aggregate param must be stack-backed, got {:?}",
                        alloc
                    );
                };
                if pass.size > 0 {
                    let src = src_locs
                        .first()
                        .copied()
                        .unwrap_or_else(|| panic!("ssa regalloc: missing ABI src for param"));
                    plan.param_copies.push(ParamCopy {
                        src,
                        dst: slot,
                        size: pass.size,
                    });
                }
            }
        }
    }
}

/// Plans edge moves for a block's terminator.
///
/// Examines the terminator and generates edge moves for each outgoing edge
/// that passes arguments to the target block's parameters.
fn plan_terminator_edge_moves(
    block: &crate::ssa::model::ir::Block,
    block_params: &HashMap<BlockId, Vec<ValueId>>,
    alloc_map: &ValueAllocMap,
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    types: &mut IrTypeCache,
    plan: &mut MovePlan,
) {
    // Helper closure to generate and push edge moves
    let mut push_edge = |from: BlockId, to: BlockId, args: &[ValueId]| {
        let moves = edge_moves_for(from, to, args, block_params, alloc_map, value_types, types);
        if !moves.is_empty() {
            plan.edge_moves.push(EdgeMove { from, to, moves });
        }
    };

    match &block.term {
        Terminator::Br { target, args } => {
            push_edge(block.id, *target, args);
        }
        Terminator::CondBr {
            then_bb,
            then_args,
            else_bb,
            else_args,
            ..
        } => {
            push_edge(block.id, *then_bb, then_args);
            push_edge(block.id, *else_bb, else_args);
        }
        Terminator::Switch {
            cases,
            default,
            default_args,
            ..
        } => {
            for case in cases {
                push_edge(block.id, case.target, &case.args);
            }
            push_edge(block.id, *default, default_args);
        }
        Terminator::Return { .. } | Terminator::Unreachable => {}
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Builds a map from value IDs to their types for the entire function.
fn build_value_types(func: &Function) -> HashMap<ValueId, crate::ssa::IrTypeId> {
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
    map
}

/// Returns true if the type requires struct-return (sret) ABI.
///
/// Types that don't fit in a register (aggregates, arrays) must be returned
/// via a pointer passed by the caller.
fn needs_sret(types: &mut IrTypeCache, ty: crate::ssa::IrTypeId) -> bool {
    if types.is_reg_type(ty) {
        return false;
    }

    // Allow small aggregates to return in registers (AAPCS-like behavior).
    let size = types.layout(ty).size() as u32;
    size > 16
}

// ============================================================================
// Edge Move Generation
// ============================================================================

/// Generates moves for a control-flow edge from `from` to `to`.
///
/// Maps each argument at the branch site to the corresponding block parameter
/// at the target, creating a move if their allocated locations differ.
fn edge_moves_for(
    from: BlockId,
    to: BlockId,
    args: &[ValueId],
    block_params: &HashMap<BlockId, Vec<ValueId>>,
    alloc_map: &ValueAllocMap,
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    types: &mut IrTypeCache,
) -> Vec<MoveOp> {
    let params = block_params.get(&to).unwrap_or_else(|| {
        panic!(
            "ssa regalloc: missing params for target block {:?} from {:?}",
            to, from
        )
    });

    if params.len() != args.len() {
        panic!(
            "ssa regalloc: block {:?} expects {} args, got {}",
            to,
            params.len(),
            args.len()
        );
    }

    let mut moves = Vec::new();
    for (arg, param) in args.iter().zip(params.iter()) {
        let src = alloc_map
            .get(arg)
            .copied()
            .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", arg));
        let dst = alloc_map
            .get(param)
            .copied()
            .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", param));
        if src != dst {
            let arg_ty = value_types
                .get(arg)
                .copied()
                .or_else(|| value_types.get(param).copied())
                .unwrap_or_else(|| {
                    panic!(
                        "ssa regalloc: missing type for block arg {:?} (param {:?})",
                        arg, param
                    )
                });
            let size = move_size_for(types, arg_ty, src, dst);
            moves.push(MoveOp { src, dst, size });
        }
    }

    moves
}

// ============================================================================
// Parallel Move Resolution
// ============================================================================

/// Orders a parallel move list to avoid destination-before-source conflicts.
///
/// The algorithm repeatedly selects moves whose destinations are not needed
/// as sources by other pending moves. When no such move exists, all remaining
/// moves form cycles, which are broken by saving one source to the scratch
/// register.
///
/// Moves that implicitly use the scratch register (e.g., stack-to-stack) are
/// treated as also writing to the scratch register for dependency tracking.
fn resolve_move_list(moves: &mut Vec<MoveOp>, scratch: crate::regalloc::target::PhysReg) {
    if moves.len() <= 1 {
        return;
    }

    // Take ownership of moves; we'll rebuild the list in the correct order
    let mut pending = std::mem::take(moves);

    // Track which registers each pending move reads from
    let mut pending_srcs: Vec<HashSet<crate::regalloc::target::PhysReg>> = pending
        .iter()
        .map(|mov| {
            let mut regs = HashSet::new();
            if let Location::Reg(reg) = mov.src {
                regs.insert(reg);
            }
            regs
        })
        .collect();

    // Count how many pending moves read from each register
    let mut src_counts: HashMap<crate::regalloc::target::PhysReg, usize> = HashMap::new();
    for regs in &pending_srcs {
        for reg in regs {
            *src_counts.entry(*reg).or_insert(0) += 1;
        }
    }

    let mut ordered = Vec::with_capacity(pending.len());

    while !pending.is_empty() {
        // Phase 1: Find a move whose destination isn't needed by other moves
        let mut ready_idx = None;
        for (idx, mov) in pending.iter().enumerate() {
            // Collect all registers this move would clobber
            let mut dst_regs = Vec::new();
            if let Location::Reg(dst_reg) = mov.dst {
                dst_regs.push(dst_reg);
            }
            if move_uses_scratch(mov) {
                dst_regs.push(scratch);
            }

            // Moves to non-register locations are always ready
            if dst_regs.is_empty() {
                ready_idx = Some(idx);
                break;
            }

            // Check if any other move still needs our destination as a source
            let mut ready = true;
            for reg in dst_regs {
                let total = src_counts.get(&reg).copied().unwrap_or(0);
                // Exclude self-references (e.g., r1 <- r1 + r2 wouldn't block itself)
                let self_uses = if pending_srcs[idx].contains(&reg) {
                    1
                } else {
                    0
                };
                if total > self_uses {
                    ready = false;
                    break;
                }
            }

            if ready {
                ready_idx = Some(idx);
                break;
            }
        }

        // If we found a ready move, emit it and update bookkeeping
        if let Some(idx) = ready_idx {
            let removed = pending.remove(idx);
            let removed_srcs = pending_srcs.remove(idx);
            for reg in removed_srcs {
                if let Some(count) = src_counts.get_mut(&reg) {
                    *count -= 1;
                    if *count == 0 {
                        src_counts.remove(&reg);
                    }
                }
            }
            ordered.push(removed);
            continue;
        }

        // Phase 2: No ready moves - all remaining moves form cycles.
        // Break a cycle by saving one source to the scratch register.
        let cycle_idx = pending
            .iter()
            .position(|mov| matches!((&mov.src, &mov.dst), (Location::Reg(_), Location::Reg(_))))
            .expect("cycle resolution requires reg-to-reg move");
        // Remove the cycle participant and update bookkeeping
        let mut mov = pending.remove(cycle_idx);
        let removed_srcs = pending_srcs.remove(cycle_idx);
        for reg in removed_srcs {
            if let Some(count) = src_counts.get_mut(&reg) {
                *count -= 1;
                if *count == 0 {
                    src_counts.remove(&reg);
                }
            }
        }

        let Location::Reg(src_reg) = mov.src else {
            unreachable!("cycle candidate must be reg -> reg");
        };

        // Save the source to scratch: src_reg -> scratch
        ordered.push(MoveOp {
            src: Location::Reg(src_reg),
            dst: Location::Reg(scratch),
            size: mov.size,
        });

        // Rewrite the move to read from scratch instead, and re-add to pending
        mov.src = Location::Reg(scratch);
        let mut regs = HashSet::new();
        if let Location::Reg(reg) = mov.src {
            regs.insert(reg);
        }
        for reg in &regs {
            *src_counts.entry(*reg).or_insert(0) += 1;
        }
        pending_srcs.push(regs);
        pending.push(mov);
    }

    *moves = ordered;
}

/// Returns true if emitting this move requires the scratch register.
///
/// Memory-to-memory moves cannot be done directly on most architectures,
/// so they use the scratch register as an intermediary. This must be
/// accounted for when ordering moves to avoid clobbering the scratch.
fn move_uses_scratch(mov: &MoveOp) -> bool {
    matches!(
        (mov.src, mov.dst),
        // Stack-to-stack and stack-to-arg moves need scratch as intermediary
        (Location::Stack(_), Location::Stack(_))
            | (Location::Stack(_), Location::IncomingArg(_))
            | (Location::Stack(_), Location::OutgoingArg(_))
            | (Location::IncomingArg(_), Location::Stack(_))
            | (Location::OutgoingArg(_), Location::Stack(_))
            | (Location::StackOffset(_, _), Location::Stack(_))
            | (Location::Stack(_), Location::StackOffset(_, _))
            | (Location::StackOffset(_, _), Location::StackOffset(_, _))
            | (Location::StackOffset(_, _), Location::IncomingArg(_))
            | (Location::StackOffset(_, _), Location::OutgoingArg(_))
            | (Location::IncomingArg(_), Location::StackOffset(_, _))
            | (Location::OutgoingArg(_), Location::StackOffset(_, _))
            | (Location::IncomingArg(_), Location::OutgoingArg(_))
            | (Location::OutgoingArg(_), Location::IncomingArg(_))
            // Loading a stack address into memory also needs scratch
            | (Location::StackAddr(_), Location::Stack(_))
            | (Location::StackAddr(_), Location::StackOffset(_, _))
            | (Location::StackAddr(_), Location::IncomingArg(_))
            | (Location::StackAddr(_), Location::OutgoingArg(_))
    )
}

/// Determines the byte size for a move operation.
///
/// Stack addresses are always pointer-sized (8 bytes). For other moves,
/// uses the type's layout size.
fn move_size_for(
    types: &mut IrTypeCache,
    ty: crate::ssa::IrTypeId,
    src: Location,
    dst: Location,
) -> u32 {
    if matches!(src, Location::StackAddr(_)) || matches!(dst, Location::StackAddr(_)) {
        return 8; // Pointer size
    }
    types.layout(ty).size() as u32
}

// ============================================================================
// Call Move Generation
// ============================================================================

#[derive(Debug, Clone, Copy)]
pub(crate) enum ArgStackKind {
    Incoming,
    Outgoing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PassKind {
    Reg,
    ByValueAgg,
    ByRef,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ArgPassInfo {
    pub(crate) kind: PassKind,
    size: u32,
}

pub(crate) fn arg_pass_info(types: &mut IrTypeCache, ty: crate::ssa::IrTypeId) -> ArgPassInfo {
    let size = types.layout(ty).size() as u32;
    if types.is_reg_type(ty) {
        return ArgPassInfo {
            kind: PassKind::Reg,
            size,
        };
    }

    ArgPassInfo {
        kind: if size <= 16 {
            PassKind::ByValueAgg
        } else {
            PassKind::ByRef
        },
        size,
    }
}

fn stack_arg_location(kind: ArgStackKind, offset: u32) -> Location {
    match kind {
        ArgStackKind::Incoming => Location::IncomingArg(offset),
        ArgStackKind::Outgoing => Location::OutgoingArg(offset),
    }
}

pub(crate) fn abi_arg_locations(
    pass: ArgPassInfo,
    param_reg_count: usize,
    target: &dyn TargetSpec,
    next_reg: &mut usize,
    next_stack: &mut u32,
    stack_kind: ArgStackKind,
) -> Vec<Location> {
    if pass.kind != PassKind::ByValueAgg || pass.size <= 8 {
        if *next_reg < param_reg_count {
            let reg = target
                .param_reg(*next_reg as u32)
                .unwrap_or_else(|| panic!("ssa regalloc: arg {} has no ABI reg", *next_reg));
            *next_reg += 1;
            return vec![Location::Reg(reg)];
        }
        let offset = *next_stack;
        *next_stack = next_stack.saturating_add(8);
        return vec![stack_arg_location(stack_kind, offset)];
    }

    if *next_reg + 1 < param_reg_count {
        let reg0 = target
            .param_reg(*next_reg as u32)
            .unwrap_or_else(|| panic!("ssa regalloc: arg {} has no ABI reg", *next_reg));
        let reg1 = target
            .param_reg((*next_reg + 1) as u32)
            .unwrap_or_else(|| panic!("ssa regalloc: arg {} has no ABI reg", *next_reg + 1));
        *next_reg += 2;
        return vec![Location::Reg(reg0), Location::Reg(reg1)];
    }

    let offset = *next_stack;
    *next_stack = next_stack.saturating_add(16);
    vec![
        stack_arg_location(stack_kind, offset),
        stack_arg_location(stack_kind, offset.saturating_add(8)),
    ]
}

struct AbiAssigner<'a> {
    target: &'a dyn TargetSpec,
    param_reg_count: usize,
    next_reg: usize,
    next_stack: u32,
    stack_kind: ArgStackKind,
}

impl<'a> AbiAssigner<'a> {
    fn new(target: &'a dyn TargetSpec, param_reg_count: usize, stack_kind: ArgStackKind) -> Self {
        Self {
            target,
            param_reg_count,
            next_reg: 0,
            next_stack: 0,
            stack_kind,
        }
    }

    fn next_locs(&mut self, pass: ArgPassInfo) -> Vec<Location> {
        abi_arg_locations(
            pass,
            self.param_reg_count,
            self.target,
            &mut self.next_reg,
            &mut self.next_stack,
            self.stack_kind,
        )
    }

    fn stack_bytes(&self) -> u32 {
        self.next_stack
    }
}

/// Looks up the type of a call argument.
fn call_arg_type(
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    arg: &ValueId,
) -> crate::ssa::IrTypeId {
    value_types
        .get(arg)
        .copied()
        .unwrap_or_else(|| panic!("ssa regalloc: missing type for arg {:?}", arg))
}

/// Returns the source location for a call argument.
fn call_arg_src(arg: ValueId, alloc_map: &ValueAllocMap) -> Location {
    alloc_map
        .get(&arg)
        .copied()
        .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", arg))
}

pub(crate) fn outgoing_stack_bytes_for_call(
    args: &[ValueId],
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
    param_reg_count: usize,
) -> u32 {
    let mut assigner = AbiAssigner::new(target, param_reg_count, ArgStackKind::Outgoing);
    for arg in args {
        let arg_ty = call_arg_type(value_types, arg);
        let pass = arg_pass_info(types, arg_ty);
        let _ = assigner.next_locs(pass);
    }
    assigner.stack_bytes()
}

/// Plans all moves for a single call instruction.
///
/// Generates:
/// - Pre-moves for indirect callee (if applicable)
/// - Pre-moves for sret pointer (if returning aggregate)
/// - Pre-moves for all arguments to their ABI locations
/// - Post-moves for the return value (if any)
fn plan_call_moves(
    block: BlockId,
    inst_index: usize,
    callee: &Callee,
    args: &[ValueId],
    result: Option<&crate::ssa::model::ir::ValueDef>,
    alloc_map: &ValueAllocMap,
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
    param_reg_count: usize,
) -> Option<CallMove> {
    let mut pre_moves = Vec::new();
    let mut post_moves = Vec::new();

    // For indirect calls, move the callee address to the indirect call register
    if let Callee::Value(value) = callee {
        let src = call_arg_src(*value, alloc_map);
        let callee_ty = call_arg_type(value_types, value);
        let dst = Location::Reg(target.indirect_call_reg());
        if src != dst {
            let size = move_size_for(types, callee_ty, src, dst);
            pre_moves.push(MoveOp { src, dst, size });
        }
    }

    // For aggregate returns (sret), pass the result address in the indirect result register
    if let Some(result) = result {
        if needs_sret(types, result.ty) {
            let reg = target
                .indirect_result_reg()
                .unwrap_or_else(|| panic!("ssa regalloc: call sret requires indirect result reg"));
            let loc = alloc_map
                .get(&result.id)
                .copied()
                .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", result.id));
            let src = match loc {
                Location::Stack(slot) => Location::StackAddr(slot),
                _ => {
                    panic!(
                        "ssa regalloc: sret result must be stack-backed, got {:?}",
                        loc
                    );
                }
            };
            let size = move_size_for(types, result.ty, src, Location::Reg(reg));
            pre_moves.push(MoveOp {
                src,
                dst: Location::Reg(reg),
                size,
            });
        }
    }

    let mut assigner = AbiAssigner::new(target, param_reg_count, ArgStackKind::Outgoing);

    // Move each argument to its ABI-specified location (register or stack slot)
    for arg in args {
        let arg_ty = call_arg_type(value_types, arg);
        let pass = arg_pass_info(types, arg_ty);
        let dst_locs = assigner.next_locs(pass);
        match pass.kind {
            PassKind::Reg => {
                let src = call_arg_src(*arg, alloc_map);
                let dst = dst_locs
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("ssa regalloc: missing ABI dst for arg"));
                if src != dst {
                    let size = move_size_for(types, arg_ty, src, dst);
                    pre_moves.push(MoveOp { src, dst, size });
                }
            }
            PassKind::ByRef => {
                let src = match call_arg_src(*arg, alloc_map) {
                    Location::Stack(slot) => Location::StackAddr(slot),
                    Location::StackAddr(slot) => Location::StackAddr(slot),
                    other => {
                        panic!(
                            "ssa regalloc: aggregate arg must be stack-backed, got {:?}",
                            other
                        );
                    }
                };
                let dst = dst_locs
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("ssa regalloc: missing ABI dst for arg"));
                if src != dst {
                    let size = move_size_for(types, arg_ty, src, dst);
                    pre_moves.push(MoveOp { src, dst, size });
                }
            }
            PassKind::ByValueAgg => {
                let src_slot = match call_arg_src(*arg, alloc_map) {
                    Location::Stack(slot) => slot,
                    other => {
                        panic!(
                            "ssa regalloc: by-value aggregate arg must be stack-backed, got {:?}",
                            other
                        );
                    }
                };
                for_each_agg_chunk(pass.size, |offset, size, idx| {
                    let Some(dst) = dst_locs.get(idx) else { return };
                    let src = Location::StackOffset(src_slot, offset);
                    pre_moves.push(MoveOp {
                        src,
                        dst: *dst,
                        size,
                    });
                });
            }
        }
    }

    // For register-sized returns, move the result from the return register
    if let Some(result) = result {
        if !needs_sret(types, result.ty) {
            let dst_loc = alloc_map
                .get(&result.id)
                .copied()
                .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", result.id));
            let pass = arg_pass_info(types, result.ty);
            if pass.kind == PassKind::Reg {
                let src = Location::Reg(target.result_reg());
                if src != dst_loc {
                    let size = move_size_for(types, result.ty, src, dst_loc);
                    post_moves.push(MoveOp {
                        src,
                        dst: dst_loc,
                        size,
                    });
                }
            } else {
                let Location::Stack(slot) = dst_loc else {
                    panic!(
                        "ssa regalloc: aggregate return must be stack-backed, got {:?}",
                        dst_loc
                    );
                };
                for_each_agg_chunk(pass.size, |offset, size, idx| {
                    let src = if idx == 0 {
                        Location::Reg(target.result_reg())
                    } else {
                        let reg1 = target.param_reg(1).unwrap_or_else(|| {
                            panic!("ssa regalloc: aggregate return requires second result reg")
                        });
                        Location::Reg(reg1)
                    };
                    post_moves.push(MoveOp {
                        src,
                        dst: Location::StackOffset(slot, offset),
                        size,
                    });
                });
            }
        }
    }

    if pre_moves.is_empty() && post_moves.is_empty() {
        None
    } else {
        Some(CallMove {
            block,
            inst_index,
            pre_moves,
            post_moves,
        })
    }
}

fn for_each_agg_chunk(mut size: u32, mut f: impl FnMut(u32, u32, usize)) {
    if size == 0 {
        return;
    }
    let size0 = size.min(8);
    if size0 > 0 {
        f(0, size0, 0);
    }
    if size > 8 {
        size = size.saturating_sub(8);
        let size1 = size.min(8);
        if size1 > 0 {
            f(8, size1, 1);
        }
    }
}
