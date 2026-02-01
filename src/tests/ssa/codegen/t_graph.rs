use crate::regalloc::stack::StackSlotId;
use crate::resolve::DefId;
use crate::ssa::codegen::graph::{CodegenBlockId, CodegenEmit, CodegenGraph};
use crate::ssa::codegen::moves::{EdgeMovePlan, MoveSchedule};
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{Callee, FunctionSig, InstKind, Terminator};
use crate::ssa::regalloc::Location;
use crate::ssa::regalloc::moves::{EdgeMove, MoveOp};
use crate::ssa::{IrTypeCache, IrTypeKind};

#[test]
fn test_codegen_graph_inserts_move_block() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let bool_ty = types.add(IrTypeKind::Bool);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "graph_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let cond = builder.const_bool(true, bool_ty);

    builder.set_terminator(
        entry,
        Terminator::CondBr {
            cond,
            then_bb,
            then_args: vec![],
            else_bb,
            else_args: vec![],
        },
    );

    builder.select_block(then_bb);
    builder.terminate(Terminator::Return { value: None });
    builder.select_block(else_bb);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let edge_moves = vec![EdgeMove {
        from: entry,
        to: then_bb,
        moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&edge_moves, &[], &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);

    let graph = CodegenGraph::new(&func, &plan);
    let entry_succs = graph.succs(CodegenBlockId::Ssa(entry));
    assert_eq!(entry_succs.len(), 2);
    assert!(
        entry_succs
            .iter()
            .any(|succ| matches!(succ, CodegenBlockId::Move(_)))
    );
    assert!(
        entry_succs
            .iter()
            .any(|succ| matches!(succ, CodegenBlockId::Ssa(_)))
    );

    let move_block = entry_succs
        .iter()
        .find_map(|succ| match succ {
            CodegenBlockId::Move(id) => Some(*id),
            _ => None,
        })
        .expect("missing move block");
    let move_succs = graph.succs(CodegenBlockId::Move(move_block));
    assert_eq!(move_succs, &[CodegenBlockId::Ssa(then_bb)]);
}

#[test]
fn test_codegen_graph_exposes_call_moves() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "graph_call_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let arg = builder.const_int(1, false, 64, u64_ty);
    let _call = builder.call(Callee::Direct(DefId(1)), vec![arg], u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let edge_moves = Vec::new();
    let call_moves = vec![crate::ssa::regalloc::moves::CallMove {
        block: func.blocks[0].id,
        inst_index: 1,
        pre_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
        post_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(1)),
            dst: Location::Stack(StackSlotId(2)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&edge_moves, &call_moves, &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);

    let call_index = func.blocks[0]
        .insts
        .iter()
        .position(|inst| matches!(inst.kind, InstKind::Call { .. }))
        .expect("missing call inst");
    let (pre, post) = graph
        .call_moves(func.blocks[0].id, call_index)
        .expect("missing call moves");

    assert_eq!(pre.len(), 1);
    assert_eq!(post.len(), 1);
}

#[test]
fn test_codegen_block_stream_orders_call_moves() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "stream_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let arg = builder.const_int(1, false, 64, u64_ty);
    let _call = builder.call(Callee::Direct(DefId(1)), vec![arg], u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let edge_moves = Vec::new();
    let call_moves = vec![crate::ssa::regalloc::moves::CallMove {
        block: func.blocks[0].id,
        inst_index: 1,
        pre_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
        post_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(1)),
            dst: Location::Stack(StackSlotId(2)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&edge_moves, &call_moves, &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);

    let mut stream = graph.block_stream(&func, func.blocks[0].id);
    let mut emits = Vec::new();
    while let Some(item) = stream.next() {
        emits.push(item);
    }

    assert!(matches!(emits[0], CodegenEmit::Inst(_)));
    assert!(matches!(emits[1], CodegenEmit::PreMoves(_)));
    assert!(matches!(emits[2], CodegenEmit::Inst(_)));
    assert!(matches!(emits[3], CodegenEmit::PostMoves(_)));
}
