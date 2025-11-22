use crate::ast::BinaryOp;
use crate::dataflow::liveness::LivenessAnalysis;
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrBlockId, IrTerminator};
use crate::types::Type;

// Helper to build a minimal function with given name / signature
fn mk_builder() -> IrFunctionBuilder {
    IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit)
}

#[test]
fn gen_kill_simple_block() {
    // Build:
    //
    // entry:
    //   %t0 = const.u32 1      // defines t0
    //   %t1 = const.u32 2      // defines t1
    //   %t2 = binop.add t0, t1 // uses t0, t1; defines t2
    //   ret %t2
    let mut b = mk_builder();

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    let t2 = b.new_temp(Type::UInt32);
    b.binary_op(t2, BinaryOp::Add, t0, t1);

    b.terminate(IrTerminator::Ret { value: Some(t2) });
    let func = b.finish();

    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    // For a straight-line block, everything used flows from defs in the same block.
    // Therefore Gen should be empty at entry, and Kill should contain all temps.
    let entry_id = func.blocks.keys().next().unwrap();
    let live_entry = &live_map[entry_id];

    // At entry, nothing is live-in in this simple example.
    assert!(live_entry.live_in.is_empty());
    // At exit, nothing is live-out because ret uses t2 and then leaves the function.
    assert!(live_entry.live_out.is_empty());
}

#[test]
fn liveness_across_branch() {
    // Build:
    //
    // entry:
    //   %t0 = const.u32 1
    //   br then
    //
    // then:
    //   %t1 = const.u32 2
    //   %t2 = binop.add t0, t1
    //   ret %t2
    //
    // We expect:
    // - t0 live-out of entry and live-in to then.
    let mut b = mk_builder();

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let then_id = b.new_block("then".to_string());

    b.terminate(IrTerminator::Br { target: then_id });

    // then block
    b.select_block(then_id);
    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);
    let t2 = b.new_temp(Type::UInt32);
    b.binary_op(t2, BinaryOp::Add, t0, t1);
    b.terminate(IrTerminator::Ret { value: Some(t2) });

    let func = b.finish();
    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let then_id = blocks_iter.next().unwrap();

    let entry_live = &live_map[entry_id];
    let then_live = &live_map[then_id];

    // t0 is needed in then, so it must be live-out of entry and live-in to then.
    assert!(entry_live.live_out.contains(&t0));
    assert!(then_live.live_in.contains(&t0));

    // t1 is only used inside then; it should not be live-out of entry.
    assert!(!entry_live.live_out.contains(&t1));
}

#[test]
fn liveness_with_phi() {
    // Build:
    //
    // entry:
    //   %t0 = const.u32 1
    //   %t1 = const.u32 2
    //   %t2 = binop.lt t0, t1
    //   condbr %t2, then, join
    //
    // then:
    //   %t3 = const.u32 3
    //   br join
    //
    // join:
    //   %t4 = phi [(entry, t0), (then, t3)]
    //   %t5 = const.u32 5
    //   %t6 = binop.add t4, t5
    //   ret %t6
    //
    // Expectations:
    // - On edge entry -> join, t0 is live-out of entry.
    // - On edge then -> join, t3 is live-out of then.
    // - t4 is a local def in join; it should not force t0/t3 to be live in both preds.
    let mut b = mk_builder();

    let then_id = b.new_block("then".to_string());
    let join_id = b.new_block("join".to_string());

    // entry block
    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);
    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);
    let t2 = b.new_temp(Type::Bool);
    b.binary_op(t2, BinaryOp::Lt, t0, t1);
    b.terminate(IrTerminator::CondBr {
        cond: t2,
        then_b: then_id,
        else_b: join_id,
    });

    // then block
    b.select_block(then_id);
    let t3 = b.new_temp(Type::UInt32);
    b.const_u32(t3, 3);
    b.terminate(IrTerminator::Br { target: join_id });

    // join block
    b.select_block(join_id);
    let t4 = b.new_temp(Type::UInt32);
    b.phi(t4, vec![(IrBlockId(0), t0), (then_id, t3)]);

    let t5 = b.new_temp(Type::UInt32);
    b.const_u32(t5, 5);

    let t6 = b.new_temp(Type::UInt32);
    b.binary_op(t6, BinaryOp::Add, t4, t5);
    b.terminate(IrTerminator::Ret { value: Some(t6) });

    let func = b.finish();
    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    // blocks are in termination order: entry, then, join
    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let then_id = blocks_iter.next().unwrap();
    let join_id = blocks_iter.next().unwrap();

    let entry_live = &live_map[entry_id];
    let then_live = &live_map[then_id];
    let join_live = &live_map[join_id];

    // On edge entry -> join, the phi uses t0, so t0 must be live-out of entry.
    assert!(entry_live.live_out.contains(&t0));
    // t3 should not be forced live-out of entry just because it is another phi operand.
    assert!(!entry_live.live_out.contains(&t3));

    // On edge then -> join, the phi uses t3, so t3 must be live-out of then.
    assert!(then_live.live_out.contains(&t3));
    // t0 should not be forced live-out of then.
    assert!(!then_live.live_out.contains(&t0));

    // In join, t4 is defined by the phi; its operands are not required to be live-in.
    assert!(!join_live.live_in.contains(&t0));
    assert!(!join_live.live_in.contains(&t3));
}
