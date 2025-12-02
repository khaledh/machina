use std::collections::HashSet;

use crate::ast::BinaryOp;
use crate::dataflow::liveness::LivenessAnalysis;

include!("ir_test_utils.rs");

#[test]
fn gen_kill_simple_block() {
    // Build:
    //
    // entry:
    //   %t0 = binop.add const.u32 1, const.u32 2 // uses consts; defines t0
    //   ret %t0
    let mut b = mk_builder();

    let t0 = b.new_temp(u64_ty());
    b.binary_op(t0, BinaryOp::Add, const_u64(1), const_u64(2));

    b.terminate(IrTerminator::Ret {
        value: Some(temp(0)),
    });
    let func = b.finish();

    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    // For a straight-line block, everything used flows from defs in the same block.
    // Therefore, Gen should be empty at entry, and Kill should contain all temps.
    let entry_id = func.blocks.keys().next().unwrap();
    let live_entry = &live_map[entry_id];

    // At entry, nothing is live-in in this simple example.
    assert!(live_entry.live_in.is_empty());
    // At exit, nothing is live-out because ret uses t0 and then leaves the function.
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
    //   %t2 = binop.add %t0, %t1
    //   ret %t2
    //
    // We expect:
    // - t0 live-out of entry and live-in to then.
    let mut b = mk_builder();

    let t0 = b.new_temp(u64_ty());
    b.move_to(t0, const_u64(1));

    let then_id = b.new_block("then".to_string());

    b.terminate(IrTerminator::Br { target: then_id });

    // then block
    b.select_block(then_id);
    let t1 = b.new_temp(u64_ty());
    b.move_to(t1, const_u64(2));
    let t2 = b.new_temp(u64_ty());
    b.binary_op(t2, BinaryOp::Add, IrOperand::Temp(t0), const_u64(2));
    b.terminate(IrTerminator::Ret {
        value: Some(temp(2)),
    });

    let func = b.finish();
    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let then_id = blocks_iter.next().unwrap();

    let entry_live = &live_map[entry_id];
    let then_live = &live_map[then_id];

    // Entry: no live-in, t0 is live-out.
    assert!(entry_live.live_in.is_empty());
    assert_eq!(entry_live.live_out, HashSet::from([t0]));

    // Then: t0 is live-in, no live-out.
    assert_eq!(then_live.live_in, HashSet::from([t0]));
    assert!(then_live.live_out.is_empty());
}

#[test]
fn liveness_with_phi() {
    // Code:
    //
    // fn test() -> u32 {
    //   var x = 1;
    //   let y = if x < 2 {
    //     x
    //   } else {
    //     x + 1
    //   }
    //   y
    // }

    // IR:
    //
    // entry:
    //   %t0 = move const.u32 1
    //   %t1 = binop.lt %t0, const.u32 2
    //   condbr %t1, then, else
    //
    // then:
    //   // %t0 is live-out of then due to the phi
    //   br join
    //
    // else:
    //   %t2 = binop.add %t0, const.u32 1
    //   br join
    //
    // join:
    //   %t3 = phi [(then, %t0), (else, %t2)]
    //   ret %t3
    //
    // Expectations:
    // - On edge then -> join, t0 is live-out of then.
    // - On edge else -> join, t2 is live-out of else.
    // - t3 is a local def in join; it should not force t0 to be live-in to else, nor t2 to be live-in to then.
    let mut b = mk_builder();

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());
    let join_id = b.new_block("join".to_string());

    // entry block
    let t0 = b.new_temp(u64_ty());
    b.move_to(t0, const_u64(1));
    let t1 = b.new_temp(bool_ty());
    b.binary_op(t1, BinaryOp::Lt, temp(0), const_u64(2));
    b.terminate(IrTerminator::CondBr {
        cond: IrOperand::Temp(t1),
        then_b: then_id,
        else_b: else_id,
    });

    // then block
    b.select_block(then_id);
    b.terminate(IrTerminator::Br { target: join_id });

    // else block
    b.select_block(else_id);
    let t2 = b.new_temp(u64_ty());
    b.binary_op(t2, BinaryOp::Add, temp(0), const_u64(1));
    b.terminate(IrTerminator::Br { target: join_id });

    // join block
    b.select_block(join_id);
    let t3 = b.new_temp(u64_ty());
    b.phi(t3, vec![(then_id, t0), (else_id, t2)]);

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t3)),
    });

    let func = b.finish();
    let analysis = LivenessAnalysis::new(func.clone());
    let live_map = analysis.analyze();

    let entry_id = func.blocks.keys().next().unwrap();

    let entry_live = &live_map[entry_id];
    let then_live = &live_map[&then_id];
    let else_live = &live_map[&else_id];
    let join_live = &live_map[&join_id];

    // In entry, t0 is live-out.
    assert!(entry_live.live_in.is_empty());
    assert_eq!(entry_live.live_out, HashSet::from([t0]));

    // On edge then -> join, t0 is both live-in and live-out of then.
    assert_eq!(then_live.live_in, HashSet::from([t0]));
    assert_eq!(then_live.live_out, HashSet::from([t0]));

    // On edge else -> join, t0 is live-in; t2 is live-out.
    assert_eq!(else_live.live_in, HashSet::from([t0]));
    assert_eq!(else_live.live_out, HashSet::from([t2]));

    // In join, t3 is defined by the phi in this block and then
    // immediately consumed by the ret, so nothing is live-in or
    // live-out.
    assert!(join_live.live_in.is_empty());
    assert!(join_live.live_out.is_empty());
}
