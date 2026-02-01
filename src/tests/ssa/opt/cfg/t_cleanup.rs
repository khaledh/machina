use crate::resolve::DefId;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::format::format_func;
use crate::ssa::model::ir::{ConstValue, FunctionSig, SwitchCase, Terminator};
use crate::ssa::{IrTypeCache, IrTypeKind};
use indoc::indoc;

#[test]
fn test_cleanup_removes_unreachable_block() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let live_bb = builder.add_block();
    let dead_bb = builder.add_block();

    builder.terminate(Terminator::Br {
        target: live_bb,
        args: Vec::new(),
    });

    builder.select_block(live_bb);
    let value = builder.const_int(1, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    builder.select_block(dead_bb);
    let value = builder.const_int(2, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let mut func = builder.finish();
    super::run_cleanup(&mut func);

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            ret %v0
        }
    "};
    super::assert_ir_eq(text, expected);
}

#[test]
fn test_cleanup_prunes_empty_block() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let empty_bb = builder.add_block();
    let target_bb = builder.add_block();

    builder.terminate(Terminator::Br {
        target: empty_bb,
        args: Vec::new(),
    });

    builder.set_terminator(
        empty_bb,
        Terminator::Br {
            target: target_bb,
            args: Vec::new(),
        },
    );

    builder.select_block(target_bb);
    let value = builder.const_int(3, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let mut func = builder.finish();
    super::run_cleanup(&mut func);

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 3:u64
            ret %v0
        }
    "};
    super::assert_ir_eq(text, expected);
}

#[test]
fn test_cleanup_merges_single_pred_block() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let next_bb = builder.add_block();

    builder.terminate(Terminator::Br {
        target: next_bb,
        args: Vec::new(),
    });

    builder.select_block(next_bb);
    let value = builder.const_int(7, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let mut func = builder.finish();
    super::run_cleanup(&mut func);

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 7:u64
            ret %v0
        }
    "};
    super::assert_ir_eq(text, expected);
}

#[test]
fn test_cleanup_collapses_switch_targets() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let target_bb = builder.add_block();

    let cond = builder.const_int(0, false, 64, u64_ty);
    let arg = builder.const_int(9, false, 64, u64_ty);
    builder.terminate(Terminator::Switch {
        value: cond,
        cases: vec![SwitchCase {
            value: ConstValue::Int {
                value: 0,
                signed: false,
                bits: 64,
            },
            target: target_bb,
            args: vec![arg],
        }],
        default: target_bb,
        default_args: vec![arg],
    });

    let param = builder.add_block_param(target_bb, u64_ty);
    builder.select_block(target_bb);
    builder.terminate(Terminator::Return { value: Some(param) });

    let mut func = builder.finish();
    super::run_cleanup(&mut func);

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            %v1: u64 = const 9:u64
            br bb1(%v1)

          bb1(%v2: u64):
            ret %v2
        }
    "};
    super::assert_ir_eq(text, expected);
}

#[test]
fn test_cleanup_prunes_unused_block_param() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let target_bb = builder.add_block();
    let arg = builder.const_int(0, false, 64, u64_ty);
    builder.terminate(Terminator::Br {
        target: target_bb,
        args: vec![arg],
    });

    let _param = builder.add_block_param(target_bb, u64_ty);
    builder.select_block(target_bb);
    let value = builder.const_int(5, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let mut func = builder.finish();
    super::run_cleanup(&mut func);

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            %v2: u64 = const 5:u64
            ret %v2
        }
    "};
    super::assert_ir_eq(text, expected);
}
