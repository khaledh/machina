use crate::resolve::DefId;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::format::format_func;
use crate::ssa::model::ir::{FunctionSig, Terminator};
use crate::ssa::{IrTypeCache, IrTypeKind};
use indoc::indoc;

#[test]
fn test_format_const_return() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    // Builder starts with cursor at entry block (block 0).
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "const_42",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );
    let value = builder.const_int(42, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });
    let func = builder.finish();

    let text = format_func(&func, &types);
    let expected = indoc! {"
        fn const_42() -> u64 {
          bb0():
            %v0: u64 = const 42

            ret %v0
        }
    "};
    assert_eq!(text, expected);
}
