use crate::resolve::DefId;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::format::format_function;
use crate::ssa::model::ir::{FunctionSig, Terminator, TypeKind, TypeTable};
use indoc::indoc;

#[test]
fn test_format_const_return() {
    let mut types = TypeTable::new();
    let u64_ty = types.add(TypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "const_42",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );
    let entry = builder.add_block();
    let value = builder.const_int(entry, 42, false, 64, u64_ty);
    builder.set_terminator(entry, Terminator::Return { value: Some(value) });
    let func = builder.finish();

    let text = format_function(&func, &types);
    let expected = indoc! {"
        fn const_42() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}
