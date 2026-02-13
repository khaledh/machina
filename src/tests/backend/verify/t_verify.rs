use crate::core::backend::lower::LoweredModule;
use crate::core::backend::lower::lower_module;
use crate::core::backend::verify::verify_module;
use crate::core::context::{ParsedContext, SemanticContext};
use crate::core::elaborate::elaborate;
use crate::core::ir::{
    BinOp, Callee, ConstValue, InstKind, Instruction, IrTypeKind, RuntimeFn, Terminator, ValueDef,
    ValueId,
};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::semck::sem_check;
use crate::core::typecheck::type_check;

fn analyze(source: &str) -> SemanticContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let sem_checked_context = sem_check(type_checked_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

fn lower(source: &str) -> LoweredModule {
    let ctx = analyze(source);
    lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower")
}

#[test]
fn test_verify_accepts_valid_module() {
    let src = r#"
fn main() -> u64 {
  let a = 1;
  a
}
"#;
    let module = lower(src);
    verify_module(&module).expect("expected verify to pass");
}

#[test]
fn test_verify_rejects_undefined_value_use() {
    let src = r#"
fn main() -> u64 {
  let a = 1;
  a + 2
}
"#;
    let mut module = lower(src);
    let lowered = &mut module.funcs[0];
    let func = &mut lowered.func;
    let block = &mut func.blocks[0];
    let result_ty = func.sig.ret;

    block.insts.push(Instruction {
        result: Some(ValueDef {
            id: ValueId(9999),
            ty: result_ty,
        }),
        kind: InstKind::BinOp {
            op: BinOp::Add,
            lhs: ValueId(4242),
            rhs: ValueId(4243),
        },
        comments: Vec::new(),
    });

    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_branch_arg_mismatch() {
    let src = r#"
fn main() -> u64 {
  let x = if true { 1 } else { 2 };
  x
}
"#;
    let mut module = lower(src);
    let func = &mut module.funcs[0].func;
    let mut mutated = false;

    for block in &mut func.blocks {
        match &mut block.term {
            Terminator::Br { args, .. } if !args.is_empty() => {
                args.pop();
                mutated = true;
                break;
            }
            Terminator::Br { .. } => {}
            Terminator::CondBr {
                then_args,
                else_args,
                ..
            } if !then_args.is_empty() => {
                then_args.pop();
                mutated = true;
                break;
            }
            Terminator::CondBr { .. } => {}
            Terminator::Switch { .. } => {}
            Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    assert!(mutated, "test setup failed to find branch args");
    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_non_pointer_store() {
    let src = r#"
fn main() -> u64 {
  var x = 0;
  x = 1;
  x
}
"#;
    let mut module = lower(src);
    let types = module.funcs[0].types.clone();
    let func = &mut module.funcs[0].func;
    let mut mutated = false;

    let non_ptr_value = func
        .blocks
        .iter()
        .flat_map(|block| block.insts.iter())
        .filter_map(|inst| inst.result.as_ref())
        .find(|value| !matches!(types.kind(value.ty), IrTypeKind::Ptr { .. }))
        .map(|value| value.id);
    let next_id = func
        .blocks
        .iter()
        .flat_map(|block| block.insts.iter().filter_map(|inst| inst.result.as_ref()))
        .map(|value| value.id.0)
        .max()
        .unwrap_or(0)
        + 1;
    let non_ptr_local = func
        .locals
        .iter()
        .find(|local| !matches!(types.kind(local.ty), IrTypeKind::Ptr { .. }))
        .map(|local| local.ty);

    for block in &mut func.blocks {
        if let Some(non_ptr_value) = non_ptr_value {
            block.insts.push(Instruction {
                result: None,
                kind: InstKind::Store {
                    ptr: non_ptr_value,
                    value: non_ptr_value,
                },
                comments: Vec::new(),
            });
            mutated = true;
            break;
        }
        if let Some(local_ty) = non_ptr_local {
            let value_id = ValueId(next_id);
            block.insts.push(Instruction {
                result: Some(ValueDef {
                    id: value_id,
                    ty: local_ty,
                }),
                kind: InstKind::Const {
                    value: ConstValue::Int {
                        value: 0,
                        signed: false,
                        bits: 64,
                    },
                },
                comments: Vec::new(),
            });
            block.insts.push(Instruction {
                result: None,
                kind: InstKind::Store {
                    ptr: value_id,
                    value: value_id,
                },
                comments: Vec::new(),
            });
            mutated = true;
            break;
        }
    }

    assert!(mutated, "test setup failed to find store");
    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_direct_call_arg_count_mismatch() {
    let src = r#"
fn add(a: u64, b: u64) -> u64 {
  a + b
}

fn main() -> u64 {
  add(1, 2)
}
"#;
    let mut module = lower(src);
    let mut mutated = false;
    for lowered in &mut module.funcs {
        for block in &mut lowered.func.blocks {
            for inst in &mut block.insts {
                if let InstKind::Call {
                    callee: Callee::Direct(_),
                    args,
                } = &mut inst.kind
                    && !args.is_empty()
                {
                    args.pop();
                    mutated = true;
                    break;
                }
            }
            if mutated {
                break;
            }
        }
        if mutated {
            break;
        }
    }
    assert!(mutated, "test setup failed to mutate direct call");
    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_direct_call_result_type_mismatch() {
    let src = r#"
fn add(a: u64, b: u64) -> u64 {
  a + b
}

fn main() -> u64 {
  add(1, 2)
}
"#;
    let mut module = lower(src);
    let mut mutated = false;

    for lowered in &mut module.funcs {
        let ptr_ty = lowered.types.add(IrTypeKind::Ptr {
            elem: lowered.func.sig.ret,
        });
        for block in &mut lowered.func.blocks {
            for inst in &mut block.insts {
                if let InstKind::Call {
                    callee: Callee::Direct(_),
                    ..
                } = &inst.kind
                    && let Some(result) = inst.result.as_mut()
                {
                    result.ty = ptr_ty;
                    mutated = true;
                    break;
                }
            }
            if mutated {
                break;
            }
        }
        if mutated {
            break;
        }
    }
    assert!(mutated, "test setup failed to mutate call result type");
    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_runtime_call_arg_count_mismatch() {
    let src = r#"
fn main() -> bool {
  let a = "a";
  let b = "b";
  a == b
}
"#;
    let mut module = lower(src);
    let mut mutated = false;

    for lowered in &mut module.funcs {
        for block in &mut lowered.func.blocks {
            for inst in &mut block.insts {
                if let InstKind::Call {
                    callee: Callee::Runtime(RuntimeFn::StringEq),
                    args,
                } = &mut inst.kind
                    && !args.is_empty()
                {
                    args.pop();
                    mutated = true;
                    break;
                }
            }
            if mutated {
                break;
            }
        }
        if mutated {
            break;
        }
    }
    assert!(mutated, "test setup failed to mutate runtime call");
    assert!(verify_module(&module).is_err());
}

#[test]
fn test_verify_rejects_runtime_call_non_register_arg() {
    let src = r#"
fn main() -> bool {
  let a = "a";
  let b = "b";
  a == b
}
"#;
    let mut module = lower(src);
    let mut mutated = false;

    for lowered in &mut module.funcs {
        let non_reg_value = lowered
            .func
            .blocks
            .iter()
            .flat_map(|block| block.insts.iter())
            .filter_map(|inst| inst.result.as_ref())
            .find(|value| !lowered.types.is_reg_type(value.ty))
            .map(|value| value.id);

        let Some(non_reg_value) = non_reg_value else {
            continue;
        };

        for block in &mut lowered.func.blocks {
            for inst in &mut block.insts {
                if let InstKind::Call {
                    callee: Callee::Runtime(RuntimeFn::StringEq),
                    args,
                } = &mut inst.kind
                {
                    if let Some(first) = args.first_mut() {
                        *first = non_reg_value;
                        mutated = true;
                    }
                    break;
                }
            }
            if mutated {
                break;
            }
        }
        if mutated {
            break;
        }
    }

    assert!(
        mutated,
        "test setup failed to inject non-register runtime arg"
    );
    assert!(verify_module(&module).is_err());
}
