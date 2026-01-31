use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::DefId;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::ssa::lower::lower_func;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::format::formact_func;
use crate::ssa::model::ir::{FunctionSig, Terminator};
use crate::ssa::opt::cfg_free::PassManager;
use crate::ssa::{IrStructField, IrTypeCache, IrTypeKind};
use crate::typeck::type_check;
use indoc::indoc;

fn analyze(source: &str) -> crate::context::SemanticContext {
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
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

fn lower_and_optimize(source: &str) -> String {
    let ctx = analyze(source);
    let func_def = ctx.module.func_defs()[0];
    let mut lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    formact_func(&lowered.func, &lowered.types)
}

#[test]
fn test_const_fold_binop() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            let x = 1 + 2;
            x
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: u64 = const 3:u64
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_const_fold_cond_br() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            if true { 1 } else { 2 }
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: bool = const true
            br bb1

          bb1():
            %v2: u64 = const 1:u64
            br bb3(%v2)

          bb2():
            %v3: u64 = const 2:u64
            br bb3(%v3)

          bb3(%v1: u64):
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_index_addr_zero_fold() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr = types.add(IrTypeKind::Ptr { elem: u8_ty });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "idx_zero",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(u8_ty, None);
    let base = builder.addr_of_local(local, u8_ptr);
    let zero = builder.const_int(0, false, 64, u64_ty);
    let idx = builder.index_addr(base, zero, u8_ptr);
    let _load = builder.load(idx, u8_ty);
    builder.terminate(crate::ssa::model::ir::Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = formact_func(&func, &types);

    assert!(!text.contains("index_addr"));
}

#[test]
fn test_index_addr_zero_param_fold() {
    let mut types = IrTypeCache::new();
    let bool_ty = types.add(IrTypeKind::Bool);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr = types.add(IrTypeKind::Ptr { elem: u8_ty });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "idx_zero_param",
        FunctionSig {
            params: vec![bool_ty],
            ret: u8_ty,
        },
    );

    let entry = builder.current_block();
    let cond = builder.add_block_param(entry, bool_ty);
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let join_bb = builder.add_block();
    let zero = builder.const_int(0, false, 64, u64_ty);

    builder.set_terminator(
        entry,
        Terminator::CondBr {
            cond,
            then_bb,
            then_args: Vec::new(),
            else_bb,
            else_args: Vec::new(),
        },
    );

    builder.select_block(then_bb);
    builder.terminate(Terminator::Br {
        target: join_bb,
        args: vec![zero],
    });

    builder.select_block(else_bb);
    builder.terminate(Terminator::Br {
        target: join_bb,
        args: vec![zero],
    });

    builder.select_block(join_bb);
    let idx = builder.add_block_param(join_bb, u64_ty);
    let local = builder.add_local(u8_ty, None);
    let base = builder.addr_of_local(local, u8_ptr);
    let addr = builder.index_addr(base, idx, u8_ptr);
    let loaded = builder.load(addr, u8_ty);
    builder.terminate(Terminator::Return {
        value: Some(loaded),
    });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = formact_func(&func, &types);

    assert!(!text.contains("index_addr"));
}

#[test]
fn test_store_field_addr_elim() {
    let mut types = IrTypeCache::new();
    let _unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "x".into(),
                ty: u64_ty,
            },
            IrStructField {
                name: "y".into(),
                ty: u64_ty,
            },
        ],
    });
    let pair_ptr = types.add(IrTypeKind::Ptr { elem: pair_ty });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "store_field",
        FunctionSig {
            params: vec![pair_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, pair_ptr);
    let loaded = builder.load(param, pair_ty);
    let local = builder.add_local(pair_ty, None);
    let local_addr = builder.addr_of_local(local, pair_ptr);
    builder.store(local_addr, loaded);
    let field_ptr = builder.field_addr(local_addr, 0, u64_ptr);
    let field = builder.load(field_ptr, u64_ty);
    builder.set_terminator(entry, Terminator::Return { value: Some(field) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = formact_func(&func, &types);

    assert!(!text.contains("store "));
    assert!(text.contains(&format!("field_addr %v{}, 0", param.0)));
}

#[test]
fn test_local_load_forward() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "local_load_forward",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let local = builder.add_local(u64_ty, None);
    let addr = builder.addr_of_local(local, u64_ptr);
    let value = builder.const_int(42, false, 64, u64_ty);
    builder.store(addr, value);
    let loaded = builder.load(addr, u64_ty);
    builder.terminate(Terminator::Return {
        value: Some(loaded),
    });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = formact_func(&func, &types);

    assert!(!text.contains("load "));
    assert!(text.contains("const 42:u64"));
}

#[test]
fn test_field_addr_cse() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "x".into(),
                ty: u64_ty,
            },
            IrStructField {
                name: "y".into(),
                ty: u64_ty,
            },
        ],
    });
    let pair_ptr = types.add(IrTypeKind::Ptr { elem: pair_ty });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "field_addr_cse",
        FunctionSig {
            params: vec![pair_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, pair_ptr);
    let first = builder.field_addr(param, 0, u64_ptr);
    let second = builder.field_addr(param, 0, u64_ptr);
    let a = builder.load(first, u64_ty);
    let b = builder.load(second, u64_ty);
    let sum = builder.binop(crate::ssa::model::ir::BinOp::Add, a, b, u64_ty);
    builder.set_terminator(entry, Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = formact_func(&func, &types);

    let field_addr_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, crate::ssa::model::ir::InstKind::FieldAddr { .. }))
        .count();
    assert_eq!(field_addr_count, 1, "{text}");
}

#[test]
fn test_load_cse() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "load_cse",
        FunctionSig {
            params: vec![u64_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let addr = builder.add_block_param(entry, u64_ptr);
    let first = builder.load(addr, u64_ty);
    let second = builder.load(addr, u64_ty);
    let sum = builder.binop(crate::ssa::model::ir::BinOp::Add, first, second, u64_ty);
    builder.terminate(Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let load_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, crate::ssa::model::ir::InstKind::Load { .. }))
        .count();
    assert_eq!(load_count, 1);
}
