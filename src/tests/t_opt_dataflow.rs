use std::collections::HashMap;

use crate::context::LivenessContext;
use crate::liveness::LivenessAnalysis;
use crate::lower::LoweredFunc;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::{
    BasicBlock, BlockId, Callee, Const, FuncBody, GlobalItem, Local, LocalId, LocalKind, Operand,
    Place, PlaceAny, Projection, Rvalue, Statement, Terminator, TyKind, TyTable,
};
use crate::opt::dataflow::copy_elide;
use crate::opt::dataflow::memcpy_lower;
use crate::resolve::def_map::DefId;
use crate::symtab::SymbolTable;

fn const_u64(value: u64) -> Operand {
    Operand::Const(Const::Int {
        value: value as i128,
        signed: false,
        bits: 64,
    })
}

fn ret_zero(dst: Place<crate::mcir::types::Scalar>) -> Statement {
    Statement::CopyScalar {
        dst,
        src: Rvalue::Use(const_u64(0)),
    }
}

fn u64_ty(types: &mut TyTable) -> crate::mcir::types::TyId {
    types.add(TyKind::Int {
        signed: false,
        bits: 64,
    })
}

fn array_ty(
    types: &mut TyTable,
    elem_ty: crate::mcir::types::TyId,
    dims: Vec<usize>,
) -> crate::mcir::types::TyId {
    types.add(TyKind::Array { elem_ty, dims })
}

fn tuple_ty(
    types: &mut TyTable,
    field_tys: Vec<crate::mcir::types::TyId>,
) -> crate::mcir::types::TyId {
    types.add(TyKind::Tuple { field_tys })
}

fn mk_body(
    types: TyTable,
    locals: Vec<Local>,
    blocks: Vec<BasicBlock>,
    ret_local: LocalId,
) -> FuncBody {
    FuncBody {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local,
        types,
    }
}

fn run_elide(body: FuncBody) -> FuncBody {
    let live_map = LivenessAnalysis::new(&body).analyze();
    let live_ctx = LivenessContext {
        funcs: vec![LoweredFunc {
            def_id: DefId(0),
            body,
        }],
        live_maps: vec![live_map],
        symbols: SymbolTable {
            func_ids: Vec::new(),
            def_names: HashMap::new(),
        },
        globals: Vec::<GlobalItem>::new(),
    };

    let mut optimized = copy_elide::run(live_ctx).funcs;
    optimized.pop().expect("expected one optimized body").body
}

fn run_memcpy_lower(body: FuncBody) -> FuncBody {
    let mut funcs = vec![LoweredFunc {
        def_id: DefId(0),
        body,
    }];
    memcpy_lower::run(&mut funcs);
    funcs.pop().expect("expected one optimized body").body
}

#[test]
fn test_copy_elide_removes_last_use_copy() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let stmts = vec![
        Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert_eq!(stmts.len(), 1);
    assert!(
        matches!(stmts[0], Statement::CopyScalar { .. }),
        "expected copy scalar to remain"
    );
}

#[test]
fn test_copy_elide_keeps_addr_taken_source() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: Some("addr".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);
    let l3 = LocalId(3);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);
    let p_addr = Place::new(l3, u64_ty, vec![]);

    let stmts = vec![
        Statement::CopyScalar {
            dst: p_addr,
            src: Rvalue::AddrOf(PlaceAny::Aggregate(p_a1.clone())),
        },
        Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected copy aggregate to remain when source is address-taken"
    );
}

#[test]
fn test_copy_elide_keeps_src_live_after() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let stmts = vec![
        Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        },
        Statement::CopyAggregate {
            dst: p_a1.clone(),
            src: p_a1.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected copy aggregate to remain when source is live after the copy"
    );
}

#[test]
fn test_copy_elide_across_blocks() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let block0 = BasicBlock {
        stmts: vec![Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        }],
        terminator: Terminator::Goto(BlockId(1)),
    };

    let block1 = BasicBlock {
        stmts: vec![
            Statement::CopyAggregate {
                dst: p_a1.clone(),
                src: p_a2.clone(),
            },
            ret_zero(p_ret),
        ],
        terminator: Terminator::Return,
    };

    let body = mk_body(types, locals, vec![block0, block1], l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().all(|stmt| {
            !matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected copy aggregate to be elided across blocks"
    );
}

#[test]
fn test_copy_elide_keeps_projected_copy() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![2]);
    let tup_ty = tuple_ty(&mut types, vec![arr_ty, arr_ty]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Temp,
            name: Some("t1".to_string()),
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Temp,
            name: Some("t2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);

    let p_t1_f0 = Place::new(l1, arr_ty, vec![Projection::Field { index: 0 }]);
    let p_t2_f0 = Place::new(l2, arr_ty, vec![Projection::Field { index: 0 }]);

    let stmts = vec![
        Statement::CopyAggregate {
            dst: p_t2_f0.clone(),
            src: p_t1_f0.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected projected copy aggregate to remain"
    );
}

#[test]
fn test_copy_elide_keeps_dst_addr_taken() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: Some("addr".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);
    let l3 = LocalId(3);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);
    let p_addr = Place::new(l3, u64_ty, vec![]);

    let stmts = vec![
        Statement::CopyScalar {
            dst: p_addr,
            src: Rvalue::AddrOf(PlaceAny::Aggregate(p_a2.clone())),
        },
        Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected copy aggregate to remain when destination is address-taken"
    );
}

#[test]
fn test_copy_elide_keeps_addr_taken_call_arg() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let arr_ty = array_ty(&mut types, u64_ty, vec![3]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let stmts = vec![
        Statement::Call {
            dst: None,
            callee: Callee::Runtime(RuntimeFn::Print),
            args: vec![
                PlaceAny::Aggregate(p_a1.clone()),
                PlaceAny::Scalar(p_ret.clone()),
            ],
        },
        Statement::CopyAggregate {
            dst: p_a2.clone(),
            src: p_a1.clone(),
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_elide(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::CopyAggregate { dst, src } if dst.base() == l2 && src.base() == l1
            )
        }),
        "expected copy aggregate to remain when source is passed as aggregate call arg"
    );
}

#[test]
fn test_memcpy_lower_inlines_small_copy() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });
    let arr_ty = array_ty(&mut types, u8_ty, vec![8]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let stmts = vec![
        Statement::CopyAggregate {
            dst: p_a2,
            src: p_a1,
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_memcpy_lower(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts
            .iter()
            .all(|stmt| !matches!(stmt, Statement::CopyAggregate { .. })),
        "expected CopyAggregate to be lowered"
    );
    assert!(
        stmts.iter().all(|stmt| {
            !matches!(
                stmt,
                Statement::Call {
                    callee: Callee::Runtime(RuntimeFn::MemCopy),
                    ..
                }
            )
        }),
        "expected inline memcpy for small copies"
    );
    assert_eq!(
        stmts
            .iter()
            .filter(|stmt| matches!(stmt, Statement::CopyScalar { .. }))
            .count(),
        9,
        "expected 8 inline byte copies plus return store"
    );
}

#[test]
fn test_memcpy_lower_calls_runtime_for_large_copy() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });
    let arr_ty = array_ty(&mut types, u8_ty, vec![32]);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a1".to_string()),
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: Some("a2".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p_ret = Place::new(l0, u64_ty, vec![]);
    let p_a1 = Place::new(l1, arr_ty, vec![]);
    let p_a2 = Place::new(l2, arr_ty, vec![]);

    let stmts = vec![
        Statement::CopyAggregate {
            dst: p_a2,
            src: p_a1,
        },
        ret_zero(p_ret),
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let optimized = run_memcpy_lower(body);
    let stmts = &optimized.blocks[0].stmts;
    assert!(
        stmts
            .iter()
            .all(|stmt| !matches!(stmt, Statement::CopyAggregate { .. })),
        "expected CopyAggregate to be lowered"
    );
    assert!(
        stmts.iter().any(|stmt| matches!(
            stmt,
            Statement::Call {
                callee: Callee::Runtime(RuntimeFn::MemCopy),
                args,
                ..
            } if matches!(args.as_slice(), [PlaceAny::Aggregate(_), PlaceAny::Aggregate(_)])
        )),
        "expected runtime __mc_memcpy call for large copy"
    );
}
