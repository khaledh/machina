use super::Pass;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::{
    BasicBlock, BlockId, Callee, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place,
    PlaceAny, Rvalue, Statement, Terminator, TyKind, TyTable,
};
use crate::opt::cfg_free::const_branch_elim::ConstBranchElim;
use crate::opt::cfg_free::local_simplify::LocalSimplify;
use crate::opt::cfg_free::memset_lower::MemSetLower;
use crate::opt::cfg_free::self_copy_elim::RemoveSelfCopies;

#[test]
fn test_remove_self_copies() {
    let mut types = TyTable::new();
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });
    let tuple_ty = types.add(TyKind::Tuple {
        field_tys: vec![u64_ty],
    });

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
        Local {
            ty: tuple_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let scalar_place = Place::new(LocalId(0), u64_ty, vec![]);
    let agg_place = Place::new(LocalId(1), tuple_ty, vec![]);

    let block = BasicBlock {
        stmts: vec![
            Statement::CopyScalar {
                dst: scalar_place.clone(),
                src: Rvalue::Use(Operand::Copy(scalar_place.clone())),
            },
            Statement::CopyScalar {
                dst: scalar_place.clone(),
                src: Rvalue::Use(Operand::Const(Const::Int {
                    value: 1,
                    signed: false,
                    bits: 64,
                })),
            },
            Statement::CopyAggregate {
                dst: agg_place.clone(),
                src: agg_place.clone(),
            },
        ],
        terminator: Terminator::Return,
    };

    let mut body = FuncBody {
        locals,
        blocks: vec![block],
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = RemoveSelfCopies;
    pass.run(&mut body);

    let stmts = &body.blocks[0].stmts;
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 1);
                }
                _ => panic!("expected const copy to remain"),
            }
        }
        _ => panic!("expected const copy to remain"),
    }
}

#[test]
fn test_const_branch_elim() {
    let mut types = TyTable::new();
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });

    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Temp,
        name: None,
    }];

    let blocks = vec![
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Switch {
                discr: Operand::Const(Const::Int {
                    value: 1,
                    signed: false,
                    bits: 64,
                }),
                cases: vec![
                    crate::mcir::SwitchCase {
                        value: 0,
                        target: BlockId(1),
                    },
                    crate::mcir::SwitchCase {
                        value: 1,
                        target: BlockId(2),
                    },
                ],
                default: BlockId(3),
            },
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
    ];

    let mut body = FuncBody {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = ConstBranchElim;
    pass.run(&mut body);

    match &body.blocks[0].terminator {
        Terminator::Goto(target) => assert_eq!(*target, BlockId(2)),
        _ => panic!("expected Switch to be folded to Goto"),
    }
}

#[test]
fn test_local_simplify_consts_and_terminators() {
    let mut types = TyTable::new();
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });

    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Temp,
        name: None,
    }];

    let place = Place::new(LocalId(0), u64_ty, vec![]);

    let blocks = vec![
        BasicBlock {
            stmts: vec![Statement::CopyScalar {
                dst: place.clone(),
                src: Rvalue::BinOp {
                    op: crate::mcir::BinOp::Add,
                    lhs: Operand::Const(Const::Int {
                        value: 2,
                        signed: false,
                        bits: 64,
                    }),
                    rhs: Operand::Const(Const::Int {
                        value: 3,
                        signed: false,
                        bits: 64,
                    }),
                },
            }],
            terminator: Terminator::If {
                cond: Operand::Copy(place.clone()),
                then_bb: BlockId(1),
                else_bb: BlockId(1),
            },
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
    ];

    let mut body = FuncBody {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = LocalSimplify;
    pass.run(&mut body);

    match &body.blocks[0].stmts[0] {
        Statement::CopyScalar { src, .. } => match src {
            Rvalue::Use(Operand::Const(Const::Int { value, .. })) => assert_eq!(*value, 5),
            _ => panic!("expected folded const add"),
        },
        _ => panic!("expected CopyScalar"),
    }

    match &body.blocks[0].terminator {
        Terminator::Goto(target) => assert_eq!(*target, BlockId(1)),
        _ => panic!("expected If to be folded to Goto"),
    }
}

#[test]
fn test_local_simplify_folds_if_const_temp() {
    let mut types = TyTable::new();
    let bool_ty = types.add(TyKind::Bool);

    let locals = vec![Local {
        ty: bool_ty,
        kind: LocalKind::Temp,
        name: None,
    }];

    let place = Place::new(LocalId(0), bool_ty, vec![]);

    let blocks = vec![
        BasicBlock {
            stmts: vec![Statement::CopyScalar {
                dst: place.clone(),
                src: Rvalue::Use(Operand::Const(Const::Bool(true))),
            }],
            terminator: Terminator::If {
                cond: Operand::Copy(place.clone()),
                then_bb: BlockId(1),
                else_bb: BlockId(2),
            },
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
        BasicBlock {
            stmts: vec![],
            terminator: Terminator::Return,
        },
    ];

    let mut body = FuncBody {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = LocalSimplify;
    let changed = pass.run(&mut body);
    assert!(changed);

    match &body.blocks[0].terminator {
        Terminator::Goto(target) => assert_eq!(*target, BlockId(1)),
        _ => panic!("expected If to be folded to Goto"),
    }
}

#[test]
fn test_memset_lower_inlines_small_memset() {
    let mut types = TyTable::new();
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });
    let arr_ty = types.add(TyKind::Array {
        elem_ty: u8_ty,
        dims: vec![4],
    });

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: None,
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let arr_place = Place::new(LocalId(1), arr_ty, vec![]);

    let block = BasicBlock {
        stmts: vec![Statement::MemSet {
            dst: arr_place,
            value: Operand::Const(Const::Int {
                value: 7,
                signed: false,
                bits: 8,
            }),
            len: 4,
        }],
        terminator: Terminator::Return,
    };

    let mut body = FuncBody {
        locals,
        blocks: vec![block],
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = MemSetLower;
    pass.run(&mut body);

    let stmts = &body.blocks[0].stmts;
    assert!(
        stmts
            .iter()
            .all(|stmt| !matches!(stmt, Statement::MemSet { .. })),
        "expected MemSet to be lowered"
    );
    assert_eq!(
        stmts
            .iter()
            .filter(|stmt| matches!(stmt, Statement::CopyScalar { .. }))
            .count(),
        4,
        "expected inline stores for small MemSet"
    );
}

#[test]
fn test_memset_lower_calls_runtime_for_large_memset() {
    let mut types = TyTable::new();
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });
    let arr_ty = types.add(TyKind::Array {
        elem_ty: u8_ty,
        dims: vec![32],
    });

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: None,
        },
        Local {
            ty: arr_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let arr_place = Place::new(LocalId(1), arr_ty, vec![]);

    let block = BasicBlock {
        stmts: vec![Statement::MemSet {
            dst: arr_place,
            value: Operand::Const(Const::Int {
                value: 1,
                signed: false,
                bits: 8,
            }),
            len: 32,
        }],
        terminator: Terminator::Return,
    };

    let mut body = FuncBody {
        locals,
        blocks: vec![block],
        entry: BlockId(0),
        ret_local: LocalId(0),
        types,
    };

    let mut pass = MemSetLower;
    pass.run(&mut body);

    let stmts = &body.blocks[0].stmts;
    assert!(
        stmts
            .iter()
            .all(|stmt| !matches!(stmt, Statement::MemSet { .. })),
        "expected MemSet to be lowered"
    );
    assert!(
        stmts.iter().any(|stmt| matches!(
            stmt,
            Statement::Call {
                callee: Callee::Runtime(RuntimeFn::MemSet),
                args,
                ..
            } if matches!(args.as_slice(), [PlaceAny::Aggregate(_), PlaceAny::Scalar(_)])
        )),
        "expected runtime __mc_memset call for large MemSet"
    );
}
