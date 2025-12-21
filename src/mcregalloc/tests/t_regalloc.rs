use crate::ids::DefId;
use crate::mcir::types::{
    BasicBlock, BlockId, Body, Callee, Const, Local, LocalId, LocalKind, Operand, Place, PlaceAny,
    Rvalue, Statement, Terminator, TyKind, TyTable,
};
use crate::mcregalloc::MappedLocal;
use crate::mcregalloc::alloc::RegAlloc;
use crate::mcregalloc::constraints::{CallArgKind, analyze_constraints};
use crate::regalloc::regs;

fn u64_ty(types: &mut TyTable) -> crate::mcir::types::TyId {
    types.add(TyKind::Int {
        signed: false,
        bits: 64,
    })
}

fn tuple2_ty(
    types: &mut TyTable,
    a: crate::mcir::types::TyId,
    b: crate::mcir::types::TyId,
) -> crate::mcir::types::TyId {
    types.add(TyKind::Tuple {
        field_tys: vec![a, b],
    })
}

fn mk_body(
    types: TyTable,
    locals: Vec<Local>,
    blocks: Vec<BasicBlock>,
    ret_local: LocalId,
) -> Body {
    Body {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local,
        types,
    }
}

#[test]
fn test_alloc_stack_addr_for_aggregate() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let tup_ty = tuple2_ty(&mut types, u64_ty, u64_ty);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let p0 = Place::new(l0, u64_ty, vec![]);
    let p1 = Place::new(l1, tup_ty, vec![]);

    let stmts = vec![
        Statement::InitAggregate {
            dst: p1.clone(),
            fields: vec![
                Operand::Const(Const::Int {
                    value: 1,
                    signed: false,
                    bits: 64,
                }),
                Operand::Const(Const::Int {
                    value: 2,
                    signed: false,
                    bits: 64,
                }),
            ],
        },
        Statement::AssignScalar {
            dst: p0.clone(),
            src: Rvalue::Use(Operand::Const(Const::Int {
                value: 1,
                signed: false,
                bits: 64,
            })),
        },
    ];
    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    let alloc = RegAlloc::new(&body, &constraints).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l1),
        Some(MappedLocal::StackAddr(_))
    ));
}

#[test]
fn test_param_aggregate_is_address_value() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let tup_ty = tuple2_ty(&mut types, u64_ty, u64_ty);

    let locals = vec![
        Local {
            ty: tup_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Param { index: 0 },
            name: Some("p0".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);

    let p0 = Place::new(l0, tup_ty, vec![]);
    let p1 = Place::new(l1, tup_ty, vec![]);

    let stmts = vec![Statement::CopyAggregate {
        dst: p0.clone(),
        src: p1.clone(),
    }];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    let alloc = RegAlloc::new(&body, &constraints).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l1),
        Some(MappedLocal::Reg(reg)) if *reg == regs::get_param_reg(0)
    ));
}

#[test]
fn test_aggregate_return_uses_indirect_reg() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let tup_ty = tuple2_ty(&mut types, u64_ty, u64_ty);

    let locals = vec![Local {
        ty: tup_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];

    let l0 = LocalId(0);
    let p0 = Place::new(
        l0,
        u64_ty,
        vec![crate::mcir::types::Projection::Field { index: 0 }],
    );

    let stmts = vec![Statement::AssignScalar {
        dst: p0,
        src: Rvalue::Use(Operand::Const(Const::Int {
            value: 1,
            signed: false,
            bits: 64,
        })),
    }];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    let alloc = RegAlloc::new(&body, &constraints).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l0),
        Some(MappedLocal::Reg(reg)) if *reg == regs::get_indirect_result_reg()
    ));
}

#[test]
fn test_alloc_stack_addr_for_address_taken() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p0 = Place::new(l0, u64_ty, vec![]);
    let p1 = Place::new(l1, u64_ty, vec![]);
    let p2 = Place::new(l2, u64_ty, vec![]);

    let stmts = vec![
        Statement::AssignScalar {
            dst: p2.clone(),
            src: Rvalue::AddrOf(PlaceAny::Scalar(p1.clone())),
        },
        Statement::AssignScalar {
            dst: p0.clone(),
            src: Rvalue::Use(Operand::Copy(p2.clone())),
        },
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    let alloc = RegAlloc::new(&body, &constraints).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l1),
        Some(MappedLocal::StackAddr(_))
    ));
}

#[test]
fn test_call_constraints_scalar_and_aggregate_args() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let tup_ty = tuple2_ty(&mut types, u64_ty, u64_ty);

    let locals = vec![
        Local {
            ty: u64_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Temp,
            name: None,
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);
    let l3 = LocalId(3);

    let p0 = Place::new(l0, u64_ty, vec![]);
    let p1 = Place::new(l1, u64_ty, vec![]);
    let p2 = Place::new(l2, tup_ty, vec![]);
    let p3 = Place::new(l3, u64_ty, vec![]);

    let stmts = vec![
        Statement::Call {
            dst: PlaceAny::Scalar(p3.clone()),
            callee: Callee::Def(DefId(0)),
            args: vec![
                PlaceAny::Scalar(p1.clone()),
                PlaceAny::Aggregate(p2.clone()),
            ],
        },
        Statement::AssignScalar {
            dst: p0.clone(),
            src: Rvalue::Use(Operand::Copy(p3.clone())),
        },
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    assert_eq!(constraints.call_constraints.len(), 1);

    let call = &constraints.call_constraints[0];
    assert_eq!(call.args.len(), 2);
    assert_eq!(call.args[0].kind, CallArgKind::Value);
    assert_eq!(call.args[1].kind, CallArgKind::Addr);
    assert!(call.result.is_some());
    assert_eq!(call.result.as_ref().unwrap().reg, regs::get_result_reg());
}

#[test]
fn test_call_constraints_sret() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let tup_ty = tuple2_ty(&mut types, u64_ty, u64_ty);

    let locals = vec![
        Local {
            ty: tup_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        },
        Local {
            ty: u64_ty,
            kind: LocalKind::Temp,
            name: None,
        },
        Local {
            ty: tup_ty,
            kind: LocalKind::Temp,
            name: None,
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let l2 = LocalId(2);

    let p1 = Place::new(l1, u64_ty, vec![]);
    let p2 = Place::new(l2, tup_ty, vec![]);

    let stmts = vec![Statement::Call {
        dst: PlaceAny::Aggregate(p2.clone()),
        callee: Callee::Def(DefId(1)),
        args: vec![PlaceAny::Scalar(p1.clone())],
    }];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let constraints = analyze_constraints(&body);
    assert_eq!(constraints.call_constraints.len(), 1);

    let call = &constraints.call_constraints[0];
    assert_eq!(call.args.len(), 2);
    assert_eq!(call.args[0].kind, CallArgKind::Value);
    assert_eq!(call.args[1].kind, CallArgKind::Addr);
    assert_eq!(call.args[1].reg, regs::get_indirect_result_reg());
    assert!(call.result.is_none());
}
