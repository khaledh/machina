use crate::liveness::LivenessAnalysis;
use crate::mcir::types::{
    BasicBlock, BlockId, Callee, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place,
    PlaceAny, Projection, Rvalue, Statement, Terminator, TyId, TyKind, TyTable,
};
use crate::regalloc::MappedLocal;
use crate::regalloc::alloc::RegAlloc;
use crate::regalloc::constraints::{CallArgKind, analyze_constraints};
use crate::regalloc::moves::{FnMoveList, Location};
use crate::regalloc::pos::{InstPos, RelInstPos};
use crate::regalloc::stack::StackSlotId;
use crate::regalloc::target::PhysReg;
use crate::regalloc::target::TargetSpec;
use crate::resolve::DefId;
use crate::targets::arm64::regs::Arm64Target;

fn u64_ty(types: &mut TyTable) -> TyId {
    types.add(TyKind::Int {
        signed: false,
        bits: 64,
    })
}

fn tuple2_ty(types: &mut TyTable, a: TyId, b: TyId) -> TyId {
    types.add(TyKind::Tuple {
        field_tys: vec![a, b],
    })
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

fn arm64_target() -> Arm64Target {
    Arm64Target::new()
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
        Statement::CopyAggregate {
            dst: p1.clone(),
            src: p1.clone(),
        },
        Statement::CopyScalar {
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
    let target = arm64_target();
    let constraints = analyze_constraints(&body, &target);
    let live_map = LivenessAnalysis::new(&body).analyze();
    let alloc = RegAlloc::new(&body, &constraints, &target, &live_map).alloc();

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
    let target = arm64_target();
    let constraints = analyze_constraints(&body, &target);
    let live_map = LivenessAnalysis::new(&body).analyze();
    let alloc = RegAlloc::new(&body, &constraints, &target, &live_map).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l1),
        Some(MappedLocal::Reg(reg)) if *reg == target.param_reg(0).unwrap()
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
    let p0 = Place::new(l0, u64_ty, vec![Projection::Field { index: 0 }]);

    let stmts = vec![Statement::CopyScalar {
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
    let target = arm64_target();
    let constraints = analyze_constraints(&body, &target);
    let live_map = LivenessAnalysis::new(&body).analyze();
    let alloc = RegAlloc::new(&body, &constraints, &target, &live_map).alloc();

    assert!(matches!(
        alloc.alloc_map.get(&l0),
        Some(MappedLocal::Reg(reg)) if *reg == target.indirect_result_reg().unwrap()
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
            dst: Some(PlaceAny::Scalar(p3.clone())),
            callee: Callee::Def(DefId(0)),
            args: vec![
                PlaceAny::Scalar(p1.clone()),
                PlaceAny::Aggregate(p2.clone()),
            ],
        },
        Statement::CopyScalar {
            dst: p0.clone(),
            src: Rvalue::Use(Operand::Copy(p3.clone())),
        },
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let target = arm64_target();
    let constraints = analyze_constraints(&body, &target);
    assert_eq!(constraints.call_constraints.len(), 1);

    let call = &constraints.call_constraints[0];
    assert_eq!(call.args.len(), 2);
    assert_eq!(call.args[0].kind, CallArgKind::Value);
    assert_eq!(call.args[1].kind, CallArgKind::Addr);
    assert!(call.result.is_some());
    assert_eq!(call.result.as_ref().unwrap().reg, target.result_reg());
}

#[test]
fn test_placevalue_move_orders_before_clobber() {
    let mut moves = FnMoveList::new();
    let pos = InstPos::new(BlockId(0), 0);
    let place = Place::new(LocalId(0), TyId(0), vec![Projection::Deref]);

    // Preserve x0, then clobber it with an arg move.
    moves.add_inst_move(
        RelInstPos::Before(pos),
        Location::Reg(PhysReg(0)),
        Location::Stack(StackSlotId(0)),
    );
    moves.add_inst_move(
        RelInstPos::Before(pos),
        Location::Stack(StackSlotId(1)),
        Location::Reg(PhysReg(0)),
    );
    // PlaceValue depends on x0; it should be ordered before the clobber move.
    moves.add_inst_move(
        RelInstPos::Before(pos),
        Location::PlaceValue(place),
        Location::Reg(PhysReg(1)),
    );

    moves.resolve_parallel_moves(&[PhysReg(16)], |mov| match &mov.from {
        Location::PlaceValue(_) => vec![PhysReg(0)],
        _ => Vec::new(),
    });

    let list = moves.get_inst_moves(pos).expect("missing moves at inst");
    let mut place_idx = None;
    let mut clobber_idx = None;
    for (idx, mov) in list.before_moves.iter().enumerate() {
        if matches!(mov.from, Location::PlaceValue(_)) {
            place_idx = Some(idx);
        }
        if matches!(mov.to, Location::Reg(reg) if reg == PhysReg(0)) {
            clobber_idx = Some(idx);
        }
    }

    assert!(
        place_idx.is_some() && clobber_idx.is_some(),
        "expected place value and clobber moves"
    );
    assert!(
        place_idx.unwrap() < clobber_idx.unwrap(),
        "place value move should occur before clobbering its base reg"
    );
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
        dst: Some(PlaceAny::Aggregate(p2.clone())),
        callee: Callee::Def(DefId(1)),
        args: vec![PlaceAny::Scalar(p1.clone())],
    }];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let target = arm64_target();
    let constraints = analyze_constraints(&body, &target);
    assert_eq!(constraints.call_constraints.len(), 1);

    let call = &constraints.call_constraints[0];
    assert_eq!(call.args.len(), 2);
    assert_eq!(call.args[0].kind, CallArgKind::Value);
    assert_eq!(call.args[1].kind, CallArgKind::Addr);
    assert_eq!(call.args[1].reg, target.indirect_result_reg().unwrap());
    assert!(call.result.is_none());
}
