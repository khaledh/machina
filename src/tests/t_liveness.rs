use crate::liveness::LivenessAnalysis;
use crate::mcir::types::{
    BasicBlock, BlockId, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place, Rvalue,
    Statement, Terminator, TyKind, TyTable,
};

fn u64_ty(types: &mut TyTable) -> crate::mcir::types::TyId {
    types.add(TyKind::Int {
        signed: false,
        bits: 64,
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

#[test]
fn test_liveness_basic() {
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
            name: Some("x".to_string()),
        },
    ];

    let l0 = LocalId(0);
    let l1 = LocalId(1);
    let ret_place = Place::new(l0, u64_ty, vec![]);
    let x_place = Place::new(l1, u64_ty, vec![]);

    let stmts = vec![
        Statement::CopyScalar {
            dst: x_place.clone(),
            src: Rvalue::Use(Operand::Const(Const::Int {
                value: 1,
                signed: false,
                bits: 64,
            })),
        },
        Statement::CopyScalar {
            dst: ret_place,
            src: Rvalue::Use(Operand::Copy(x_place)),
        },
    ];

    let blocks = vec![BasicBlock {
        stmts,
        terminator: Terminator::Return,
    }];

    let body = mk_body(types, locals, blocks, l0);
    let live_map = LivenessAnalysis::new(&body).analyze();

    assert_eq!(live_map.len(), 1);
    let live = &live_map[0];
    assert!(live.live_in.is_empty());
    assert!(live.live_out.is_empty());
}
