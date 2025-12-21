use indoc::indoc;

use std::collections::HashMap;

use crate::codegen::arm64::{FuncCodegen, McFunction};
use crate::ids::DefId;
use crate::mcir::types::{
    BasicBlock, BlockId, Callee, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place,
    Rvalue, Statement, Terminator, TyId, TyKind, TyTable,
};
use crate::regalloc::moves::FnMoveList;
use crate::regalloc::regs::Arm64Reg as R;
use crate::regalloc::{AllocationResult, MappedLocal};

fn u64_ty(types: &mut TyTable) -> TyId {
    types.add(TyKind::Int {
        signed: false,
        bits: 64,
    })
}

fn mk_body(types: TyTable, locals: Vec<Local>, blocks: Vec<BasicBlock>, ret: LocalId) -> FuncBody {
    FuncBody {
        locals,
        blocks,
        entry: BlockId(0),
        ret_local: ret,
        types,
    }
}

fn mk_alloc(alloc_map: Vec<(LocalId, MappedLocal)>) -> AllocationResult {
    AllocationResult {
        alloc_map: alloc_map.into_iter().collect(),
        moves: FnMoveList::new(),
        frame_size: 0,
        used_callee_saved: Vec::new(),
        stack_slot_count: 0,
    }
}

#[test]
fn test_prologue_no_frame() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];
    let blocks = vec![BasicBlock {
        stmts: Vec::new(),
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, LocalId(0));
    let alloc = mk_alloc(vec![]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, 0).unwrap();

    let prologue = codegen.emit_prologue().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
    "};
    assert_eq!(prologue, expected);
}

#[test]
fn test_epilogue_no_frame() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];
    let blocks = vec![BasicBlock {
        stmts: Vec::new(),
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, LocalId(0));
    let alloc = mk_alloc(vec![]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, 0).unwrap();

    let epilogue = codegen.emit_epilogue().unwrap();

    let expected = "  ldp x29, x30, [sp], #16\n  ret\n";
    assert_eq!(epilogue, expected);
}

#[test]
fn test_assign_const_to_return_reg() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];
    let ret = LocalId(0);
    let ret_place = Place::new(ret, u64_ty, vec![]);
    let blocks = vec![BasicBlock {
        stmts: vec![Statement::AssignScalar {
            dst: ret_place,
            src: Rvalue::Use(Operand::Const(Const::Int {
                value: 42,
                signed: false,
                bits: 64,
            })),
        }],
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, ret);
    let alloc = mk_alloc(vec![(ret, MappedLocal::Reg(R::X0))]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, 0).unwrap();

    let asm = codegen.generate().unwrap();
    assert!(asm.contains("  mov x16, #42\n"));
    assert!(asm.contains("  mov x0, x16\n"));
    assert!(asm.contains("  ret\n"));
}

#[test]
fn test_call_emits_bl() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let locals = vec![Local {
        ty: u64_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];
    let ret = LocalId(0);
    let blocks = vec![BasicBlock {
        stmts: vec![Statement::Call {
            dst: crate::mcir::types::PlaceAny::Scalar(Place::new(ret, u64_ty, vec![])),
            callee: Callee::Def(DefId(7)),
            args: Vec::new(),
        }],
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, ret);
    let alloc = mk_alloc(vec![(ret, MappedLocal::Reg(R::X0))]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let mut def_names = HashMap::new();
    def_names.insert(DefId(7), "callee".to_string());
    let mut codegen = FuncCodegen::new(&func, &def_names, 0).unwrap();

    let asm = codegen.generate().unwrap();
    assert!(asm.contains("  bl _callee\n"));
}
