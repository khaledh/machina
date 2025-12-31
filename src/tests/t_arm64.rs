use indoc::indoc;

use std::collections::HashMap;

use super::{FuncCodegen, McFunction};
use crate::mcir::types::*;
use crate::regalloc::moves::FnMoveList;
use crate::regalloc::stack::StackSlotId;
use crate::regalloc::{AllocationResult, MappedLocal};
use crate::resolve::def_map::DefId;
use crate::targets::arm64::codegen::Arm64Codegen;
use crate::targets::arm64::regs::{self, Arm64Reg as R};

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
    let global_labels = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

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
    let global_labels = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

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
        stmts: vec![Statement::CopyScalar {
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
    let alloc = mk_alloc(vec![(ret, MappedLocal::Reg(regs::phys(R::X0)))]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let global_labels = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

    let asm = codegen.generate().unwrap();
    let direct = asm.contains("  mov x0, #42\n") || asm.contains("  movz x0, #42, lsl #0\n");
    let via_scratch = (asm.contains("  mov x16, #42\n")
        || asm.contains("  movz x16, #42, lsl #0\n"))
        && asm.contains("  mov x0, x16\n");
    assert!(
        direct || via_scratch,
        "expected return reg to be loaded with 42"
    );
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
            dst: Some(PlaceAny::Scalar(Place::new(ret, u64_ty, vec![]))),
            callee: Callee::Def(DefId(7)),
            args: Vec::new(),
        }],
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, ret);
    let alloc = mk_alloc(vec![(ret, MappedLocal::Reg(regs::phys(R::X0)))]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let mut def_names = HashMap::new();
    def_names.insert(DefId(7), "callee".to_string());
    let global_labels = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

    let asm = codegen.generate().unwrap();
    assert!(asm.contains("  bl _callee\n"));
}

#[test]
fn test_enum_payload_stores_to_return_slot() {
    let mut types = TyTable::new();
    let u64_ty = u64_ty(&mut types);
    let bool_ty = types.add(TyKind::Bool);
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });
    let payload_blob_ty = types.add(TyKind::Array {
        elem_ty: u8_ty,
        dims: vec![9],
    });
    let enum_ty = types.add(TyKind::Tuple {
        field_tys: vec![u64_ty, payload_blob_ty],
    });

    let locals = vec![Local {
        ty: enum_ty,
        kind: LocalKind::Return,
        name: Some("ret".to_string()),
    }];
    let ret = LocalId(0);
    let blocks = vec![BasicBlock {
        stmts: vec![
            Statement::CopyScalar {
                dst: Place::new(ret, u64_ty, vec![Projection::Field { index: 0 }]),
                src: Rvalue::Use(Operand::Const(Const::Int {
                    value: 1,
                    signed: false,
                    bits: 64,
                })),
            },
            Statement::CopyScalar {
                dst: Place::new(
                    ret,
                    u64_ty,
                    vec![
                        Projection::Field { index: 1 },
                        Projection::ByteOffset { offset: 0 },
                    ],
                ),
                src: Rvalue::Use(Operand::Const(Const::Int {
                    value: 7,
                    signed: false,
                    bits: 64,
                })),
            },
            Statement::CopyScalar {
                dst: Place::new(
                    ret,
                    bool_ty,
                    vec![
                        Projection::Field { index: 1 },
                        Projection::ByteOffset { offset: 8 },
                    ],
                ),
                src: Rvalue::Use(Operand::Const(Const::Bool(true))),
            },
        ],
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, ret);

    let mut alloc_map = HashMap::new();
    alloc_map.insert(ret, MappedLocal::StackAddr(StackSlotId(2)));
    let alloc = AllocationResult {
        alloc_map,
        moves: FnMoveList::new(),
        frame_size: 24,
        used_callee_saved: Vec::new(),
        stack_slot_count: 3,
    };
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let global_labels = HashMap::new();
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

    let asm = codegen.generate().unwrap();
    assert_eq!(asm.matches("\n  str ").count(), 2);
    assert_eq!(asm.matches("\n  strb ").count(), 1);
}

#[test]
fn test_emit_rodata_globals() {
    let globals = vec![GlobalItem {
        id: GlobalId(0),
        kind: GlobalSection::RoData,
        payload: GlobalPayload::String("ab".to_string()),
    }];

    let funcs = Vec::new();
    let def_names = HashMap::new();
    let mut codegen = Arm64Codegen::new(funcs, &def_names, &globals);
    let asm = codegen.generate().unwrap();

    assert!(asm.contains(".section __TEXT,__const"));
    assert!(asm.contains(".G0:"));
    assert!(asm.contains(".ascii \"ab\""));
}

#[test]
fn test_global_addr_materialization() {
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
        stmts: vec![Statement::CopyScalar {
            dst: ret_place,
            src: Rvalue::Use(Operand::Const(Const::GlobalAddr { id: GlobalId(0) })),
        }],
        terminator: Terminator::Return,
    }];
    let body = mk_body(types, locals, blocks, ret);
    let alloc = mk_alloc(vec![(ret, MappedLocal::Reg(regs::phys(R::X0)))]);
    let func = McFunction {
        name: "test".to_string(),
        body: &body,
        alloc: &alloc,
    };
    let def_names = HashMap::new();
    let mut global_labels = HashMap::new();
    global_labels.insert(GlobalId(0), ".G0".to_string());
    let mut codegen = FuncCodegen::new(&func, &def_names, &global_labels, 0).unwrap();

    let asm = codegen.generate().unwrap();
    assert!(
        asm.contains(".G0@PAGE"),
        "expected adrp/add to reference global label"
    );
    assert!(
        asm.contains(".G0@PAGEOFF"),
        "expected pageoff add to reference global label"
    );
}
