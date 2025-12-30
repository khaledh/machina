use super::Pass;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::{
    Callee, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place, PlaceAny, Projection,
    Rvalue, Statement, TyId, TyKind, TyTable,
};

const INLINE_THRESHOLD: u64 = 16;

/// Lower MemSet statements to either inline stores or a runtime call.
pub struct MemOpsLower;

impl Pass for MemOpsLower {
    fn name(&self) -> &'static str {
        "memops-lower"
    }

    fn run(&mut self, body: &mut FuncBody) -> bool {
        let mut changed = false;
        for block in &mut body.blocks {
            let mut new_stmts = Vec::with_capacity(block.stmts.len());
            for stmt in block.stmts.drain(..) {
                match stmt {
                    Statement::MemSet { dst, value, len } => {
                        changed = true;
                        lower_memset(
                            &mut body.locals,
                            &mut body.types,
                            &mut new_stmts,
                            dst,
                            value,
                            len,
                        );
                    }
                    _ => new_stmts.push(stmt),
                }
            }
            block.stmts = new_stmts;
        }
        changed
    }
}

fn lower_memset(
    locals: &mut Vec<Local>,
    types: &mut TyTable,
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    value: Operand,
    len: u64,
) {
    let (elem_ty, _) = match types.kind(dst.ty()) {
        TyKind::Array { elem_ty, dims } => (*elem_ty, dims),
        other => panic!("MemSet expects array destination, got {other:?}"),
    };

    let is_u8 = matches!(
        types.kind(elem_ty),
        TyKind::Int {
            signed: false,
            bits: 8
        }
    );
    if !is_u8 {
        panic!("MemSet lowering only supports u8 arrays for now");
    }

    if len <= INLINE_THRESHOLD {
        inline_memset(out, dst, value, len, elem_ty);
    } else {
        call_runtime_memset(locals, types, out, dst, value, len, elem_ty);
    }
}

fn inline_memset(
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    value: Operand,
    len: u64,
    elem_ty: TyId,
) {
    for i in 0..len {
        let index_proj = Projection::Index {
            index: Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: i as i128,
            }),
        };
        let mut projs = dst.projections().to_vec();
        projs.push(index_proj);
        let field_place = Place::new(dst.base(), elem_ty, projs);
        out.push(Statement::CopyScalar {
            dst: field_place,
            src: Rvalue::Use(value.clone()),
        });
    }
}

fn call_runtime_memset(
    locals: &mut Vec<Local>,
    types: &mut TyTable,
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    value: Operand,
    len: u64,
    elem_ty: TyId,
) {
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });
    let slice_ty = types.add(TyKind::Struct {
        fields: vec![
            crate::mcir::StructField {
                name: "ptr".to_string(),
                ty: u64_ty,
            },
            crate::mcir::StructField {
                name: "len".to_string(),
                ty: u64_ty,
            },
        ],
    });

    let slice_local = push_temp(locals, slice_ty);
    let slice_place = Place::new(slice_local, slice_ty, vec![]);

    let ptr_field = Place::new(slice_local, u64_ty, vec![Projection::Field { index: 0 }]);
    let len_field = Place::new(slice_local, u64_ty, vec![Projection::Field { index: 1 }]);

    out.push(Statement::CopyScalar {
        dst: ptr_field,
        src: Rvalue::AddrOf(PlaceAny::Aggregate(dst.clone())),
    });
    out.push(Statement::CopyScalar {
        dst: len_field,
        src: Rvalue::Use(Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: len as i128,
        })),
    });

    let value_place = ensure_value_place(locals, out, value, elem_ty);

    out.push(Statement::Call {
        dst: None,
        callee: Callee::Runtime(RuntimeFn::MemSet),
        args: vec![
            PlaceAny::Aggregate(slice_place),
            PlaceAny::Scalar(value_place),
        ],
    });
}

fn ensure_value_place(
    locals: &mut Vec<Local>,
    out: &mut Vec<Statement>,
    value: Operand,
    elem_ty: TyId,
) -> Place<crate::mcir::Scalar> {
    match value {
        Operand::Copy(place) | Operand::Move(place) => place,
        Operand::Const(c) => {
            let tmp = push_temp(locals, elem_ty);
            let dst = Place::new(tmp, elem_ty, vec![]);
            out.push(Statement::CopyScalar {
                dst: dst.clone(),
                src: Rvalue::Use(Operand::Const(c)),
            });
            dst
        }
    }
}

fn push_temp(locals: &mut Vec<Local>, ty: TyId) -> LocalId {
    let id = LocalId(locals.len() as u32);
    locals.push(Local {
        ty,
        kind: LocalKind::Temp,
        name: None,
    });
    id
}
