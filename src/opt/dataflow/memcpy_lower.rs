use crate::lower::LoweredFunc;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::layout::size_of_ty;
use crate::mcir::{
    Callee, Const, FuncBody, Local, LocalId, LocalKind, Operand, Place, PlaceAny, Projection,
    Rvalue, Statement, TyId, TyKind, TyTable,
};

const INLINE_THRESHOLD: u64 = 16;

// Lower aggregate copies into inline byte copies or runtime __rt_memcpy calls.
pub fn run(funcs: &mut [LoweredFunc]) {
    for func in funcs {
        lower_body(&mut func.body);
    }
}

fn lower_body(body: &mut FuncBody) {
    for block in &mut body.blocks {
        let mut new_stmts = Vec::with_capacity(block.stmts.len());
        for stmt in block.stmts.drain(..) {
            match stmt {
                Statement::CopyAggregate { dst, src } => {
                    lower_memcpy(&mut body.locals, &mut body.types, &mut new_stmts, dst, src);
                }
                _ => new_stmts.push(stmt),
            }
        }
        block.stmts = new_stmts;
    }
}

fn lower_memcpy(
    locals: &mut Vec<Local>,
    types: &mut TyTable,
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    src: Place<crate::mcir::Aggregate>,
) {
    let size = size_of_ty(types, dst.ty()) as u64;
    if size == 0 {
        return;
    }

    if size <= INLINE_THRESHOLD {
        inline_memcpy(types, out, dst, src, size);
    } else {
        call_runtime_memcpy(locals, types, out, dst, src, size);
    }
}

fn inline_memcpy(
    types: &mut TyTable,
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    src: Place<crate::mcir::Aggregate>,
    len: u64,
) {
    // Copy byte-by-byte so this works for any aggregate layout.
    let u8_ty = types.add(TyKind::Int {
        signed: false,
        bits: 8,
    });

    for offset in 0..len {
        let mut dst_proj = dst.projections().to_vec();
        dst_proj.push(Projection::ByteOffset {
            offset: offset as usize,
        });
        let dst_byte = Place::new(dst.base(), u8_ty, dst_proj);

        let mut src_proj = src.projections().to_vec();
        src_proj.push(Projection::ByteOffset {
            offset: offset as usize,
        });
        let src_byte = Place::new(src.base(), u8_ty, src_proj);

        out.push(Statement::CopyScalar {
            dst: dst_byte,
            src: Rvalue::Use(Operand::Copy(src_byte)),
        });
    }
}

fn call_runtime_memcpy(
    locals: &mut Vec<Local>,
    types: &mut TyTable,
    out: &mut Vec<Statement>,
    dst: Place<crate::mcir::Aggregate>,
    src: Place<crate::mcir::Aggregate>,
    len: u64,
) {
    // Materialize ptr+len args for the runtime memcpy.
    let u64_ty = types.add(TyKind::Int {
        signed: false,
        bits: 64,
    });
    let dst_ptr_local = push_temp(locals, u64_ty);
    let src_ptr_local = push_temp(locals, u64_ty);
    let dst_ptr = Place::new(dst_ptr_local, u64_ty, vec![]);
    let src_ptr = Place::new(src_ptr_local, u64_ty, vec![]);

    let len_const = Operand::Const(Const::Int {
        signed: false,
        bits: 64,
        value: len as i128,
    });

    out.push(Statement::CopyScalar {
        dst: dst_ptr.clone(),
        src: Rvalue::AddrOf(PlaceAny::Aggregate(dst.clone())),
    });
    out.push(Statement::CopyScalar {
        dst: src_ptr.clone(),
        src: Rvalue::AddrOf(PlaceAny::Aggregate(src.clone())),
    });

    let len_place = ensure_value_place(locals, out, len_const, u64_ty);

    out.push(Statement::Call {
        dst: None,
        callee: Callee::Runtime(RuntimeFn::MemCopy),
        args: vec![
            PlaceAny::Scalar(dst_ptr),
            PlaceAny::Scalar(src_ptr),
            PlaceAny::Scalar(len_place),
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
