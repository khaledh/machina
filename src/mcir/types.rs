use std::fmt;
use std::fmt::Write as _;
use std::marker::PhantomData;

use crate::resolve::def_map::DefId;

// --- IR Type System ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(pub u32);

impl TyId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    // Scalar Types
    Unit,
    Bool,
    Int { signed: bool, bits: u8 },

    // Aggregate Types
    Array { elem_ty: TyId, dims: Vec<usize> },
    Tuple { field_tys: Vec<TyId> },
    Struct { fields: Vec<StructField> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: TyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyInfo {
    pub kind: TyKind,
}

impl TyInfo {
    pub fn is_scalar(&self) -> bool {
        matches!(self.kind, TyKind::Unit | TyKind::Bool | TyKind::Int { .. })
    }

    pub fn is_aggregate(&self) -> bool {
        matches!(
            self.kind,
            TyKind::Array { .. } | TyKind::Tuple { .. } | TyKind::Struct { .. }
        )
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TyTable {
    types: Vec<TyInfo>,
}

impl TyTable {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }

    pub fn add(&mut self, kind: TyKind) -> TyId {
        let id = TyId(self.types.len() as u32);
        self.types.push(TyInfo { kind });
        id
    }

    pub fn get(&self, id: TyId) -> &TyInfo {
        &self.types[id.index()]
    }

    pub fn kind(&self, id: TyId) -> &TyKind {
        &self.get(id).kind
    }

    pub fn type_to_string(&self, id: TyId) -> String {
        let mut out = String::new();
        let _ = self.write_ty(id, &mut out);
        out
    }

    fn write_ty(&self, id: TyId, out: &mut String) -> fmt::Result {
        match self.kind(id) {
            TyKind::Unit => write!(out, "()"),
            TyKind::Bool => write!(out, "bool"),
            TyKind::Int { signed, bits } => {
                let prefix = if *signed { "i" } else { "u" };
                write!(out, "{}{}", prefix, bits)
            }
            TyKind::Array { elem_ty, dims } => {
                self.write_ty(*elem_ty, out)?;
                write!(out, "[")?;
                for (i, dim) in dims.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "{}", dim)?;
                }
                write!(out, "]")
            }
            TyKind::Tuple { field_tys } => {
                write!(out, "(")?;
                for (i, ty) in field_tys.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    self.write_ty(*ty, out)?;
                }
                write!(out, ")")
            }
            TyKind::Struct { fields } => {
                write!(out, "struct {{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "{}: ", field.name)?;
                    self.write_ty(field.ty, out)?;
                }
                write!(out, " }}")
            }
        }
    }
}

// --- IR Constructs ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

impl LocalId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl BlockId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Temp,
    Param { index: u32 },
    Return,
    User,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    pub ty: TyId,
    pub kind: LocalKind,
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Unit,
    Bool(bool),
    Int { value: i128, signed: bool, bits: u8 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scalar {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aggregate {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Projection {
    Field { index: usize },
    Index { index: Operand },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place<K> {
    base: LocalId,
    proj: Vec<Projection>,
    ty: TyId,
    _k: PhantomData<K>,
}

impl<K> Place<K> {
    pub(crate) fn new(base: LocalId, ty: TyId, proj: Vec<Projection>) -> Self {
        Self {
            base,
            proj,
            ty,
            _k: PhantomData,
        }
    }

    pub fn base(&self) -> LocalId {
        self.base
    }

    pub fn projections(&self) -> &[Projection] {
        &self.proj
    }

    pub fn ty(&self) -> TyId {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceAny {
    Scalar(Place<Scalar>),
    Aggregate(Place<Aggregate>),
}

impl From<Place<Scalar>> for PlaceAny {
    fn from(place: Place<Scalar>) -> Self {
        Self::Scalar(place)
    }
}

impl From<Place<Aggregate>> for PlaceAny {
    fn from(place: Place<Aggregate>) -> Self {
        Self::Aggregate(place)
    }
}

impl PlaceAny {
    pub fn ty(&self) -> TyId {
        match self {
            PlaceAny::Scalar(place) => place.ty(),
            PlaceAny::Aggregate(place) => place.ty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Copy(Place<Scalar>),
    #[allow(dead_code)]
    Move(Place<Scalar>),
    Const(Const),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rvalue {
    Use(Operand),
    BinOp {
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
    },
    UnOp {
        op: UnOp,
        arg: Operand,
    },
    #[allow(dead_code)]
    AddrOf(PlaceAny),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    CopyScalar {
        dst: Place<Scalar>,
        src: Rvalue,
    },
    #[allow(dead_code)]
    InitAggregate {
        dst: Place<Aggregate>,
        fields: Vec<Operand>,
    },
    CopyAggregate {
        dst: Place<Aggregate>,
        src: Place<Aggregate>,
    },
    Call {
        dst: PlaceAny,
        callee: Callee,
        args: Vec<PlaceAny>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Callee {
    Def(DefId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Return,
    Goto(BlockId),
    If {
        cond: Operand,
        then_bb: BlockId,
        else_bb: BlockId,
    },
    Unterminated,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub stmts: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncBody {
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    pub entry: BlockId,
    pub ret_local: LocalId,
    pub types: TyTable,
}

impl fmt::Display for TyId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl fmt::Display for LocalKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LocalKind::Temp => write!(f, "temp"),
            LocalKind::Param { index } => write!(f, "param#{}", index),
            LocalKind::Return => write!(f, "return"),
            LocalKind::User => write!(f, "user"),
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Unit => write!(f, "()"),
            Const::Bool(value) => write!(f, "{}", value),
            Const::Int { value, .. } => write!(f, "{}", value),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::LtEq => "<=",
            BinOp::GtEq => ">=",
        };
        write!(f, "{}", op)
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            UnOp::Neg => "-",
        };
        write!(f, "{}", op)
    }
}

impl fmt::Display for Projection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Projection::Field { index } => write!(f, ".{}", index),
            Projection::Index { index } => write!(f, "[{}]", index),
        }
    }
}

impl<K> fmt::Display for Place<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for proj in &self.proj {
            write!(f, "{}", proj)?;
        }
        Ok(())
    }
}

impl fmt::Display for PlaceAny {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlaceAny::Scalar(place) => write!(f, "{}", place),
            PlaceAny::Aggregate(place) => write!(f, "{}", place),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Copy(place) => write!(f, "{}", place),
            Operand::Move(place) => write!(f, "move {}", place),
            Operand::Const(value) => write!(f, "{}", value),
        }
    }
}

impl fmt::Display for Rvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rvalue::Use(op) => write!(f, "{}", op),
            Rvalue::BinOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Rvalue::UnOp { op, arg } => write!(f, "({}{})", op, arg),
            Rvalue::AddrOf(place) => write!(f, "&{}", place),
        }
    }
}

impl fmt::Display for Callee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Callee::Def(def_id) => write!(f, "def#{}", def_id),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::CopyScalar { dst, src } => write!(f, "{} = {}", dst, src),
            Statement::InitAggregate { dst, fields } => {
                write!(f, "init {} = [", dst)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "]")
            }
            Statement::CopyAggregate { dst, src } => write!(f, "{} = copy {}", dst, src),
            Statement::Call { dst, callee, args } => {
                write!(f, "{} = call {}(", dst, callee)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return => write!(f, "return"),
            Terminator::Goto(target) => write!(f, "goto {}", target),
            Terminator::If {
                cond,
                then_bb,
                else_bb,
            } => write!(f, "if {} goto {} else {}", cond, then_bb, else_bb),
            Terminator::Unterminated => write!(f, "unterminated"),
        }
    }
}

impl fmt::Display for FuncBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "body {{")?;
        writeln!(f, "  entry: {}", self.entry)?;
        writeln!(f, "  ret: {}", self.ret_local)?;
        writeln!(f, "  locals:")?;
        if self.locals.is_empty() {
            writeln!(f, "    <none>")?;
        } else {
            for (i, local) in self.locals.iter().enumerate() {
                let local_id = LocalId(i as u32);
                let ty_str = self.types.type_to_string(local.ty);
                match &local.name {
                    Some(name) => writeln!(
                        f,
                        "    {} ({}, name={}): {}",
                        local_id, local.kind, name, ty_str
                    )?,
                    None => writeln!(f, "    {} ({}): {}", local_id, local.kind, ty_str)?,
                };
            }
        }
        writeln!(f, "  blocks:")?;
        if self.blocks.is_empty() {
            writeln!(f, "    <none>")?;
        } else {
            for (i, block) in self.blocks.iter().enumerate() {
                let bb = BlockId(i as u32);
                writeln!(f, "    {}:", bb)?;
                for stmt in &block.stmts {
                    writeln!(f, "      {}", stmt)?;
                }
                writeln!(f, "      {}", block.terminator)?;
            }
        }
        write!(f, "}}")
    }
}
