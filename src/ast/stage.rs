use crate::resolve::def::DefId;

/// Stage markers for tree-shaped AST/HIR/typed variants.
pub trait TreeStage {
    type Def;
    type Ty;
}

pub struct AstStage;
pub struct HirStage;
pub struct TypedStage;

impl TreeStage for AstStage {
    type Def = ();
    type Ty = ();
}

impl TreeStage for HirStage {
    type Def = DefId;
    type Ty = ();
}

impl TreeStage for TypedStage {
    type Def = DefId;
    type Ty = ();
}

pub type AstDef = <AstStage as TreeStage>::Def;
pub type HirDef = <HirStage as TreeStage>::Def;
pub type TypedDef = <TypedStage as TreeStage>::Def;
pub type AstTy = <AstStage as TreeStage>::Ty;
pub type HirTy = <HirStage as TreeStage>::Ty;
pub type TypedTy = <TypedStage as TreeStage>::Ty;
