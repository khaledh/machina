use crate::mcir::types::*;

#[derive(Debug)]
pub struct FuncBuilder {
    pub body: FuncBody,
}

impl FuncBuilder {
    pub fn new(ret_ty: TyId) -> Self {
        let mut locals = Vec::new();
        let ret_local = LocalId(locals.len() as u32);
        locals.push(Local {
            ty: ret_ty,
            kind: LocalKind::Return,
            name: Some("ret".to_string()),
        });

        let entry = BlockId(0);
        let blocks = vec![BasicBlock {
            stmts: Vec::new(),
            terminator: Terminator::Unterminated, // temporary; override when done
        }];

        Self {
            body: FuncBody {
                locals,
                blocks,
                entry,
                ret_local,
                types: TyTable::new(),
            },
        }
    }

    pub fn new_local(&mut self, ty: TyId, kind: LocalKind, name: Option<String>) -> LocalId {
        let id = LocalId(self.body.locals.len() as u32);
        self.body.locals.push(Local { ty, kind, name });
        id
    }

    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.body.blocks.len() as u32);
        self.body.blocks.push(BasicBlock {
            stmts: Vec::new(),
            terminator: Terminator::Unterminated, // temporary; override when done
        });
        id
    }

    pub fn push_comment(&mut self, bb: BlockId, comment: String) {
        self.body.blocks[bb.index()]
            .stmts
            .push(Statement::Comment(comment));
    }

    pub fn push_stmt(&mut self, bb: BlockId, stmt: Statement) {
        self.body.blocks[bb.index()].stmts.push(stmt);
    }

    pub fn set_terminator(&mut self, bb: BlockId, term: Terminator) {
        self.body.blocks[bb.index()].terminator = term;
    }
}
