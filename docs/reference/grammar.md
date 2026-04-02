# Grammar

This is a user-facing BNF-style reference for current Machina syntax.

## Notation

- `::=` means "is defined as"
- `|` separates alternatives
- `*` means zero or more
- `?` means optional (zero or one)

## Modules

```bnf
Module             ::= RequiresBlock? TopLevelItem*

RequiresBlock      ::= "requires" "{" RequireItem* "}"
RequireItem        ::= RequirePath ("as" Identifier)? ("," | ";")?
RequirePath        ::= Identifier ("::" Identifier)*
```

## Top-Level Declarations

```bnf
TopLevelItem       ::= (AttributeList (TypeDef | TraitDef | StaticDef | FuncDecl | FuncDef))
                     | MethodBlock
                     | MachineDef

AttributeList      ::= ("@" Attribute)*
Attribute          ::= Identifier ("(" AttributeArgList? ")")?
AttributeArgList   ::= AttributeArg ("," AttributeArg)* ","?
AttributeArg       ::= StringLit
                     | IntLit
                     | Identifier
                     | Identifier ":" AttributeArgValue
AttributeArgValue  ::= StringLit | IntLit | Identifier

TypeDef            ::= "type" Identifier TypeParamList? "=" TypeDefBody
TypeDefBody        ::= TypeExpr ";"?
                     | StructDef
                     | EnumDef
                     | LinearTypeDef

StructDef          ::= "{" StructFieldList? "}"
StructFieldList    ::= StructField ("," StructField)* ","?
StructField        ::= AttributeList Identifier ":" TypeExpr

EnumDef            ::= EnumVariantDef ("|" EnumVariantDef)* ";"?
EnumVariantDef     ::= Identifier ("(" TypeExprList ")")?

StaticDef          ::= "static" ("let" | "var") Identifier (":" TypeExpr)? "=" Expr ";"?

TraitDef           ::= "trait" Identifier "{" TraitItem* "}"
TraitItem          ::= MethodSig ";" | TraitPropertyDecl
TraitPropertyDecl  ::= "prop" Identifier ":" TypeExpr "{" TraitAccessorDecl* "}"
TraitAccessorDecl  ::= "get" ";" | "set" ";"

MachineDef         ::= "machine" Identifier "hosts" Identifier "(" "key" ":" Identifier ")" "{" MachineItem* "}"
```

## Type Expressions

```bnf
TypeExpr           ::= TypeExprTerm ("|" TypeExprTerm)*
TypeExprTerm       ::= TypeExprAtom NullableSuffix? RefinementType? TypeSuffix*
TypeExprAtom       ::= UnitType | NamedType | TupleType | RangeType | FnType | RawPtrType

TypeSuffix         ::= ArraySuffix | SliceSuffix | HeapSuffix
ArraySuffix        ::= "[" ExtentList "]"
SliceSuffix        ::= "[" "]"
HeapSuffix         ::= "^"
NullableSuffix     ::= "?"      # currently accepted for paddr, vaddr, and view<...>

RefinementType     ::= ":" Refinement ("&" Refinement)*
Refinement         ::= "bounds" "(" IntLit ("," IntLit)? ")"
                     | "nonzero"

UnitType           ::= "()"
NamedType          ::= TypeName TypeArgList?
TypeName           ::= Identifier ("::" Identifier)* | "set" | "map"
TupleType          ::= "(" TypeExpr "," TypeExprList? ")"
RangeType          ::= "range" "(" IntLit ("," IntLit)? ")"
FnType             ::= "fn" "(" FnTypeParamList? ")" "->" TypeExpr
RawPtrType         ::= "*" TypeExprTerm

TypeParamList      ::= "<" IdentifierList ">"
TypeArgList        ::= "<" TypeExprList ">"
IdentifierList     ::= Identifier ("," Identifier)* ","?

FnTypeParamList    ::= FnTypeParam ("," FnTypeParam)* ","?
FnTypeParam        ::= ParamMode? TypeExpr

TypeExprList       ::= TypeExpr ("," TypeExpr)* ","?
ExtentList         ::= ExtentExpr ("," ExtentExpr)* ","?
ExtentExpr         ::= IntLit | FieldPath
FieldPath          ::= Ident ("." Ident)*
```

## Functions, Methods, and Properties

```bnf
FuncDecl           ::= FuncSig ";"
FuncDef            ::= FuncSig Block

FuncSig            ::= "fn" Identifier TypeParamList? "(" ParamList? ")" ("->" TypeExpr)?

ParamList          ::= Param ("," Param)* ","?
Param              ::= ParamMode? Identifier ":" TypeExpr ("=" Expr)?
ParamMode          ::= "inout" | "out" | "sink"

MethodBlock        ::= TypeName TypeArgList? "::" TypeName? "{" MethodItem* "}"
MethodItem         ::= MethodDecl | MethodDef | PropertyDef
MethodDecl         ::= AttributeList MethodSig ";"
MethodDef          ::= AttributeList MethodSig Block
MethodSig          ::= "fn" Identifier TypeParamList? "(" SelfParam ("," ParamList)? ")" ("->" TypeExpr)?
SelfParam          ::= ParamMode? "self"

PropertyDef        ::= "prop" Identifier ":" TypeExpr "{" PropertyAccessor* "}"
PropertyAccessor   ::= "get" Block | "set" "(" Identifier ")" Block

ClosureExpr        ::= ClosureCaptureList? ClosureSig Expr
ClosureCaptureList ::= "[" "move" Identifier ("," Identifier)* ","? "]"
ClosureSig         ::= "|" ParamList? "|" ("->" TypeExpr)?
```

## Statements and Patterns

```bnf
Block              ::= "{" BlockItem* (Expr ";"?)? "}"
BlockItem          ::= StmtExpr | Expr ";"

StmtExpr           ::= LetBind | VarBind | VarDecl | Assign
                     | While | For | Using | Defer | Break | Continue | Return

LetBind            ::= "let" Pattern (":" TypeExpr)? "=" Expr ";"
VarBind            ::= "var" Pattern (":" TypeExpr)? "=" Expr ";"
VarDecl            ::= "var" Identifier ":" TypeExpr ";"

Assign             ::= PostfixExpr AssignOp Expr ";"
AssignOp           ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="
                     | "&=" | "|=" | "^=" | "<<=" | ">>="

While              ::= "while" Expr Block
For                ::= "for" Pattern "in" Expr Block
Using              ::= "using" Identifier "=" Expr Block
Defer              ::= "defer" Expr ";"
Break              ::= "break" ";"
Continue           ::= "continue" ";"
Return             ::= "return" Expr? ";"

Pattern            ::= IdentPattern | ArrayPattern | TuplePattern | StructPattern
IdentPattern       ::= Identifier
ArrayPattern       ::= "[" PatternList "]"
TuplePattern       ::= "(" Pattern "," PatternList? ")"
StructPattern      ::= Identifier "{" StructPatternFieldList? "}"

StructPatternFieldList ::= StructPatternField ("," StructPatternField)* ","?
StructPatternField ::= Identifier (":" Pattern)?
PatternList        ::= Pattern ("," Pattern)* ","?
```

## Expressions

```bnf
Expr               ::= If | Match | TryExpr
TryExpr            ::= TernaryExpr ("or" (Expr | OrHandlerBlock))?
OrHandlerBlock     ::= "{" (MatchArmList | BlockBody)? "}"
MatchArmList       ::= MatchArm ("," MatchArm)* ","?
BlockBody          ::= BlockItem* (Expr ";"?)?
TernaryExpr        ::= InfixExpr ("?" Expr ":" TernaryExpr)?

If                 ::= "if" Expr Block IfTail?
IfTail             ::= "else" (Block | If)

Match              ::= "match" Expr "{" MatchArm ("," MatchArm)* ","? "}"
MatchArm           ::= MatchPattern "=>" Expr
MatchPattern       ::= "_" | BoolLit | IntLit | TypedMatchBinding
                     | TuplePattern | EnumVariantPattern
TypedMatchBinding  ::= Identifier ":" TypeExpr
EnumVariantPattern ::= Identifier ("::" Identifier)? ("(" MatchBindingList? ")")?
MatchBindingList   ::= MatchBinding ("," MatchBinding)* ","?
MatchBinding       ::= Identifier | "_"
```

## Operators and Postfix Forms

```bnf
InfixExpr          ::= PipeExpr
PipeExpr           ::= OrExpr ("|>" PostfixExpr)*
OrExpr             ::= AndExpr ("||" AndExpr)*
AndExpr            ::= BitOrExpr ("&&" BitOrExpr)*
BitOrExpr          ::= BitXorExpr ("|" BitXorExpr)*
BitXorExpr         ::= BitAndExpr ("^" BitAndExpr)*
BitAndExpr         ::= CompareExpr ("&" CompareExpr)*
CompareExpr        ::= ShiftExpr (("==" | "!=" | "<" | "<=" | ">" | ">=") ShiftExpr)*
ShiftExpr          ::= AddExpr (("<<" | ">>") AddExpr)*
AddExpr            ::= MulExpr (("+" | "-") MulExpr)*
MulExpr            ::= UnaryExpr (("*" | "/" | "%") UnaryExpr)*
UnaryExpr          ::= ("-" | "!" | "~" | "^") UnaryExpr
                     | "move" UnaryExpr
                     | PostfixExpr

PostfixExpr        ::= Primary (Call | LabeledCall | ArrayIndex | SliceRange | TupleField | StructField | MethodCall)* ("?")?
Call               ::= "(" CallArgList? ")"
LabeledCall        ::= ":" Identifier "(" CallArgList? ")"
CallArgList        ::= CallArg ("," CallArg)* ","?
CallArg            ::= ("inout" | "out" | "move")? Expr
ArrayIndex         ::= "[" ExprList "]"
SliceRange         ::= "[" Expr? ".." Expr? "]"
TupleField         ::= "." IntLit
StructField        ::= "." Identifier
MethodCall         ::= "." Identifier "(" CallArgList? ")"
```

## Primary Expressions and Literals

```bnf
Primary            ::= Literal
                     | Identifier
                     | EnumVariant
                     | StructUpdate
                     | EmitExpr
                     | ReplyExpr
                     | UnsafeExpr
                     | "(" Expr ")"
                     | Block
                     | ClosureExpr

EmitExpr           ::= "emit" ("Send" | ("Request" (":" Identifier)?)) "(" "to" ":" Expr "," Expr ")"
ReplyExpr          ::= "reply" "(" Expr "," Expr ")"
UnsafeExpr         ::= "unsafe" Block

EnumVariant        ::= TypeName "::" Identifier ("(" ExprList ")")?
StructUpdate       ::= "{" Expr "|" StructUpdateField ("," StructUpdateField)* ","? "}"
StructUpdateField  ::= Identifier (":" Expr)?

Literal            ::= UnitLit | NoneLit | IntLit | BoolLit | CharLit | StringLit | StringFmt
                     | ArrayLit | SetLit | MapLit | TupleLit | StructLit

ArrayLit           ::= TypeExpr? "[" ExprList "]"
                     | TypeExpr? "[" Expr ";" IntLit "]"
TupleLit           ::= "(" Expr "," ExprList? ")"
StructLit          ::= TypeName "{" StructLitFieldList? "}"
StructLitFieldList ::= StructLitField ("," StructLitField)* ","?
StructLitField     ::= Identifier (":" Expr)?

SetLit             ::= "{" Expr "," ExprList? "}"
                     | "set" "<" TypeExpr ">" "{" ExprList? "}"
MapLit             ::= "{" MapEntry ("," MapEntry)* ","? "}"
                     | "map" "<" TypeExpr "," TypeExpr ">" "{" MapEntryList? "}"
```

## Lexical

```bnf
Identifier         ::= [a-zA-Z_][a-zA-Z0-9_]*
ExprList           ::= Expr ("," Expr)* ","?
MapEntry           ::= Expr ":" Expr
MapEntryList       ::= MapEntry ("," MapEntry)* ","?

UnitLit            ::= "()"
NoneLit            ::= "None"
BoolLit            ::= "true" | "false"
IntLit             ::= DecimalLit | BinaryLit | OctalLit | HexLit
DecimalLit         ::= [0-9]([0-9_])*
BinaryLit          ::= "0b" [01_]+
OctalLit           ::= "0o" [0-7_]+
HexLit             ::= "0x" [0-9a-fA-F_]+
CharLit            ::= "'" (char | escape) "'"
StringLit          ::= '"' (string-char | escape)* '"'
StringFmt          ::= "f" StringLit
```

## Notes

- Bare set literals require a comma for single-element cases (for example:
  `{1,}`), so `{x}` remains a block expression.
- Bare empty map/set literals are not allowed; use typed forms such as
  `map<u64, u64>{}` or `set<u64>{}`.
- `expr?` is still the bare propagation form for error unions.
- `expr or { ... }` is inline recovery sugar; `expr or |err| { ... }` remains
  the callable-handler form.
- `?` in type position is currently accepted for low-level nullable forms such
  as `paddr?`, `vaddr?`, and `view<...>?`; it is not yet a general optional
  type feature.
