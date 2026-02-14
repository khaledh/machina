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
TopLevelItem       ::= (AttributeList (TypeDef | TraitDef | FuncDecl | FuncDef))
                     | MethodBlock
                     | TypestateDef

AttributeList      ::= ("@[" AttributeItem ("," AttributeItem)* ","? "]")?
AttributeItem      ::= Identifier ("(" StringLit ("," StringLit)* ")")?

TypeDef            ::= "type" Identifier TypeParamList? "=" TypeDefBody
TypeDefBody        ::= TypeExpr ";"?
                     | StructDef
                     | EnumDef

StructDef          ::= "{" StructFieldList? "}"
StructFieldList    ::= StructField ("," StructField)* ","?
StructField        ::= Identifier ":" TypeExpr

EnumDef            ::= EnumVariantDef ("|" EnumVariantDef)* ";"?
EnumVariantDef     ::= Identifier ("(" TypeExprList ")")?

TraitDef           ::= "trait" Identifier "{" TraitItem* "}"
TraitItem          ::= MethodSig ";" | TraitPropertyDecl
TraitPropertyDecl  ::= "prop" Identifier ":" TypeExpr "{" TraitAccessorDecl* "}"
TraitAccessorDecl  ::= "get" ";" | "set" ";"

TypestateDef       ::= "typestate" Identifier "{" TypestateItem* "}"
TypestateItem      ::= TypestateFields | TypestateNew | TypestateState
TypestateFields    ::= "fields" "{" StructFieldList? "}"
TypestateNew       ::= Func
TypestateState     ::= "state" Identifier "{" TypestateStateItem* "}"
TypestateStateItem ::= TypestateStateFields | Func
TypestateStateFields ::= "fields" "{" StructFieldList? "}"
```

## Type Expressions

```bnf
TypeExpr           ::= TypeExprTerm ("|" TypeExprTerm)*
TypeExprTerm       ::= TypeExprAtom RefinementType? TypeSuffix*
TypeExprAtom       ::= UnitType | NamedType | TupleType | FnType

TypeSuffix         ::= ArraySuffix | SliceSuffix | DynArraySuffix | HeapSuffix
ArraySuffix        ::= "[" IntLitList "]"
SliceSuffix        ::= "[" "]"
DynArraySuffix     ::= "[" "*" "]"
HeapSuffix         ::= "^"

RefinementType     ::= ":" Refinement ("&" Refinement)*
Refinement         ::= "bounds" "(" IntLit ("," IntLit)? ")"
                     | "nonzero"

UnitType           ::= "()"
NamedType          ::= TypeName TypeArgList?
TypeName           ::= Identifier ("::" Identifier)* | "set" | "map"
TupleType          ::= "(" TypeExpr "," TypeExprList? ")"
FnType             ::= "fn" "(" FnTypeParamList? ")" "->" TypeExpr

TypeParamList      ::= "<" IdentifierList ">"
TypeArgList        ::= "<" TypeExprList ">"
IdentifierList     ::= Identifier ("," Identifier)* ","?

FnTypeParamList    ::= FnTypeParam ("," FnTypeParam)* ","?
FnTypeParam        ::= ParamMode? TypeExpr

TypeExprList       ::= TypeExpr ("," TypeExpr)* ","?
IntLitList         ::= IntLit ("," IntLit)* ","?
```

## Functions, Methods, and Properties

```bnf
FuncDecl           ::= FuncSig ";"
FuncDef            ::= FuncSig Block
Func               ::= FuncSig Block

FuncSig            ::= "fn" Identifier TypeParamList? "(" ParamList? ")" ("->" TypeExpr)?

ParamList          ::= Param ("," Param)* ","?
Param              ::= ParamMode? Identifier ":" TypeExpr
ParamMode          ::= "inout" | "out" | "sink"

MethodBlock        ::= TypeName "::" TypeName? "{" MethodItem* "}"
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
                     | While | For | Break | Continue | Return

LetBind            ::= "let" Pattern (":" TypeExpr)? "=" Expr ";"
VarBind            ::= "var" Pattern (":" TypeExpr)? "=" Expr ";"
VarDecl            ::= "var" Identifier ":" TypeExpr ";"
Assign             ::= PostfixExpr "=" Expr ";"

While              ::= "while" Expr Block
For                ::= "for" Pattern "in" (RangeExpr | Expr) Block
RangeExpr          ::= IntLit ".." IntLit

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
Expr               ::= If | Match | InfixExpr

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
InfixExpr          ::= OrExpr
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

PostfixExpr        ::= Primary (Call | ArrayIndex | SliceRange | TupleField | StructField | MethodCall | TryPostfix)*
Call               ::= "(" CallArgList? ")"
CallArgList        ::= CallArg ("," CallArg)* ","?
CallArg            ::= ("inout" | "out" | "move")? Expr
ArrayIndex         ::= "[" ExprList "]"
SliceRange         ::= "[" Expr? ".." Expr? "]"
TupleField         ::= "." IntLit
StructField        ::= "." Identifier
MethodCall         ::= "." Identifier "(" CallArgList? ")"
TryPostfix         ::= "?"
```

## Primary Expressions and Literals

```bnf
Primary            ::= Literal
                     | Identifier
                     | EnumVariant
                     | StructUpdate
                     | "(" Expr ")"
                     | Block
                     | ClosureExpr

EnumVariant        ::= TypeName "::" Identifier ("(" ExprList ")")?
StructUpdate       ::= "{" Expr "|" StructUpdateField ("," StructUpdateField)* ","? "}"
StructUpdateField  ::= Identifier ":" Expr

Literal            ::= UnitLit | IntLit | BoolLit | CharLit | StringLit | StringFmt
                     | ArrayLit | SetLit | MapLit | TupleLit | StructLit

ArrayLit           ::= TypeExpr? "[" ExprList "]"
                     | TypeExpr? "[" Expr ";" IntLit "]"
TupleLit           ::= "(" Expr "," ExprList? ")"
StructLit          ::= TypeName "{" StructLitFieldList? "}"
StructLitFieldList ::= StructLitField ("," StructLitField)* ","?
StructLitField     ::= Identifier ":" Expr

SetLit             ::= "{" Expr "," ExprList? "}"
                     | "set" "<" TypeExpr ">" "{" (ExprList)? "}"

MapLit             ::= "{" MapEntry ("," MapEntry)* ","? "}"
                     | "map" "<" TypeExpr "," TypeExpr ">" "{" (MapEntryList)? "}"
MapEntry           ::= Expr ":" Expr
MapEntryList       ::= MapEntry ("," MapEntry)* ","?
```

## Lexical

```bnf
Identifier         ::= [a-zA-Z_][a-zA-Z0-9_]*
ExprList           ::= Expr ("," Expr)* ","?

UnitLit            ::= "()"
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
- `?` is an error-union propagation operator.
