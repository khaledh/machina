# Grammar

This is the formal grammar of the Machina programming language in BNF notation.

## Notation

- `::=` means "is defined as"
- `|` separates alternatives
- `*` means zero or more
- `?` means optional (zero or one)
- `( )` groups elements
- `"text"` is a literal token
- `[a-z]` is a character class

## Top-Level

```bnf
Module             ::= TopLevelItem*

TopLevelItem       ::= (AttributeList (TypeDef | TraitDef | FuncDecl | FuncDef)) | MethodBlock
```

## Attributes

```bnf
AttributeList      ::= ("@[" AttributeItem ("," AttributeItem)* ","? "]")?
AttributeItem      ::= Identifier ("(" StringLit ("," StringLit)* ")")?
```

## Type Definitions

```bnf
TypeDef            ::= "type" Identifier "=" TypeDefBody
TypeDefBody        ::= TypeAliasDef | StructDef | EnumDef

TypeAliasDef       ::= TypeExpr ";"?

StructDef          ::= "{" StructFieldList? "}"
StructFieldList    ::= StructField ("," StructField)* ","?
StructField        ::= Identifier ":" TypeExpr

EnumDef            ::= EnumVariantDef ("|" EnumVariantDef)* ";"?
EnumVariantDef     ::= Identifier ( "(" TypeExprList ")" )?

TraitDef           ::= "trait" Identifier "{" TraitMethodDecl* "}"
TraitMethodDecl    ::= MethodSig ";"
```

## Type Expressions

```bnf
TypeExpr           ::= TypeExprTerm ("|" TypeExprTerm)*
TypeExprTerm       ::= TypeExprAtom RefinementType? TypeSuffix*
TypeExprAtom       ::= UnitType | NamedType | TupleType | RangeType | FnType
TypeSuffix         ::= ArraySuffix | SliceSuffix | HeapSuffix
ArraySuffix        ::= "[" IntLitList "]"
SliceSuffix        ::= "[" "]"
HeapSuffix         ::= "^"
RefinementType     ::= ":" Refinement ("&" Refinement)*

UnitType           ::= "()"
NamedType          ::= Identifier
TupleType          ::= "(" TypeExpr "," TypeExprList? ")"
RangeType          ::= "range" "(" IntLit ("," IntLit)? ")"
FnType             ::= "fn" "(" FnTypeParamList? ")" "->" TypeExpr
Refinement         ::= "bounds" "(" IntLit ("," IntLit)? ")" | "nonzero"

FnTypeParamList    ::= FnTypeParam ("," FnTypeParam)* ","?
FnTypeParam        ::= ParamMode? TypeExpr

TypeExprList       ::= TypeExpr ("," TypeExpr)* ","?
IntLitList         ::= IntLit ("," IntLit)* ","?
```

## Functions

```bnf
FuncDecl           ::= FuncSig ";"
FuncDef            ::= FuncSig Block

FuncSig            ::= "fn" Identifier "(" ParamList? ")" ("->" TypeExpr)?

ParamList          ::= Param ("," Param)* ","?
Param              ::= ParamMode? Identifier ":" TypeExpr
ParamMode          ::= "inout" | "out" | "sink"
```

## Methods

```bnf
MethodBlock        ::= Identifier "::" Identifier? "{" MethodItem* "}"
MethodItem         ::= MethodDecl | MethodDef
MethodDecl         ::= AttributeList MethodSig ";"
MethodDef          ::= AttributeList MethodSig Block
MethodSig          ::= "fn" Identifier "(" SelfParam ("," ParamList)? ")" ("->" TypeExpr)?
SelfParam          ::= ParamMode? "self"
```

## Closures

```bnf
ClosureExpr        ::= ClosureCaptureList? ClosureSig Expr
ClosureSig         ::= "|" ParamList? "|" ("->" TypeExpr)?
ClosureCaptureList ::= "[" "move" Identifier ("," Identifier)* ","? "]"
```

## Blocks and Statements

```bnf
Block              ::= "{" BlockItem* (Expr ";"?)? "}"
BlockItem          ::= StmtExpr | Expr ";"

StmtExpr           ::= LetBind | VarBind | VarDecl | Assign | While | For

LetBind            ::= "let" Pattern (":" TypeExpr)? "=" Expr ";"
VarBind            ::= "var" Pattern (":" TypeExpr)? "=" Expr ";"
VarDecl            ::= "var" Identifier ":" TypeExpr ";"

Assign             ::= PostfixExpr "=" Expr ";"

While              ::= "while" Expr Block
For                ::= "for" Pattern "in" (RangeExpr | Expr) Block
RangeExpr          ::= IntLit ".." IntLit
```

## Patterns

```bnf
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
MatchPattern       ::= "_" | BoolLit | IntLit | TuplePattern | EnumVariantPattern

EnumVariantPattern ::= Identifier ("::" Identifier)? ("(" MatchBindingList? ")")?
MatchBindingList   ::= MatchBinding ("," MatchBinding)* ","?
MatchBinding       ::= Identifier | "_"
```

## Operator Expressions

Precedence from lowest (1) to highest (10):

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
```

## Postfix Expressions

```bnf
PostfixExpr        ::= Primary (Call | ArrayIndex | SliceRange | TupleField | StructField | MethodCall)*

Call               ::= "(" CallArgList? ")"
CallArgList        ::= CallArg ("," CallArg)* ","?
CallArg            ::= ("inout" | "out" | "move")? Expr

ArrayIndex         ::= "[" ExprList "]"
SliceRange         ::= "[" Expr? ".." Expr? "]"
TupleField         ::= "." IntLit
StructField        ::= "." Identifier
MethodCall         ::= "." Identifier "(" CallArgList? ")"
```

## Primary Expressions

```bnf
Primary            ::= Literal
                     | Identifier
                     | EnumVariant
                     | StructUpdate
                     | "(" Expr ")"
                     | Block
                     | ClosureExpr

EnumVariant        ::= Identifier "::" Identifier ("(" ExprList ")")?
StructUpdate       ::= "{" Expr "|" StructUpdateField ("," StructUpdateField)* ","? "}"
StructUpdateField  ::= Identifier ":" Expr
```

## Literals

```bnf
Literal            ::= UnitLit | IntLit | BoolLit | CharLit | StringLit | StringFmt
                     | ArrayLit | TupleLit | StructLit

UnitLit            ::= "()"
BoolLit            ::= "true" | "false"
IntLit             ::= DecimalLit | BinaryLit | OctalLit | HexLit
DecimalLit         ::= [0-9] ([0-9_])*
BinaryLit          ::= "0b" [01_]+
OctalLit           ::= "0o" [0-7_]+
HexLit             ::= "0x" [0-9a-fA-F_]+

CharLit            ::= "'" (char | escape) "'"
StringLit          ::= '"' (string-char | escape)* '"'
StringFmt          ::= "f" StringLit

ArrayLit           ::= TypeExpr? "[" ExprList "]"
                     | TypeExpr? "[" Expr ";" IntLit "]"
TupleLit           ::= "(" Expr "," ExprList? ")"
StructLit          ::= Identifier "{" StructLitFieldList? "}"
StructLitFieldList ::= StructLitField ("," StructLitField)* ","?
StructLitField     ::= Identifier ":" Expr
```

## Identifiers and Lists

```bnf
Identifier         ::= [a-zA-Z_] [a-zA-Z0-9_]*
ExprList           ::= Expr ("," Expr)* ","?
```

## Comments

```
// Single-line comment (to end of line)
```

Block comments are not supported.
