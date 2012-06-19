module AST

type Ident = Ident of string

type Subexpr =
    | Number of int
    | Var of string
    | Sum of Subexpr*Subexpr
    | Fun of Ident*Subexpr

type Expr = Expr of Ident*Subexpr

type Prog = Prog of List<Expr>
        