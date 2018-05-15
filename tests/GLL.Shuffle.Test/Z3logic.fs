module Z3logic
open Microsoft.Z3
open TrieToFormula

let prove (ctx : Context) (f : BoolExpr) = 
    printfn "Proving: %A" f
    let s = ctx.MkSolver()
    let p = ctx.MkParams()
    p.Add("mbqi", false)
    s.Parameters <- p
    s.Assert(f)
    let q = s.Check()

    match q with
    | Status.UNKNOWN -> 
        printfn "Unknown because: %A" s.ReasonUnknown
    | Status.SATISFIABLE ->
        printfn "OK, proof: %A" s.Model
    | Status.UNSATISFIABLE ->
        failwith "UNSAT"




let simpleTest () = 
    let ctx = new Context()


    //let a = ctx.MkConst("a", ctx.BoolSort)
    let a = ctx.MkBoolConst("a")
    let b = ctx.MkBoolConst("b")
    let c = ctx.MkBoolConst("c")
    let d = ctx.MkBoolConst("d")

    let e1 = ctx.MkEq(a, b)
    let e2 = ctx.MkEq(b, c)
    let e3 = ctx.MkEq(c, d)
    let e4 = ctx.MkNot(ctx.MkEq(d, a))
    let e5 = ctx.MkAnd([|a; b|])
    let f = ctx.MkAnd([|e1; e2; e3(*; e4*)|])

    prove ctx f

    ctx.Dispose()

let MkXor (ctx : Context) (exprs : BoolExpr [])  = 
    let r = ref (ctx.MkXor(exprs.[0], exprs.[1]))
    for i in 2..exprs.Length-1 do
        r := ctx.MkXor(!r, exprs.[i])
    !r

let fomulaToZ3expr formula = 
    let ctx = new Context()

    let rec transform = function 
    | OR (l) ->
        l
        |> Array.map transform
        |> ctx.MkOr
    | AND(l) ->
        l
        |> Array.map transform
        |> ctx.MkAnd
    | XOR(l) ->
        l
        |> Array.map transform
        |> MkXor ctx
    | NOT(l) ->
        transform l |> ctx.MkNot 
    | EQ(l,r) ->
        ctx.MkEq( transform l, transform r)
    | VAR(s) ->
        ctx.MkBoolConst(s)
    | NONE -> failwith "unexpected NONE"

    ctx, transform formula

    
let solveFormula formula = 
    let ctx, f = fomulaToZ3expr formula
    prove ctx f
    ctx.Dispose()