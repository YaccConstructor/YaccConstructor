module Z3logic
open Microsoft.Z3


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


    let a = ctx.MkConst("a", ctx.BoolSort)
    let b = ctx.MkConst("b", ctx.BoolSort)
    let c = ctx.MkConst("c", ctx.BoolSort)
    let d = ctx.MkConst("d", ctx.BoolSort)

    let e1 = ctx.MkEq(a, b)
    let e2 = ctx.MkEq(b, c)
    let e3 = ctx.MkEq(c, d)
    let e4 = ctx.MkNot(ctx.MkEq(d, a))

    let f = ctx.MkAnd([|e1; e2; e3(*; e4*)|])

    prove ctx f

    ctx.Dispose()

