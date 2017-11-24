namespace GrammarCombinator
open GrammarCombinator.Wrapper.Shortcuts

module Combinators =
    type Product = Product of option<string * int> // optional name and unique id
                              * Production<unit -> Product>
    with override this.ToString() =
            match this with
            | Product(None, p) -> sprintf "Product(None, %O)" p
            | Product(Some(s, n), p) -> sprintf "Product(%O$%O, %O)" s n p

    let inline internal pref n = Production.PRef(Source "", Some n)
    let internal getProd = function Product(_, p) -> p
    let private mkProd = untuple Product None

    let private applyBinop op a b = mkProd <| op (getProd a) (getProd b)
    let private applyUnaryop op = getProd >> op >> mkProd

    type Product with
        static member (%) (p, (m, n)) = mkProd <| Production.PRepet(getProd p, Some m, Some n)
        static member (%) (p, (m)) = mkProd <| Production.PRepet(getProd p, Some m, None)
        static member (+) (a: Product, b: Product) = applyBinop Wrapper.IL.conc a b

    let (<|>) = applyBinop <| untuple Production.PAlt
    let (^&^) = applyBinop <| untuple Production.PConj
    let (!*) = applyUnaryop Production.PMany
    let (!?) = applyUnaryop Production.POpt
    let (!+) = applyUnaryop Production.PSome
    let (!~) = applyUnaryop Production.PNeg

    let tok = Source >> Production.PToken >> mkProd
    let Eps = mkProd <| Production.PSeq([], None, None)

module internal Core =
    open Combinators
    open Yard.Core.IL

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns


    // --------------------------------------------- AT EVALUATE ---------------------------------------------
    let private getUniqueID =
        let uniqueID = ref 0
        fun (_: unit) ->
            let last = !uniqueID
            uniqueID := !uniqueID + 1
            last

    let private assignProd name =
        let uid = getUniqueID()
        fun p -> Product(Some(name, uid), getProd p)


    // --------------------------------------------- BEFORE EVALUATE ---------------------------------------------
    let rec private fixExpression =
        let rec mkRuleWithName (name: Var) = function
            | Lambda(u, prodExpr) -> Expr.Lambda(u, mkRuleWithName name prodExpr)
            | prodExpr when prodExpr.Type = typeof<Product> ->
                let assigner = assignProd name.Name
                <@@ assigner %%(fixExpression prodExpr) @@>
            | prodExpr -> fixExpression prodExpr // TODO: match inner grammars
        function
        | Let(name, prodExpr, body) ->
            let letExpr = mkRuleWithName name prodExpr
            Expr.Let(name, letExpr, fixExpression body)
        | LetRecursive(dfs, body) ->
            let bindings = List.map (fun (n, p) -> n, mkRuleWithName n p) dfs
            Expr.LetRecursive(bindings, fixExpression body)
        | Application(op, args) as app ->
            Expr.Application(
                if op.Type = typeof<unit -> Product>
                then <@@ fun (_: unit) -> Product(None, pref %%op) @@>
                else fixExpression op
                , fixExpression args)
        | IfThenElse(p, t1, t2) -> Expr.IfThenElse(fixExpression p, fixExpression t1, fixExpression t2)
        | NewObject(cinfo, exprs) -> Expr.NewObject(cinfo, List.map fixExpression exprs)
        | AddressOf expr -> Expr.AddressOf <| fixExpression expr
        | AddressSet(left, right) -> Expr.AddressSet(fixExpression left, fixExpression right)
        | Sequential(left, right) -> Expr.Sequential(fixExpression left, fixExpression right)
        | TryFinally(tryblock, finalblock) -> Expr.TryFinally(fixExpression tryblock, fixExpression finalblock)
        | WhileLoop(cond, body) -> Expr.WhileLoop(fixExpression cond, fixExpression body)
        | Coerce(expr, tp) -> Expr.Coerce(fixExpression expr, tp)
        | TypeTest(expr, tp) -> Expr.TypeTest(fixExpression expr, tp)
        | UnionCaseTest(expr, uinfo) -> Expr.UnionCaseTest(fixExpression expr, uinfo)
        | TryWith(body, filterVar, filterBody, catchVar, catchBody) ->
            Expr.TryWith(fixExpression body, filterVar, fixExpression filterBody, catchVar, fixExpression catchBody)
        | TupleGet(expr, ind) -> Expr.TupleGet(fixExpression expr, ind)
        | NewTuple exprs -> Expr.NewTuple <| List.map fixExpression exprs
        | NewArray(tp, exprs) -> Expr.NewArray(tp, List.map fixExpression exprs)
        | NewRecord(tp, exprs) -> Expr.NewRecord(tp, List.map fixExpression exprs)
        | NewDelegate(tp, vrs, expr) -> Expr.NewDelegate(tp, vrs, fixExpression expr)
        | NewUnionCase(uinfo, exprs) -> Expr.NewUnionCase(uinfo, List.map fixExpression exprs)
        | DefaultValue _ as dv -> dv
        | Var _ as vr -> vr
        | Value _ as vl -> vl
        | Lambda(vr, expr) -> Expr.Lambda(vr, fixExpression expr)
        | VarSet(vr, expr) -> Expr.VarSet(vr, fixExpression expr)
        | ForIntegerRangeLoop(loopVariable, start, endExpr, body) ->
            Expr.ForIntegerRangeLoop(loopVariable, fixExpression start, fixExpression endExpr, fixExpression body)
        | Call(None, op, args) -> Expr.Call(op, List.map fixExpression args)
        | Call(Some expr, op, args) -> Expr.Call(fixExpression expr, op, List.map fixExpression args)
        | FieldGet(None, _) as fg -> fg
        | FieldGet(Some expr, finfo) -> Expr.FieldGet(fixExpression expr, finfo)
        | FieldSet(None, finfo, expr) -> Expr.FieldSet(finfo, fixExpression expr)
        | FieldSet(Some fld, finfo, expr) -> Expr.FieldSet(fixExpression fld, finfo, fixExpression expr)
        | PropertyGet(None, pinfo, args) -> Expr.PropertyGet(pinfo, List.map fixExpression args)
        | PropertyGet(Some expr, pinfo, exprs) -> Expr.PropertyGet(fixExpression expr, pinfo, List.map fixExpression exprs)
        | PropertySet(None, pinfo, exprs, expr) -> Expr.PropertySet(pinfo, fixExpression expr, List.map fixExpression exprs)
        | PropertySet(Some prop, pinfo, exprs, expr) ->
            Expr.PropertySet(fixExpression prop, pinfo, fixExpression expr, List.map fixExpression exprs)
        | Quote _ as expr -> failwithf "Cannot use Quotations inside Quotations! %O" expr
        | t -> failwithf "Internal error. Please report this object to developer team: %O" t

    let fixTree (expr: Expr<Product>) : Expr = fixExpression expr.Raw


    // --------------------------------------------- POSTPROCESSING ---------------------------------------------
    let productToGrammar start =
        let inline mkName name uid = name + "$" + uid.ToString()
        let inline mkPRef name uid = PRef(Source <| mkName name uid, None)

        let collectRules : Product -> list<Rule> =
            let rec collectRulesk product used k =
                match product with
                | Wrapper.IL.BinaryIL(binOp, l, r) ->
                    collectRulesk l used (fun (l, used, rules) ->
                    collectRulesk r used (fun (r, used, rules') ->
                    k <| (binOp l r, used, rules @ rules')))
                | Wrapper.IL.UnaryIL(unOp, p) ->
                    collectRulesk p used (fun (x, used, rules) ->
                    k <| (unOp x, used, rules))
                | PSeq(ps, _, _) ->
                    mapFoldk
                        (fun (used, rules) pelem k ->
                            collectRulesk pelem.rule used (fun (r, used, rules') -> k(r, (used, rules @ rules'))))
                        (used, []) ps
                        (fun (rs, (used, rules)) ->
                            k (PSeq(List.map Wrapper.IL.prodElem rs, None, None), used, rules))
                | PToken s -> k (PToken s, used, [])
                | PRef(_, Some callback) ->
                    match callback() with
                    | Product(Some(name, uid), _) when Set.contains uid used ->
                        k (mkPRef name uid, used, [])
                    | Product(Some(name, uid), prod) ->
                        collectRulesk prod (Set.add uid used) (fun (prod, used, rules) ->
                        k (mkPRef name uid, used, Wrapper.IL.rule (mkName name uid) prod false :: rules))
                    | _ -> __unreachable__()
                | PRepet(prod, l, r) ->
                    collectRulesk prod used (fun (prod, used, rules) ->
                    k <| (PRepet(prod, l, r), used, rules))
                | _ -> fail()

            let collectRulesFromProduct = function
                | Product(Some(name, uid), prod) ->
                    collectRulesk prod Set.empty (fun (prod, _, rules) ->
                    Wrapper.IL.rule (mkName name uid) prod true :: rules)
                | _ -> __unreachable__()

            assignProd start >> collectRulesFromProduct

        collectRules >> Wrapper.IL.grammar >> Wrapper.IL.definition

module GrammarGenerator =
    open Microsoft.FSharp.Quotations
    open FSharp.Quotations.Evaluator
    open Combinators
    open Core

    let generate (name: string) =
        fixTree
        >> QuotationEvaluator.EvaluateUntyped
        >> (fun x -> x :?> Product)
        >> productToGrammar name
