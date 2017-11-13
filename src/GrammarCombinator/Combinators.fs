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

    type Product with
        static member (%) (p, (m, n)) = Production.PRepet(getProd p, Some m, Some n)
        static member (%) (p, (m)) = Production.PRepet(getProd p, Some m, None)

    let private applyBinop op a b = mkProd <| op (getProd a) (getProd b)
    let private applyUnaryop op = getProd >> op >> mkProd

    type Product with
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

    let inline private assignProd uname p = Product(Some uname, getProd p)


    // --------------------------------------------- BEFORE EVALUATE ---------------------------------------------
    let rec private fixExpression =
        let rec mkRuleWithName (name: Var) = function
            | Lambda(u, prodExpr) -> Expr.Lambda(u, mkRuleWithName name prodExpr)
            | prodExpr when prodExpr.Type = typeof<Product> ->
                let uname = name.Name, getUniqueID()
                <@@ assignProd %%(Expr.Value uname) %%(fixExpression prodExpr) @@>
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
        | PropertyGet(None, info, args) ->
            Expr.PropertyGet(info, List.map fixExpression args)
        | Call(None, op, args) ->
            Expr.Call(op, List.map fixExpression args)
        | IfThenElse(p, t1, t2) ->
            Expr.IfThenElse(fixExpression p, fixExpression t1, fixExpression t2)
        | t -> t // TODO: all Expressions

    let fixTree (expr: Expr<Product>) : Expr = fixExpression expr.Raw


    // --------------------------------------------- POSTPROCESSING ---------------------------------------------
    let productToGrammar start : Product -> GrammarDefinition =
        let inline mkName name uid = name + "$" + uid.ToString()
        let inline mkPRef name uid = PRef(Source <| mkName name uid, None)

        let collectRules : Product -> list<Rule> =
            let rec collectRulesk product used k =
                let getBinOp = function
                    | PShuff _ -> PShuff
                    | PAlt _ -> PAlt
                    | PConj _ -> PConj
                let getUnaryOp = function
                    | PNeg _ -> PNeg
                    | PMany _ -> PMany
                    | PSome _ -> PSome
                    | POpt _ -> POpt
                match product with
                | PShuff(l, r)
                | PAlt(l, r)
                | PConj(l, r) ->
                    collectRulesk l used (fun (l, used, rules) ->
                    collectRulesk r used (fun (r, used, rules') ->
                    k <| (getBinOp product (l, r), used, rules @ rules')))
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
                | PNeg p
                | PMany p
                | PSome p
                | POpt p ->
                    collectRulesk p used (fun (x, used, rules) ->
                    k <| (getUnaryOp product x, used, rules))
                | _ -> fail()

            let collectRulesFromProduct = function
                | Product(Some(name, uid), prod) ->
                    collectRulesk prod Set.empty (fun (prod, _, rules) ->
                    Wrapper.IL.rule (mkName name uid) prod true :: rules)
                | _ -> __unreachable__()

            assignProd(start, getUniqueID()) >> collectRulesFromProduct

        collectRules >> Wrapper.IL.grammar >> Wrapper.IL.definition

module GrammarGenerator =
    open Microsoft.FSharp.Quotations
    open FSharp.Quotations.Evaluator
    open Combinators
    open Core

    let generate (name: string) : Expr<Product> -> GrammarDefinition =
        fixTree
        >> QuotationEvaluator.EvaluateUntyped
        >> (fun x -> x :?> Product)
        >> productToGrammar name
