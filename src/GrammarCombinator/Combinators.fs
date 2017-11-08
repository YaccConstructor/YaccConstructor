namespace GrammarCombinator
open GrammarCombinator.Wrapper.Shortcuts

module Combinators =
    let internal extendRules n p = Set.add <| Wrapper.IL.rule n p false

    type Product =
        | Product of option<string> // optional name (name -> prod)
                     * Production
                     * Set<Rule>    // referenced simple rules
                     * list<unit -> Product> // recursive rules. if `p` is in the list, we want it's set of rules too
        with
        member this.Name = match this with Product(Some name, _, _, _) -> name

    let internal refProduct = function
        | Product(Some n, p, used, recs) -> Wrapper.IL.pref n, extendRules n p used, recs
        | Product(None, p, used, recs) -> p, used, recs

    let private applyBinop op a b =
        let a, used, recs = refProduct a
        let b, used', recs' = refProduct b
        Product(None, op a b, Set.union used used', recs @ recs')

    let private applyUnaryop op = refProduct >> (fun (p, used, recs) -> Product(None, op p, used, recs))

    type Product with
        static member (+) (a: Product, b: Product) = applyBinop Wrapper.IL.conc a b

    let (<|>) = applyBinop <| untuple Production.PAlt
    let (^&^) = applyBinop <| untuple Production.PConj
    let (!*) = applyUnaryop Production.PMany
    let (!?) = applyUnaryop Production.POpt
    let (!+) = applyUnaryop Production.PSome
    let (!~) = applyUnaryop Production.PNeg

    let inline private anonProd p = Product(None, p, set[], [])
    let tok =  Source >> Production.PToken >> anonProd
    let Eps = anonProd <| Production.PSeq([], None, None)

module internal Core =
    open Combinators

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns


    // --------------------------------------------- AT EVALUATE ---------------------------------------------
    let private assignProd n = refProduct >> (fun (p, used, recs) -> Product(Some n, p, used, recs))


    // --------------------------------------------- BEFORE EVALUATE ---------------------------------------------
    let rec fixTree : Expr -> Expr =
        let mkRuleWithName (name: Var) (prodExpr: Expr) =
            let assignRule prod =
                <@@ assignProd %%(Expr.Value name.Name) %%(fixTree prod) @@>
            match name.Type with
            | t when t = typeof<unit -> Product> ->
                match prodExpr with Lambda(u, prodExpr) -> Expr.Lambda(u, assignRule prodExpr)
            | t when t = typeof<Product> ->
                assignRule prodExpr
            | _ -> fixTree prodExpr
        function
        | Let(name, prodExpr, body) ->
            let letExpr = mkRuleWithName name prodExpr
            Expr.Let(name, letExpr, fixTree body)
        | LetRecursive(dfs, body) ->
            let bindings = List.map (fun (n, p) -> n, mkRuleWithName n p) dfs
            Expr.LetRecursive(bindings, fixTree body)
        | Application(op, args) as app ->
            Expr.Application(
                if op.Type = typeof<unit -> Product>
                then <@@ fun (_: unit) -> Product(None, Wrapper.IL.pref <| op.ToString(), set[], [%%op]) @@>
                else fixTree op
                , fixTree args)
        | PropertyGet(None, info, args) ->
            Expr.PropertyGet(info, List.map fixTree args)
        | Call(None, op, args) ->
            Expr.Call(op, List.map fixTree args)
        | IfThenElse(p, t1, t2) ->
            Expr.IfThenElse(fixTree p, fixTree t1, fixTree t2)
        | t -> t // TODO: all Expressions


    // --------------------------------------------- POSTPROCESSING ---------------------------------------------
    let productToGrammar start : Product -> GrammarDefinition =
        let collectRules : Product -> list<Rule> =
            let rec collectRulesBFS used rules = function
                | [] -> rules
                | el::queue ->
                    match el with
                    | Product(Some name, prod, rules', recs) ->
                        let used = Set.add name used
                        let rules = extendRules name prod <| Set.union rules rules'
                        let queue' =
                            List.map ((|>)()) recs
                            |> List.filter (fun x -> not <| Set.contains x.Name used)
                        collectRulesBFS used rules <| queue' @ queue

            let collectRulesFromProd = function
                | Product(Some name, prod, rules, recs) -> // guaranteed to be the only one case
                    List.map ((|>)()) recs
                    |> collectRulesBFS Set.empty Set.empty
                    |> Set.union rules
                    |> Set.add (Wrapper.IL.rule name prod true)
                    |> List.ofSeq

            assignProd start >> collectRulesFromProd

        collectRules >> Wrapper.IL.grammar >> Wrapper.IL.definition

module GrammarGenerator =
    open Microsoft.FSharp.Quotations
    open FSharp.Quotations.Evaluator
    open Combinators
    open Core

    let generate (name: string) : Expr -> GrammarDefinition =
        fixTree
        >> QuotationEvaluator.EvaluateUntyped
        >> (fun x -> x :?> Product)
        >> productToGrammar name
