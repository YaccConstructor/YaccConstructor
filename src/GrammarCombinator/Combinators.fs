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

    let generate (name: string) : Expr<Product> -> GrammarDefinition =
        fixTree
        >> QuotationEvaluator.EvaluateUntyped
        >> (fun x -> x :?> Product)
        >> productToGrammar name
