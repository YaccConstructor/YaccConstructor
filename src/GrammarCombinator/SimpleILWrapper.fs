namespace GrammarCombinator.Wrapper

module internal Shortcuts =
    open Yard.Core.IL

    type Production<'a> = Production<Source, 'a>
    type Rule = Rule<Source, Source>
    type Grammar = Grammar<Source, Source>
    type GrammarDefinition = Definition<Source, Source>
    let Source x = new Source(x)
    let fail() = failwith "Internal Fail"
    let untuple f a b = f(a, b)

module internal IL =
    open Yard.Core.IL
    open Shortcuts


    let conc x y =
        let prodElem a = {omit=false; rule=a; binding=None; checker=None}
        let xys =
            match x, y with
            | PSeq(xs, _, _), PSeq(ys, _, _) -> xs @ ys
            | PSeq(xs, _, _), _ -> List.append xs [prodElem y]
            | _, PSeq(ys, _, _) -> prodElem x :: ys
            | _ -> [prodElem x; prodElem y]
        PSeq(xys, None, None)

    let rule (name: string) (p: Production<Source>) (isStart: bool) : Rule =
        {defaultRule (Source name) p with isStart = isStart; isPublic = true}

    let grammar (rules: list<Rule>) : Grammar =
        List.map (fun r -> {r with allPublic = true}) (Yard.Core.Helpers.defaultModules rules)

    let definition (g: Grammar) : GrammarDefinition = {emptyGrammarDefinition with grammar = g}
