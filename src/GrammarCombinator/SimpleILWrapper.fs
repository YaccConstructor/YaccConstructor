namespace GrammarCombinator.Wrapper

module internal Shortcuts =
    open Yard.Core.IL

    type Production = Production<Source, Source>
    type Rule = Rule<Source, Source>
    type Grammar = Grammar<Source, Source>
    type GrammarDefinition = Definition<Source, Source>
    let Source x = new Source(x)
    let fail() = failwith "Internal Fail"
    let untuple f a b = f(a, b)

module internal IL =
    open Yard.Core.IL
    open Shortcuts

    let inline pref n = Production.PRef(Source n, None)

    let conc a b =
        Production.PSeq([{omit=false; rule=a; binding=None; checker=None};
                         {omit=false; rule=b; binding=None; checker=None}], None, None)

    let rule (name: string) (p: Production) (isStart: bool) : Rule =
        {defaultRule (Source name) p with isStart = isStart; isPublic = true}

    let grammar (rules: list<Rule>) : Grammar =
        List.map (fun r -> {r with allPublic = true}) (Yard.Core.Helpers.defaultModules rules)

    let definition (g: Grammar) : GrammarDefinition = {emptyGrammarDefinition with grammar = g}
