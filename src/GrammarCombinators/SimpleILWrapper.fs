namespace YC.Frontends.GrammarCombinators.Wrapper

module internal Shortcuts =
    open YC.Core.IL

    type Production<'a> = Production<Source, 'a>
    type Rule = Rule<Source, Source>
    type Grammar = Grammar<Source, Source>
    let Source x = new Source(x)
    let fail() = failwith "Internal Fail"
    let __unreachable__() = failwith "Unreachable branch hit"
    let untuple f a b = f(a, b)

    let rec mapFoldk f a xs k =
        match xs with
        | [] -> k ([], a)
        | x::xs' ->
            f a x (fun (x', a') ->
                mapFoldk f a' xs' (fun (ys, b) -> k (x' :: ys, b)))

module internal IL =
    open YC.Core.IL
    open Shortcuts

    let prodElem a = {omit=false; rule=a; binding=None; checker=None}

    let conc x y =
        let xys =
            match x, y with
            | PSeq(xs, _, _), PSeq(ys, _, _) -> xs @ ys
            | PSeq(xs, _, _), _ -> List.append xs [prodElem y]
            | _, PSeq(ys, _, _) -> prodElem x :: ys
            | _ -> [prodElem x; prodElem y]
        PSeq(xys, None, None)

    let (|BinaryIL|_|) prod =
        let getBinOp = function
            | PShuff _ -> PShuff
            | PAlt _ -> PAlt
            | PConj _ -> PConj
            | _ -> failwith "unsupported"
        match prod with
        | PShuff(l, r)
        | PAlt(l, r)
        | PConj(l, r) -> Some((fun x y -> getBinOp prod (x, y)), l, r)
        | _ -> None

    let (|UnaryIL|_|) prod =
        let getUnaryOp = function
            | PNeg _ -> PNeg
            | PMany _ -> PMany
            | PSome _ -> PSome
            | POpt _ -> POpt
            | _ -> failwith "unsupported"
        match prod with
            | PNeg p
            | PMany p
            | PSome p
            | POpt p -> Some(getUnaryOp prod, p)
            | _ -> None

    let rule (name: string) (p: Production<Source>) (isStart: bool) : Rule =
        {defaultRule (Source name) p with isStart = isStart; isPublic = true}

    let grammar (rules: list<Rule>) : Grammar =
        List.map (fun r -> {r with allPublic = true}) (YC.Core.Helpers.defaultModules rules)

    let definition (g: Grammar) : Definition<Source, Source> = {emptyGrammarDefinition with grammar = g}
