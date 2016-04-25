module CalcAstEvaluate

open RNGLR.ReadBackParser.CalcEBNF

let CalcEvaluate ast =
    
    let tokenValue = function
    | A _ -> 1
    | B _ -> 2
    | _ -> 0
    
    let rec evaluateExpr expr =
        match expr with
        | MY_expr' (leftTerm, rightPart) ->
            let leftValue = evaluateTerm leftTerm
            let f leftValue rightSignAndTerm =
                let sign, term = rightSignAndTerm
                let rightValue = evaluateTerm term
                match sign with
                | Choice1Of2 (ADD _) -> leftValue + rightValue
                | Choice2Of2 (MIN _) -> leftValue - rightValue
                | _ -> failwithf "Wrong sign"
            rightPart |> List.fold f leftValue

    and evaluateTerm = function
    | MY_term' (leftFact, rightPart)->
        let leftValue = evaluateFact leftFact
        let f leftValue rightSignAndFact =
            let sign, fact = rightSignAndFact
            let rightValue = evaluateFact fact
            match sign with
            | Choice1Of2 (MUL _) -> leftValue * rightValue
            | Choice2Of2 (DIV _) -> leftValue / rightValue
            | _ -> failwithf "Wrong sign"
        rightPart |> List.fold f leftValue

    and evaluateFact = function
    | MY_fact' fact ->
        match fact with
        | Choice1Of2 (MY_num' num) ->
            match num with
            | Choice1Of2 x | Choice2Of2 x -> tokenValue x
        | Choice2Of2 (_, expr, _) ->
            evaluateExpr expr

    ast |> unbox |> MY_expr' |> evaluateExpr