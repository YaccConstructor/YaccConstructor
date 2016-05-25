module CalcAstEvaluate

open RNGLR.ReadBackParser.CalcEBNF

let CalcEvaluate ast =
    
    let rec evaluateExpr = function
    | Expr (leftTerm, rightPart) ->
        let f leftValue (sign, term) =
            let rightValue = evaluateTerm term
            match sign with
            | Choice1Of2 (_) -> leftValue + rightValue
            | Choice2Of2 (_) -> leftValue - rightValue
        rightPart |> List.fold f (evaluateTerm leftTerm)

    and evaluateTerm = function
    | Term (leftFact, rightPart)->
        let f leftValue (sign, fact) =
            let rightValue = evaluateFact fact
            match sign with
            | Choice1Of2 (_) -> leftValue * rightValue
            | Choice2Of2 _ -> leftValue / rightValue
        rightPart |> List.fold f (evaluateFact leftFact)

    and evaluateFact = function
    | Fact fact ->
        match fact with
        | Choice1Of2 (NUM num) ->
            num
        | Choice2Of2 (_, expr, _) ->
            evaluateExpr expr
        | _ -> failwithf "Invalid Factor node"

    evaluateExpr ast