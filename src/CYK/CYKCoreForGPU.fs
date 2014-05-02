namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

type CYKCoreForGPU() =

    // правила грамматики, инициализируются в Recognize
    let mutable rules : array<rule> = [||]

    let mutable recTable:Option<_>[] = null

    let mutable rowSize = 0

    let mutable nTermsCount = 0

    let mutable lblNameArr = [||]

    let lblString lbl = 
        match lblNameArr with
        | [||] -> "0" 
        | _ -> 
                match lbl with 
                | 0uy -> "0"
                | _ -> lblNameArr.[(int lbl) - 1]

    let calcDiff columnIndex =
            if columnIndex < 2
            then 0
            else 
                let diff = ref 0
                [|1..columnIndex-1|]
                |> Array.iter (fun i -> diff := (!diff + i))
                !diff

    let printTbl () =
        (*
        printfn "As table:"
        for i in 0..rowSize - 1 do
            for j in 0..rowSize - 1 do
                if i < rowSize - i
                then
                    let startIndex = (i * rowSize + j - calcDiff i) * nTermsCount
                    let mutable count = 0
                    for m in startIndex..startIndex + nTermsCount - 1 do
                        if recTable.[m].IsSome then count <- count + 1
                    printf " %d |" count
            printfn ""
        printfn ""
        *)
        let needNewLine i =
            if i = rowSize - 1 then true
            else
                let mutable lnEndCounter = rowSize - 1
                let mutable newLine = rowSize - 1
                let mutable k = 0
                let mutable need = false
                while (k  <= rowSize - 1 && i <> newLine) do
                    newLine <- newLine + lnEndCounter
                    lnEndCounter <- lnEndCounter - 1
                    k <- k + 1
                    if newLine = i 
                    then 
                        need <- true
                need

        [| 0..( rowSize * rowSize - calcDiff rowSize - 1) |] 
        |> Array.iter( fun i -> 
                                let startIndex = i * nTermsCount // (i * rowSize + j - calcDiff i) * nTermsCount
                                let mutable count = 0
                                for m in startIndex..startIndex + nTermsCount - 1 do
                                    if recTable.[m].IsSome then count <- count + 1
                                printf " %d |" count
                                if needNewLine i then printfn ""
        )
        
    let recognitionTable (_,_) (s:uint16[]) weightCalcFun =

        nTermsCount <- 
            rules
            |> Array.map(fun r -> 
                            let rule = getRuleStruct r
                            rule.RuleName)
            |> Set.ofArray
            |> Set.count

        rowSize <- s.Length
        recTable <- Array.init ( ( rowSize * rowSize - calcDiff rowSize ) * nTermsCount) (fun _ -> None)
        printfn "row size %d" rowSize
        printfn "non terms count %d" nTermsCount
        printfn "matrix should be %d" (rowSize * rowSize * nTermsCount)
        printfn "rec table size %d" recTable.Length

        let chooseNewLabel (ruleLabel:uint8) (lbl1:byte) (lbl2:byte) lState1 lState2 =
            if lState1 = LblState.Conflict then new LabelWithState(noLbl, LblState.Conflict)
            elif lState2 = LblState.Conflict then new LabelWithState(noLbl, LblState.Conflict)
            elif lState1 = LblState.Undefined && lState2 = LblState.Undefined && ruleLabel = noLbl 
            then new LabelWithState(noLbl, LblState.Undefined)
            else
                let mutable notEmptyLbl1 = noLbl
                let mutable notEmptyLbl2 = noLbl
                let mutable notEmptyLbl3 = noLbl 
                let mutable realLblCount = 0
                if lbl1 <> noLbl then 
                    notEmptyLbl1 <- lbl1
                    realLblCount <- realLblCount + 1
                if lbl2 <> noLbl then
                    if realLblCount = 0 then notEmptyLbl1 <- lbl2
                    elif realLblCount = 1 then notEmptyLbl2 <- lbl2
                    realLblCount <- realLblCount + 1
                if ruleLabel <> noLbl then 
                    if realLblCount = 0 then notEmptyLbl1 <- ruleLabel
                    elif realLblCount = 1 then notEmptyLbl2 <- ruleLabel
                    elif realLblCount = 2 then notEmptyLbl3 <- ruleLabel
                    realLblCount <- realLblCount + 1

                if realLblCount = 1 ||
                    (realLblCount = 2 && notEmptyLbl2 = notEmptyLbl1) ||
                    (realLblCount = 3 && notEmptyLbl2 = notEmptyLbl1 && notEmptyLbl3 = notEmptyLbl1)
                then new LabelWithState(notEmptyLbl1, LblState.Defined)
                else new LabelWithState(noLbl, LblState.Conflict)
                
        let processRule rule ruleIndex i k l =
            let rule = getRuleStruct rule
            if rule.R2 <> 0us then
                let leftStart = ( i * rowSize + k - calcDiff i ) * nTermsCount
                let rightStart = ( (k+i+1) * rowSize + l-k-1 - (calcDiff (k+i+1)) ) * nTermsCount

                for m in 0..nTermsCount - 1 do
                    let leftCell = recTable.[leftStart + m]
                    if leftCell.IsSome && getCellRuleTop leftCell.Value rules = rule.R1 then
                        let cellData1 = getCellDataStruct leftCell.Value
                        for n in 0..nTermsCount - 1 do
                            let rightCell = recTable.[rightStart + n]
                            if rightCell.IsSome && getCellRuleTop rightCell.Value rules = rule.R2 then
                                let cellData2 = getCellDataStruct rightCell.Value
                                let lblWithState = chooseNewLabel rule.Label cellData1.Label cellData2.Label cellData1.LabelState cellData2.LabelState
                                let newWeight = weightCalcFun rule.Weight cellData1.Weight cellData2.Weight
                                let currentElem = buildData ruleIndex lblWithState.State lblWithState.Label newWeight
                                recTable.[( i * rowSize + l - calcDiff i ) * nTermsCount + int rule.RuleName - 1] <- new CellData(currentElem, uint32 k) |> Some
            
        let elem i len rulesIndexed = 
            // foreach rule r in grammar in parallel
            rulesIndexed 
            |> Array.iter (fun (curRule:RuleIndexed) -> for k in 0..(len-1) do processRule curRule.Rule curRule.Index i k len)
        (*
        let elem2 i len symRuleArr = 
            // foreach symbol in grammar in parallel
            symRuleArr
            |> Array.Parallel.iter (fun (item:SymbolRuleMapItem) ->
                                        // foreach rule r per symbol in parallel
                                        item.Rules
                                        |> Array.iter (
                                            fun curRule -> for k in 0..(len-1) do processRule curRule.Rule curRule.Index i k len
                                        )
            )
        *)
        let fillTable rulesIndexed =
          [|1..rowSize - 1|]
          |> Array.iter (fun len ->
                [|0..rowSize - 1 - len|] // for start = 0 to nWords - length in parallel
                |> Array.Parallel.iter (fun i -> elem i len rulesIndexed))
        (*
        let fillTable2 symRuleArr = 
            [|1..rowSize - 1|]
            |> Array.iter (fun len ->
                [|0..rowSize - 1 - len|] // for start = 0 to nWords - length in parallel
                |> Array.Parallel.iter (fun i -> elem2 i len symRuleArr))
        *)
        
        rules
        |> Array.iteri 
            (fun ruleIndex rule ->
                for k in 0..(rowSize - 1) do
                    let rule = getRuleStruct rule               
                    if rule.R2 = 0us && rule.R1 = s.[k] then
                        let lState =
                            match rule.Label with
                            | 0uy -> LblState.Undefined
                            | _   -> LblState.Defined
                        let currentElem = buildData ruleIndex lState rule.Label rule.Weight
                        recTable.[(k * rowSize + 0 - calcDiff k) * nTermsCount + int rule.RuleName - 1] <- new CellData(currentElem,0u) |> Some)
        printfn "total rules count %d" rules.Length
                             
        let ntrIndexes = new ResizeArray<_>() // non-terminal rules indexes array
        rules
        |> Array.iteri
            (fun ruleIndex rule ->
                let ruleStruct = getRuleStruct rule
                if ruleStruct.R2 <> 0us then 
                    ntrIndexes.Add ruleIndex )
        let nonTermRules = Array.init ntrIndexes.Count (fun i -> new RuleIndexed(rules.[ntrIndexes.[i]], ntrIndexes.[i]) )        
        printfn "non terminal rules count %d" nonTermRules.Length
        (*
        // left parts of non-terminal rules array
        // needed only for 2nd realization
        let symRuleMap = 
            nonTermRules
            |> Seq.groupBy (fun rule -> initSymbol (getRuleStruct rule.Rule).RuleName )
            |> Map.ofSeq
            |> Map.map (fun k v -> Array.ofSeq v)
        
        let symRuleArr =
            symRuleMap
            |> Map.toArray
            |> Array.map (fun (sym,rules) -> 
                            //printfn "Symbol %d rules count: %d" sym rules.Length
                            new SymbolRuleMapItem(sym,rules))
        *)
        let fillStart = System.DateTime.Now
        printfn "Fill table started %s" (string fillStart)
        fillTable nonTermRules
        let fillFinish = System.DateTime.Now
        printfn "Fill table finished %s [%s]" (string fillFinish) (string (fillFinish - fillStart))
        (*
        let fillImprStart = System.DateTime.Now
        printfn "Fill table improved started %s" (string fillImprStart)
        fillTable2 symRuleArr
        let fillImprFinish = System.DateTime.Now
        printfn "Fill table improved finished %s [%s]" (string fillImprFinish) (string (fillImprFinish - fillImprStart))
        *)
        recTable

    let recognize ((grules, start) as g) s weightCalcFun =
        let recTable = recognitionTable g s weightCalcFun
        
        printTbl ()

        let getString state lbl weight = 
            let stateString = 
                match state with
                |LblState.Defined -> "defined"
                |LblState.Undefined -> "undefined"
                |LblState.Conflict -> "conflict"
                |_ -> ""

            String.concat " " [stateString; ":"; "label ="; lblString lbl; "weight ="; string weight]
            
        let rec out i last =
            let cellData = recTable.[( 0 * rowSize + s.Length-1 ) * nTermsCount + i]
            if i <= last 
            then 
                if cellData.IsSome 
                then
                    let cellData = getCellDataStruct (cellData.Value)
                    if i = last
                    then [getString cellData.LabelState cellData.Label cellData.Weight]
                    else getString cellData.LabelState cellData.Label cellData.Weight :: out (i+1) last
                else "" :: out (i+1) last
            else [""]

        let lastIndex = nTermsCount - 1 //(recTable.[0 * rowSize + s.Length-1]).Length - 1
        
        out 0 lastIndex

    let print lblValue weight leftI rightL leftL =
        let out = String.concat " " ["label ="; lblString lblValue; "weight ="; string weight; 
                    "left ="; string leftI; "right ="; string (leftI + rightL + leftL + 1)]
        printfn "%s" out

    let rec trackLabel i l (cell:CellData)  flag =
        let ruleInd,_,curL,curW = getData cell.rData
        let rule = getRuleStruct rules.[int ruleInd]
        let (leftI,leftL),(rightI,rightL) = getSubsiteCoordinates i l (int cell._k)
        if l = 0
        then if curL <> noLbl
             then print curL curW leftI rightL leftL
        else 
            let checkIndex start ind tryFind ruleCheck =
                if ind < start + nTermsCount - 1 then
                    tryFind start (ind + 1) ruleCheck
                else None

            let rec tryFind start index ruleCheck = 
                let x = recTable.[index]
                match x with
                | Some x ->
                    let ind,lSt,lbl,_ = getData x.rData
                    let curRule = getRuleStruct rules.[int ind]
                    if curRule.RuleName = ruleCheck then
                        Some (Some x)
                    else checkIndex start index tryFind ruleCheck
                | None -> checkIndex start index tryFind ruleCheck

            let startLeft = ( leftI * rowSize + leftL - calcDiff leftI ) * nTermsCount
            let left = tryFind startLeft startLeft rule.R1

            let startRight = ( rightI * rowSize + rightL - calcDiff rightI ) * nTermsCount
            let right = tryFind startRight startRight rule.R2
                
            match right with
            | Some (Some right) ->
                match left with 
                | Some (Some left) ->
                    let _,_,lLbl,_ = getData left.rData
                    let _,_,rLbl,_ = getData right.rData
                    if curL <> noLbl && lLbl = noLbl && rLbl = noLbl
                    then print curL curW leftI rightL leftL
                    else
                        trackLabel leftI leftL left  true
                        trackLabel rightI rightL right  true
                | _ -> ()
            | _ ->
                if flag && rule.Label <> noLbl
                then print curL curW leftI rightL leftL
            
    let labelTracking lastInd = 
        let i,l = 0,lastInd
        let startIndex = ( i * rowSize + l - calcDiff i ) * nTermsCount
        for ind in startIndex..startIndex + nTermsCount - 1 do
            recTable.[ind] 
            |> Option.iter(fun x ->
                                printfn "derivation #%d" (ind + 1)
                                trackLabel i l x false)
            
    
    member this.Recognize ((grules, start) as g) s weightCalcFun lblNames = 
        rules <- grules
        lblNameArr <- lblNames
        // Info about dialects of derivation in format: "<lblState> <lblName> <weight>"
        // If dialect undefined or was conflict lblName = "0" 
        let out = recognize g s weightCalcFun |> List.filter ((<>)"") |> String.concat "\n"
        match out with
        | "" -> "The string is not derivable in specified grammar"
        | _ -> 
            labelTracking (s.Length - 1)
            out
