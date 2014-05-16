namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

type CYKOnGPU() =

    // правила грамматики, инициализируются в Recognize
    let mutable rules : array<rule> = [||]

    let mutable recTable:_[] = null

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

    let recognitionTable (_,_) (s:Microsoft.FSharp.Core.uint16[]) weightCalcFun =

        nTermsCount <- 
            rules
            |> Array.map(fun r -> 
                            let rule = getRuleStruct r
                            rule.RuleName)
            |> Set.ofArray
            |> Set.count

        rowSize <- s.Length
        recTable <- Array.init (rowSize * rowSize * nTermsCount) ( fun _ -> createEmptyCellData () )

        (*
        let elem2 i len symRuleArr = 
            // foreach symbol in grammar in parallel
            symRuleArr
            |> Array.Parallel.iter (fun (item:SymbolRuleMapItem) ->
                                        // foreach rule r per symbol in parallel
                                        item.Rules
                                        |> Array.iter (
                                            fun curRule -> ()(*for k in 0..(len-1) do processRule curRule.Rule curRule.Index i k len*)
                                        )
            )
        *)
        let fillTable rulesIndexed =
          let gpuWork = new GPUWork(rowSize, nTermsCount, recTable, rules, rulesIndexed)
          // todo
          gpuWork.Run()
          gpuWork.Dispose()
          (*
          [|1..rowSize - 1|]
          |> Array.iter (fun len ->
                [|0..rowSize - 1 - len|] // for start = 0 to nWords - length in parallel
                |> Array(*.Parallel*).iter (fun i -> 
                    ((*elem i len rulesIndexed*))
                )
         *)
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
                        recTable.[(0 * rowSize + k - calcDiff 0) * nTermsCount + int rule.RuleName - 1] <- new CellData(currentElem,0u) (*|> Some*))   
        //printfn "total rules count %d" rules.Length
                             
        let ntrIndexes = new ResizeArray<_>() // non-terminal rules indexes array
        rules
        |> Array.iteri
            (fun ruleIndex rule ->
                let ruleStruct = getRuleStruct rule
                if ruleStruct.R2 <> 0us then 
                    ntrIndexes.Add ruleIndex )
        let nonTermRules = Array.init ntrIndexes.Count (fun i -> new RuleIndexed(rules.[ntrIndexes.[i]], ntrIndexes.[i]) )        
        //printfn "non terminal rules count %d" nonTermRules.Length
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
        
        let printTbl () =
            for i in 0..s.Length-1 do
                for j in 0..s.Length-1 do
                    let startIndex = (i * rowSize + j) * nTermsCount
                    let mutable count = 0
                    for m in startIndex..startIndex + nTermsCount - 1 do
                        if not (isCellDataEmpty (recTable.[m])) then count <- count + 1
                    printf "! %s !" (string count)
                printfn " "
            printfn "" 
            
        let getString state lbl weight = 
            let stateString = 
                match state with
                |LblState.Defined -> "defined"
                |LblState.Undefined -> "undefined"
                |LblState.Conflict -> "conflict"
                |_ -> ""

            String.concat " " [stateString; ":"; "label ="; lblString lbl; "weight ="; string weight]
            
        let rec out i last =
            let index = ( (s.Length-1) * rowSize + 0 - calcDiff (s.Length-1) ) * nTermsCount + i
            if i <= last 
            then
                let cellData = recTable.[index] 
                if not (isCellDataEmpty (cellData))
                then
                    let cellData = getCellDataStruct (cellData)
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
                match isCellDataEmpty x with
                | false ->
                    let ind,lSt,lbl,_ = getData x.rData
                    let curRule = getRuleStruct rules.[int ind]
                    if curRule.RuleName = ruleCheck then
                        Some (Some x)
                    else checkIndex start index tryFind ruleCheck
                | true -> checkIndex start index tryFind ruleCheck

            let startLeft = ( leftL * rowSize + leftI - calcDiff leftL ) * nTermsCount
            let left = tryFind startLeft startLeft rule.R1

            let startRight = ( rightL * rowSize + rightI - calcDiff rightL ) * nTermsCount
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
                        trackLabel leftI leftL left true
                        trackLabel rightI rightL right true
                | _ -> ()
            | _ ->
                if flag && rule.Label <> noLbl
                then print curL curW leftI rightL leftL
            
    let labelTracking lastInd = 
        let i,l = 0,lastInd
        let startIndex = ( l * rowSize + i - calcDiff l ) * nTermsCount
        let derivationNum = ref 0
        for ind in startIndex..startIndex + nTermsCount - 1 do
            derivationNum := !derivationNum + 1
            if not (isCellDataEmpty recTable.[ind])
            then 
                let x = recTable.[ind]
                printfn "derivation #%d" !derivationNum
                trackLabel i l x false
            
    
    member this.Recognize ((grules, start) as g) s weightCalcFun lblNames = 
        rules <- grules
        lblNameArr <- lblNames
        // Info about dialects of derivation in format: "<lblState> <lblName> <weight>"
        // If dialect undefined or was conflict lblName = "0" 
        let out = recognize g s weightCalcFun |> List.filter ((<>)"") |> String.concat "\n"
        match out with
        | "" -> "Строка не выводима в заданной грамматике."
        | _ -> 
            labelTracking (s.Length - 1)
            out