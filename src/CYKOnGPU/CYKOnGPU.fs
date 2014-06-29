namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

type CYKOnGPU(platformName, debug) =

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
                
    let calcDiff columnIndex = columnIndex * (columnIndex - 1) / 2

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

        let printTbl () =
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
                                    let startIndex = i * nTermsCount
                                    let mutable count = 0
                                    for m in startIndex..startIndex + nTermsCount - 1 do
                                        let isEmpty = recTable.[m].rData = System.UInt64.MaxValue && recTable.[m]._k = 0ul
                                        if not isEmpty then count <- count + 1
                                    printf " %d |" count
                                    if needNewLine i then printfn "")

        let fillTable (nTermRuleMap:SymbolRuleMapItem[]) indexesBySymbols biggestSymbol =          
          if (debug) then
              let cpuWork = new CPUWork(rowSize, nTermsCount, recTable, rules, indexesBySymbols, biggestSymbol)
              [|1..rowSize - 1|]
              |> Array.iter (fun l -> 
                  [|0..rowSize - 1|]
                  |> Array.Parallel.iter ( fun i -> [|0..nTermRuleMap.Length - 1|] |> Array.Parallel.iter (fun s -> cpuWork.Run l i s) )
              )          
          else
              let gpuWork = new GPUWork(rowSize, nTermsCount, recTable, rules, platformName, indexesBySymbols, biggestSymbol)
              [|1..rowSize - 1|]
              |> Array.iter (fun l -> gpuWork.Run l)          
              gpuWork.Finish()
              gpuWork.Dispose()

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
        
        let nTermRuleMap = 
            nonTermRules
            |> Seq.groupBy (fun rule -> initSymbol (getRuleStruct rule.Rule).RuleName )
            |> Map.ofSeq
            |> Map.map (fun k v -> Array.ofSeq v)
            |> Map.toArray
            |> Array.map (fun (sym,rules) -> 
                            //printfn "Symbol %d rules count: %d" sym rules.Length
                            new SymbolRuleMapItem(sym, Array.init rules.Length (fun i -> rules.[i].Index)))

        let biggestSymbol = 
            (   
                nTermRuleMap
                |> Array.maxBy (fun pair -> pair.RulesCount)
            ).RulesCount
        printfn "biggest symbol size %d" biggestSymbol

        let indexesBySymbols = Array.init (nTermRuleMap.Length * biggestSymbol) (fun i -> 
                                                                                    let symbol = i / biggestSymbol
                                                                                    let symRules = nTermRuleMap.[symbol]
                                                                                    let ruleIndex = i % biggestSymbol
                                                                                    if (ruleIndex < symRules.RulesCount) then symRules.RuleIndexes.[ruleIndex]
                                                                                    else -1
                                                                                )
        
        let fillStart = System.DateTime.Now
        printfn "Fill table started %s" (string fillStart)
        fillTable nTermRuleMap indexesBySymbols biggestSymbol
        let fillFinish = System.DateTime.Now
        printfn "Fill table finished %s [%s]" (string fillFinish) (string (fillFinish - fillStart))
        
        printTbl()

        recTable

    let recognize ((grules, start) as g) s weightCalcFun =
        let recTable = recognitionTable g s weightCalcFun
            
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

        let lastIndex = nTermsCount - 1        
        out 0 lastIndex

    let print lblValue weight leftI rightL leftL =
        let out = String.concat " " ["label ="; lblString lblValue; "weight ="; string weight; 
                    "left ="; string leftI; "right ="; string (leftI + rightL + leftL + 1)]
        printfn "%s" out

    let rec trackLabel i l (cell:CellData)  flag =
        let ruleInd,_,curL,curW = getData cell.rData
        let rule = getRuleStruct rules.[int ruleInd]
        let (leftI,leftL),(rightI,rightL) = getSubsiteCoordinates i l (int cell._k)
        if l = 0 then 
            if curL <> noLbl then print curL curW leftI rightL leftL
        else 
            let checkIndex start ind tryFind ruleCheck =
                if ind < start + nTermsCount - 1 then tryFind start (ind + 1) ruleCheck
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
        | "" -> "The string is not derivable in specified grammar"
        | _ -> 
            labelTracking (s.Length - 1)
            out
