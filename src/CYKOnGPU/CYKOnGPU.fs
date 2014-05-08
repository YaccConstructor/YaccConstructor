namespace Yard.Generators.CYKGenerator

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

open System.Collections.Generic

type GPUWork(extRowSize, extNTermsCount, extRecTable:_[], extRules, extRulesIndexed:_[]) =

    let rowSize = extRowSize
    let nTermsCount = extNTermsCount
    let mutable recTable = extRecTable
    let rules = extRules
    let mutable rulesIndexed = extRulesIndexed
    
    let platformName = "AMD*"
    let deviceType = DeviceType.Default
    
    // Configure provider
    // device provider creating
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message

    // command queue creating:
    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let run = 
        printfn "Using %A" provider
        
        let lws,ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
        let maxLocalWorkSize = 1// int <| lws.CastTo<uint64>()
        //printfn "Local memory size: %d" maxLocalWorkSize

        let fillArray (arr:'a[]) (initFun:unit -> 'a) =
            let count = arr.Length
            if count % maxLocalWorkSize <> 0 
            then 
                let div = count / maxLocalWorkSize
                Array.init ((div + 1) * maxLocalWorkSize) (fun i -> if i < count then arr.[i]
                                                                    else initFun())
            else arr
        
        let realRecTableLen = recTable.Length
        printfn "real rec table len: %d" realRecTableLen
        recTable <- fillArray recTable createEmptyCellData        
        printfn "rec table len: %d" recTable.Length                          
        
        let createEmptyRuleIndexed () =
            new RuleIndexed((buildRule 0 0 0 0 0),0)

        let realRulesIndexedLen = rulesIndexed.Length
        printfn "real rules indexed len: %d" realRulesIndexedLen
        rulesIndexed <- fillArray rulesIndexed createEmptyRuleIndexed
        printfn "rules indexed len: %d" rulesIndexed.Length

        // Write quotation
        let command = 
            <@
                fun (r:_3D) (rulesIndexed:_[]) (recTable:_[]) (rules:rule[]) -> 
                    let tx = r.GlobalID0 // len
                    let ty = r.GlobalID1 // i

                    let i = ty
                    let len = tx

                    if (len <= realRulesIndexedLen && i <= realRecTableLen)
                    then                     
                        let calcDiff columnIndex =
                            if columnIndex < 2
                            then 0
                            else 
                                let diff = ref 0
                                for i in 1..columnIndex-1 do
                                    diff := (!diff + i)
                                !diff

                        let isCellDataEmpty (cd:CellData) = 
                            cd.rData = System.UInt64.MaxValue && cd._k = 0ul

                                
                        let getRuleName (rule:rule) = 
                            let r1 = Microsoft.FSharp.Core.Operators.uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL)
                            let rName = Microsoft.FSharp.Core.Operators.uint16 ((r1 >>> 16) &&& 0xFFFFFFFFu)
                            rName
                            
                        let getR1 (rule:rule) =
                            let r1 = Microsoft.FSharp.Core.Operators.uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL)
                            let r1 = Microsoft.FSharp.Core.Operators.uint16 (r1 &&& 0xFFFFFFFFu)
                            r1

                        let getR2 (rule:rule) =
                            let r2 = uint32 (rule &&& 0xFFFFFFFFUL)
                            let r2 = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                            r2

                        let getRuleLabel (rule:rule) =
                            let r2 = uint32 (rule &&& 0xFFFFFFFFUL)
                            let r2,lbl = // ?
                                Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), 
                                Microsoft.FSharp.Core.Operators.uint16 (r2 &&& 0xFFFFFFFFu)
                            let lblName = 
                                Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                            lblName

                        let getRuleWeight (rule:rule) =
                            let r2 = uint32 (rule &&& 0xFFFFFFFFUL)
                            let r2,lbl = // ?
                                Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), 
                                Microsoft.FSharp.Core.Operators.uint16 (r2 &&& 0xFFFFFFFFu)
                            let lblWeight = 
                                Microsoft.FSharp.Core.Operators.byte (lbl &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                            lblWeight

                        let getRuleNum (rule:tblData) =
                            let rNum = Microsoft.FSharp.Core.Operators.uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL)
                            rNum

                        let getLblState (rule:tblData) = 
                            let r2 = Microsoft.FSharp.Core.Operators.uint32 (rule &&& 0xFFFFFFFFUL)
                            let lState = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                            let lblState =
                                match lState with
                                | 0us -> LblState.Defined
                                | 1us -> LblState.Undefined
                                | 2us -> LblState.Conflict
                                | _ -> failwith "Unexpected lblState value!"
                            lblState

                        let getCellLabel(rule:tblData) = 
                            let r2 = Microsoft.FSharp.Core.Operators.uint32 (rule &&& 0xFFFFFFFFUL)
                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (r2 &&& 0xFFFFFFFFu)
                            let lblName = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                            lblName

                        let getCellWeight (rule:tblData) = 
                            let r2 = Microsoft.FSharp.Core.Operators.uint32 (rule &&& 0xFFFFFFFFUL)
                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (r2 &&& 0xFFFFFFFFu)
                            let lblWeight = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                            lblWeight
                            
                        let getCellRuleTop (cellData:CellData) (rules:rule[]) =
                            let curRuleNum = getRuleNum cellData.rData
                            let ruleName = getRuleName rules.[int curRuleNum]
                            ruleName
                                                    
                        let buildData rNum (lState:LblState) (lblName:Microsoft.FSharp.Core.uint8) (lblWeight:Microsoft.FSharp.Core.uint8) =
                            let (lbl:Microsoft.FSharp.Core.uint16) = 
                                (Microsoft.FSharp.Core.Operators.uint16 lblName <<< 8) ||| Microsoft.FSharp.Core.Operators.uint16 lblWeight
                            let (r2:Microsoft.FSharp.Core.uint32) = (uint32 lState <<< 16) ||| uint32 lbl
                            let r =  (uint64 rNum <<< 32) ||| uint64 r2
                            r

                        let chooseNewLabel ruleLabel (lbl1:byte) (lbl2:byte) lState1 lState2 =
                            if lState1 = LblState.Conflict then [| int noLbl; int LblState.Conflict |] // new LabelWithState(noLbl, LblState.Conflict)
                            elif lState2 = LblState.Conflict then [| int noLbl; int LblState.Conflict |] // new LabelWithState(noLbl, LblState.Conflict)
                            elif lState1 = LblState.Undefined && lState2 = LblState.Undefined && ruleLabel = noLbl then [| int noLbl; int LblState.Undefined |] // new LabelWithState(noLbl, LblState.Undefined)
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
                                then [| int noLbl; int LblState.Defined |] // new LabelWithState(notEmptyLbl1, LblState.Defined)
                                else [| int noLbl; int LblState.Conflict |] // new LabelWithState(noLbl, LblState.Conflict)
                    
                        let processRule rule ruleIndex i k l =
                            let r2 = getR2 rule
                            if r2 <> 0us then
                                let leftStart = ( k * rowSize + i - calcDiff k ) * nTermsCount
                                let rightStart = ( (l-k-1) * rowSize + k+i+1 - (calcDiff (l-k-1)) ) * nTermsCount

                                for m in 0..nTermsCount - 1 do
                                    let leftCell:CellData = recTable.[leftStart + m]
                                    let r1 = getR1 rule
                                    if not (isCellDataEmpty (leftCell)) && getCellRuleTop leftCell rules = r1 then
                                        for n in 0..nTermsCount - 1 do
                                            let rightCell = recTable.[rightStart + n]
                                            if not (isCellDataEmpty (rightCell)) && getCellRuleTop rightCell rules = r2 then
                                                let label = getRuleLabel rule
                                                let label1 = getCellLabel leftCell.rData
                                                let lState1 = getLblState leftCell.rData
                                                let label2 = getCellLabel rightCell.rData
                                                let lState2 = getLblState rightCell.rData
                                                let lblWithState = chooseNewLabel label label1 label2 lState1 lState2
                                                
                                                let weight = getRuleWeight rule
                                                let weight1 = getCellWeight leftCell.rData
                                                let weight2 = getCellWeight rightCell.rData
                                                let newWeight = 0uy // weightCalcFun weight weight1 weight2
                                                
                                                let currentElem = buildData ruleIndex (toState (lblWithState.[1])) (byte lblWithState.[0]) newWeight
                                                let ruleName = int (getRuleName rule)
                                                let index = ( l * rowSize + i - calcDiff l ) * nTermsCount + ruleName - 1 
                                                recTable.[index].rData <- currentElem
                                                recTable.[index]._k <- uint32 k

                        for k in 0..(len-1) do
                            let curRule:RuleIndexed = rulesIndexed.[k]
                                                    
                            processRule curRule.Rule curRule.Index i k len
                    
            @>
    
        // Compile&Run
        // kernel function compilation
        let str = ref ""
        let kernel, kernelPrepare, kernelRun = provider.Compile(command,_outCode = str)
        printfn "%s" !str
        
        // computation grid configuration: 2D grid with size = rows*columns and local block with size=localWorkSize*localWorkSize
        let d =(new _3D(rulesIndexed.Length, recTable.Length, recTable.Length, maxLocalWorkSize, maxLocalWorkSize, maxLocalWorkSize))
        // Prepare kernel. Pass actual parameters for computation:
        kernelPrepare d rulesIndexed recTable rules
        // Add command into queue and finish it
        let _ = commandQueue.Add(kernelRun()).Finish()
    
        // Get result
        let _ = commandQueue.Add(recTable.ToHost(provider)).Finish()
        
        printfn "done."

    member this.Run() = run

    member this.Dispose() =
        // Releasing of resources
        commandQueue.Dispose()
        provider.Dispose()


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

        (* writed in command *)
        (*
        let elem i len rulesIndexed = 
            // foreach rule r in grammar in parallel
            rulesIndexed 
            |> Array.iter (fun (curRule:RuleIndexed) -> ()(*for k in 0..(len-1) do processRule curRule.Rule curRule.Index i k len*))
        *)
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