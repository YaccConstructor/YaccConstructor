namespace Yard.Generators.CYKGenerator

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

open System.Collections.Generic

type GPUWork(rowSize, nTermsCount, extRecTable:_[], extRules, platformName, _indexesBySymbols:_[], biggestSym) =

    let command = 
        <@
            fun (grid:_2D) (table:CellData[]) (rules:uint64[]) (*(tableLenNum:_[])*) (rowSizeNum:_[]) (nTermsCountNum:_[]) (lNum:_[]) (indexesBySymbols:int[]) (biggestSymNum:_[])-> 
                let i = grid.GlobalID0 // column index, may be accessed parallel
                let s = grid.GlobalID1 // symbol index
                let l = lNum.[0] // row index, must be accessed sequentially
                let rowSize = rowSizeNum.[0]
                    
                // check is this cell in needed matrix part
                if (i <= (rowSize - 1 - l)) then
                    let nTermsCount = nTermsCountNum.[0] 
                    let biggestSym = biggestSymNum.[0]
                    let symbolStart = s * biggestSym
                    let symbolEnd = s * biggestSym + biggestSym - 1
                    
                    let noLbl = 0uy
                    let maxValue = System.UInt64.MaxValue
                    
                    for r in symbolStart..symbolEnd do
                        let ruleIndex = indexesBySymbols.[r]
                        if ruleIndex >= 0 then
                            let currentRule = rules.[ruleIndex]
                        
                            for k in 0..(l - 1) do                                                    
                                (* process rule *)
                                (* get r2 *)
                                let r2 = Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL)
                                let r2 = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                                if r2 <> 0us then
                                    let index = k
                                    let leftDiff = index * (index - 1) / 2
                                    let leftStart = ( k * rowSize + i - leftDiff ) * nTermsCount
                                    let index = l-k-1
                                    let rightDiff = index * (index - 1) / 2 
                                    let rightStart = ( (l-k-1) * rowSize + k+i+1 - rightDiff ) * nTermsCount
                                    for m in 0..nTermsCount - 1 do
                                        let leftCell:CellData = table.[leftStart + m]
                                        (* get r1 *)
                                        let r1 = Microsoft.FSharp.Core.Operators.uint32 ((currentRule >>> 32) &&&  0xFFFFFFFFUL)
                                        let r1 = Microsoft.FSharp.Core.Operators.uint16 (r1 &&& 0xFFFFFFFFu)
                                        let isLeftEmpty = leftCell.rData = maxValue && leftCell._k = 0ul
                                        if not isLeftEmpty then
                                            (* get rule num *)
                                            let leftRuleNum = int ((leftCell.rData >>> 32) &&&  0xFFFFFFFFUL)                                        
                                            (* get rule name *)
                                            let leftRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[leftRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
                                            (* get cell rule top *)
                                            let leftTop = Microsoft.FSharp.Core.Operators.uint16 ((leftRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
                                            (* is cell data empty *)
                                            if leftTop = r1 then
                                                for n in 0..nTermsCount - 1 do
                                                    let rightCell = table.[rightStart + n]
                                                    let isRightEmpty = rightCell.rData = maxValue && rightCell._k = 0ul
                                                    if not isRightEmpty then
                                                        (* get rule num *)
                                                        let rightRuleNum = int ((rightCell.rData >>> 32) &&&  0xFFFFFFFFUL)                                        
                                                        (* get rule name *)
                                                        let rightRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[rightRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
                                                        (* get cell rule top *)
                                                        let rightTop = Microsoft.FSharp.Core.Operators.uint16 ((rightRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
                                                        (* is cell data empty *)             
                                                        if rightTop = r2 then
                                                        (* get rule label *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint16 (Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL) &&& 0xFFFFFFFFu)
                                                            let label = Microsoft.FSharp.Core.Operators.byte ((buf >>> 8) &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                                                            (* get cell label *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                            let label1 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                                            (* get label state *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                            let lState1Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                                            let lState1 = Microsoft.FSharp.Core.Operators.uint32 lState1Part
                                                            (* get cell label *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                            let label2 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                                            (* get label state *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                            let lState2Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                                            let lState2 = Microsoft.FSharp.Core.Operators.uint32 lState2Part
                                                            (* label states *)
                                                            let sDefined = 0u
                                                            let sUndefined = 1u
                                                            let sConflict = 2u
                                                
                                                            let newLabel = ref noLbl
                                                            let newState = ref sUndefined                                                
                                                            (* choose new label *)
                                                            if lState1 = sConflict then newLabel := noLbl; newState := sConflict
                                                            elif lState2 = sConflict then newLabel := noLbl; newState := sConflict
                                                            elif lState1 = sUndefined && lState2 = sUndefined && label = noLbl 
                                                            then newLabel := noLbl; newState := sUndefined
                                                            else
                                                                let mutable notEmptyLbl1 = noLbl
                                                                let mutable notEmptyLbl2 = noLbl
                                                                let mutable notEmptyLbl3 = noLbl 
                                                                let mutable realLblCount = 0
                                                                if label1 <> noLbl then 
                                                                    notEmptyLbl1 <- label1
                                                                    realLblCount <- realLblCount + 1
                                                                if label2 <> noLbl then
                                                                    if realLblCount = 0 then notEmptyLbl1 <- label2
                                                                    elif realLblCount = 1 then notEmptyLbl2 <- label2
                                                                    realLblCount <- realLblCount + 1
                                                                if label <> noLbl then 
                                                                    if realLblCount = 0 then notEmptyLbl1 <- label
                                                                    elif realLblCount = 1 then notEmptyLbl2 <- label
                                                                    elif realLblCount = 2 then notEmptyLbl3 <- label
                                                                    realLblCount <- realLblCount + 1
                                                                if realLblCount = 1 ||
                                                                    (realLblCount = 2 && notEmptyLbl2 = notEmptyLbl1) ||
                                                                    (realLblCount = 3 && notEmptyLbl2 = notEmptyLbl1 && notEmptyLbl3 = notEmptyLbl1)
                                                                then newLabel := notEmptyLbl1; newState := sDefined
                                                                else newLabel := noLbl; newState := sConflict
                                                
                                                            (* get rule weight *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint16 (Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL) &&& 0xFFFFFFFFu)
                                                            let weight = Microsoft.FSharp.Core.Operators.byte (buf &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                                                            (* get cell weight *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                            let weight1 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                                            (* get cell weight *)
                                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                            let weight2 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                                            let newWeight = weight + weight1 + weight2
                                                
                                                            (* build data *)
                                                            let currentLabel = 
                                                                (Microsoft.FSharp.Core.Operators.uint16 !newLabel <<< 8) 
                                                                ||| Microsoft.FSharp.Core.Operators.uint16 newWeight
                                                            let currentR2 = (Microsoft.FSharp.Core.Operators.uint32 !newState <<< 16) ||| Microsoft.FSharp.Core.Operators.uint32 currentLabel
                                                            let currentElem = (uint64 ruleIndex <<< 32) ||| uint64 currentR2

                                                            (* get rule name *)
                                                            let ruleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((currentRule >>> 32) &&&  0xFFFFFFFFUL)
                                                            (* get cell rule top *)
                                                            let ruleName = int (Microsoft.FSharp.Core.Operators.uint16 ((ruleNamePart >>> 16) &&& 0xFFFFFFFFu))
                                                            let diff = l * (l - 1) / 2
                                                            let index = ( l * rowSize + i - diff ) * nTermsCount + ruleName - 1                                                         
                                                            table.[index].rData <- currentElem
                                                            table.[index]._k <- Microsoft.FSharp.Core.Operators.uint32 k
                    
        @>

    let deviceType = DeviceType.Default
    
    let provider =
        try  
            let res = ComputeProvider.Create(platformName, deviceType)
            printfn "provider: %A" res
            res
        with 
        | ex -> failwith ex.Message

    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    //let lws,ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
    let maxLocalWorkSize = 1 // int <| lws.CastTo<uint64>()
        
    let fillArray (arr:'a[]) (initFun:unit -> 'a) =
        let count = arr.Length
        if count % maxLocalWorkSize <> 0 
        then 
            let div = count / maxLocalWorkSize
            Array.init ((div + 1) * maxLocalWorkSize) (fun i -> if i < count then arr.[i]
                                                                else initFun())
        else arr
        
    //let realRecTableLen = extRecTable.Length
    let recTable: CellData[] = extRecTable //fillArray extRecTable createEmptyCellData        
        
    let createEmptyRuleIndexed () =
        new RuleIndexed((buildRule 0 0 0 0 0),0)

    //let realRulesIndexedLen = extRulesIndexed.Length
    //let rulesIndexed = extRulesIndexed //fillArray extRulesIndexed createEmptyRuleIndexed
        
    let rules = extRules //fillArray extRules (fun () -> uint64 0)

    let indexesBySymbols = _indexesBySymbols

    let rowSizeArray = [| rowSize |]
    let nTermsCountArray = [| nTermsCount |]
    let biggestSymArray = [| biggestSym |]
    
    let kernel, kernelPrepare, kernelRun = 
        let str = ref ""
        let res = provider.Compile(command,_outCode = str)
        printfn "%s" !str
        res
        
    let d = new _2D(rowSize, indexesBySymbols.Length, 1, 1)
              
    member this.Run(l) = 
        kernelPrepare d recTable rules (*[|realRecTableLen|]*) rowSizeArray nTermsCountArray [| l |] indexesBySymbols biggestSymArray
        let _ = commandQueue.Add(kernelRun())
        ()

    member this.Finish() =
        let _ = commandQueue.Add(recTable.ToHost(provider)).Finish()
        commandQueue.Finish() |> ignore

    member this.Dispose() =
        commandQueue.Dispose()
        provider.CloseAllBuffers()
        provider.Dispose()

