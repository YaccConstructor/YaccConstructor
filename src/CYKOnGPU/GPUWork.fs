namespace Yard.Generators.CYKGenerator

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

open System.Collections.Generic

type GPUWork(rowSize, nTermsCount, extRecTable:_[], extRules, extRulesIndexed:_[](*, indexes:_[]*)) =

    let command = 
        <@
            fun (grid:_1D) (rulesIndexed:_[]) (table:CellData[]) (rules:uint64[]) (rulesIndexedLenNum:_[]) (*(tableLenNum:_[])*) (rowSizeNum:_[]) (nTermsCountNum:_[]) (lNum:_[]) (*(indexes:_[])*)-> 
                let i = grid.GlobalID0 // column number, may be accessed parallel
                let l = lNum.[0] // row number, must be accessed sequentially
                let rowSize = rowSizeNum.[0]
                    
                // check is this cell in needed matrix part
                if (i <= (rowSize - 1 - l)) then
                    let nTermsCount = nTermsCountNum.[0] 
                    let ruleCount = rulesIndexedLenNum.[0]
                    let noLbl = 0uy
                    
                    for r in 0..(ruleCount - 1) do // k is rule number. this level also may be parallized 
                        let curRuleIndexed:RuleIndexed = rulesIndexed.[r]
                        let currentRule = curRuleIndexed.Rule
                        let ruleIndex = curRuleIndexed.Index
                        
                        for k in 0..(l - 1) do                                                    
                            (* process rule *)
                            (* get r2 *)
                            let r2 = Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL)
                            let r2 = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                            if r2 <> 0us then
                                (* calc diff *)
                                let index = k
                                let diffBuf = ref 0
                                let mutable leftDiff = 0 
                                if index >= 2 then 
                                    for i in 1..index-1 do
                                        diffBuf := (!diffBuf + i)
                                    leftDiff <- !diffBuf
                                let leftStart = ( k * rowSize + i - leftDiff ) * nTermsCount
                                (* calc diff *)
                                let index = l-k-1
                                let diffBuf = ref 0
                                let mutable rightDiff = 0 
                                if index >= 2 then 
                                    for i in 1..index-1 do
                                        diffBuf := (!diffBuf + i)
                                    rightDiff <- !diffBuf
                                let rightStart = ( (l-k-1) * rowSize + k+i+1 - rightDiff ) * nTermsCount
                                for m in 0..nTermsCount - 1 do
                                    let leftCell:CellData = table.[leftStart + m]
                                    (* get r1 *)
                                    let r1 = Microsoft.FSharp.Core.Operators.uint32 ((currentRule >>> 32) &&&  0xFFFFFFFFUL)
                                    let r1 = Microsoft.FSharp.Core.Operators.uint16 (r1 &&& 0xFFFFFFFFu)
                                    let isLeftEmpty = leftCell.rData = System.UInt64.MaxValue && leftCell._k = 0ul
                                    if not isLeftEmpty then
                                        (* get rule num *)
                                        let leftRuleNum = int ((leftCell.rData >>> 32) &&&  0xFFFFFFFFUL)                                        
                                        (* get rule name *)
                                        //printfn "l r n %d" leftRuleNum
                                        let leftRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[leftRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
                                        (* get cell rule top *)
                                        let leftTop = Microsoft.FSharp.Core.Operators.uint16 ((leftRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
                                        (* is cell data empty *)
                                        if leftTop = r1 then
                                            for n in 0..nTermsCount - 1 do
                                                let rightCell = table.[rightStart + n]
                                                let isRightEmpty = rightCell.rData = System.UInt64.MaxValue && rightCell._k = 0ul
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
                                                        let buf = Microsoft.FSharp.Core.Operators.uint16 (uint32 (currentRule &&& 0xFFFFFFFFUL) &&& 0xFFFFFFFFu)
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
                                                        (* calc diff *)
                                                        let diffBuf = ref 0
                                                        let mutable diff = 0 
                                                        if l >= 2 then 
                                                            for i in 1..l-1 do
                                                                diffBuf := (!diffBuf + i)
                                                            diff <- !diffBuf
                                                        let index = ( l * rowSize + i - diff ) * nTermsCount + ruleName - 1                                                         
                                                        //printfn "%d[%d]" leftRuleNum index
                                                        //printfn "%d[%d]" rightRuleNum (index + 1)                               
                                                        table.[index].rData <- currentElem
                                                        table.[index]._k <- Microsoft.FSharp.Core.Operators.uint32 k
                    
        @>

    let platformName = "*"//"AMD*"
    let deviceType = DeviceType.Default
    
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
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
    let rulesIndexed = extRulesIndexed //fillArray extRulesIndexed createEmptyRuleIndexed
        
    let rules = extRules //fillArray extRules (fun () -> uint64 0)
    
    member this.Run(l) = 
        let str = ref ""
        let kernel, kernelPrepare, kernelRun = provider.Compile(command,_outCode = str)
        printfn "%s" !str
        let d = new _1D(rowSize, rowSize)
        kernelPrepare d rulesIndexed recTable rules [|rulesIndexed.Length|] (*[|realRecTableLen|]*) [|rowSize|] [|nTermsCount|] [|l|] //indexes
        let _ = commandQueue.Add(kernelRun()).Finish() |> ignore
        ()

    member this.Finish() =
        let _ = commandQueue.Add(recTable.ToHost(provider)).Finish()
        commandQueue.Finish() |> ignore
        (*
        let printIndexes = 
            indexes
            |> Array.iteri (fun i el -> if el<> 0 then printfn "%d[%d]" el i)

        printIndexes
        *)

    member this.Dispose() =
        commandQueue.Dispose()
        provider.CloseAllBuffers()
        provider.Dispose()
        


