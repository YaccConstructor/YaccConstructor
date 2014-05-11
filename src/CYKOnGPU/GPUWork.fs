namespace Yard.Generators.CYKGenerator

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

open System.Collections.Generic

type GPUWork(rowSize, nTermsCount, extRecTable:_[], extRules, extRulesIndexed:_[]) =

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

    // Write quotation
    let command = 
        <@
            fun (r:_3D) (rulesIndexed:_[]) (recTable:_[]) (rules:rule[]) (rulesIndexedLen:_[]) (recTableLen:_[]) (rowSizeArr:_[]) (nTermsCountArr:_[])-> 
                let tx = r.GlobalID0 // len
                let ty = r.GlobalID1 // i

                let i = ty
                let len = tx

                let realRulesIndexedLen = rulesIndexedLen.[0]
                let realRecTableLen = recTableLen.[0]
                let rowSize = rowSizeArr.[0]
                let nTermsCount = nTermsCountArr.[0]

                let noLbl = 0uy

                if (len <= realRulesIndexedLen && i <= realRecTableLen)
                then                     
                            
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
                       
                    for k in 0..(len-1) do
                        let curRule:RuleIndexed = rulesIndexed.[k]
                        let rule = curRule.Rule
                        let ruleIndex = curRule.Index
                        let l = len
                                                    
                        (* process rule *)
                        (* get r2 *)
                        let r2 = uint32 (rule &&& 0xFFFFFFFFUL)
                        let r2 = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                        if r2 <> 0us then
                            (* calc diff *)
                            let index = k
                            let diffBuf = ref 0
                            let leftDiff = 
                                if index < 2
                                then 0
                                else 
                                    for i in 1..index-1 do
                                        diffBuf := (!diffBuf + i)
                                    !diffBuf
                            let leftStart = ( k * rowSize + i - leftDiff ) * nTermsCount
                            (* calc diff *)
                            let index = l-k-1
                            let diffBuf = ref 0
                            let rightDiff = 
                                if index < 2
                                then 0
                                else 
                                    for i in 1..index-1 do
                                        diffBuf := (!diffBuf + i)
                                    !diffBuf
                            let rightStart = ( (l-k-1) * rowSize + k+i+1 - rightDiff ) * nTermsCount

                            for m in 0..nTermsCount - 1 do
                                let leftCell:CellData = recTable.[leftStart + m]
                                (* get r1 *)
                                let r1 = Microsoft.FSharp.Core.Operators.uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL)
                                let r1 = Microsoft.FSharp.Core.Operators.uint16 (r1 &&& 0xFFFFFFFFu)
                                (* get rule num *)
                                let leftRuleNum = int (uint32 ((leftCell.rData >>> 32) &&&  0xFFFFFFFFUL))
                                (* get rule name *)
                                let leftRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[leftRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
                                (* get cell rule top *)
                                let leftTop = Microsoft.FSharp.Core.Operators.uint16 ((leftRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
                                (* is cell data empty *)
                                let isLeftEmpty = leftCell.rData = System.UInt64.MaxValue && leftCell._k = 0ul
                                if not isLeftEmpty && leftTop = r1 then
                                    for n in 0..nTermsCount - 1 do
                                        let rightCell = recTable.[rightStart + n]
                                        (* get rule num *)
                                        let rightRuleNum = int (uint32 ((rightCell.rData >>> 32) &&&  0xFFFFFFFFUL))
                                        (* get rule name *)
                                        let rightRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[rightRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
                                        (* get cell rule top *)
                                        let rightTop = Microsoft.FSharp.Core.Operators.uint16 ((rightRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
                                        (* is cell data empty *)
                                        let isRightEmpty = rightCell.rData = System.UInt64.MaxValue && rightCell._k = 0ul
                                        if not isRightEmpty && rightTop = r2 then
                                            (* get rule label *)
                                            // todo move from func
                                            let label = getRuleLabel rule
                                            (* get cell label *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                            let label1 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                            (* get label state *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                            let lState1Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                            let lState1 = 
                                                if lState1Part = 0us then LblState.Defined
                                                elif lState1Part = 1us then LblState.Undefined
                                                elif lState1Part = 2us then LblState.Conflict
                                                else failwith "Unexpected lblState value!" 
                                            (* get cell label *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                            let label2 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                            (* get label state *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                            let lState2Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                            let lState2 = 
                                                if lState2Part = 0us then LblState.Defined
                                                elif lState2Part = 1us then LblState.Undefined
                                                elif lState2Part = 2us then LblState.Conflict
                                                else failwith "Unexpected lblState value!" 
                                            let newLabel = ref noLbl
                                            let newState = ref LblState.Undefined
                                                
                                            (* choose new label *)
                                            if lState1 = LblState.Conflict then newLabel := noLbl; newState := LblState.Conflict
                                            elif lState2 = LblState.Conflict then newLabel := noLbl; newState := LblState.Conflict
                                            elif lState1 = LblState.Undefined && lState2 = LblState.Undefined && label = noLbl 
                                            then newLabel := noLbl; newState := LblState.Undefined
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
                                                then newLabel := notEmptyLbl1; newState := LblState.Defined
                                                else newLabel := noLbl; newState := LblState.Conflict
                                                
                                            (* get rule weight *)
                                            // todo move from func
                                            let weight = getRuleWeight rule
                                            (* get cell weight *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                            let weight1 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                            (* get cell weight *)
                                            let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                            let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                            let weight2 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                            // todo weight calc
                                            let newWeight = 0uy // weightCalcFun weight weight1 weight2
                                                
                                            (* build data *)
                                            let currentLabel = 
                                                (Microsoft.FSharp.Core.Operators.uint16 !newLabel <<< 8) 
                                                ||| Microsoft.FSharp.Core.Operators.uint16 newWeight
                                            let currentR2 = (uint32 !newState <<< 16) ||| uint32 currentLabel
                                            let currentElem = (uint64 ruleIndex <<< 32) ||| uint64 currentR2

                                            (* get rule name *)
                                            let ruleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL)
                                            (* get cell rule top *)
                                            let ruleName = int (Microsoft.FSharp.Core.Operators.uint16 ((ruleNamePart >>> 16) &&& 0xFFFFFFFFu))
                                            (* calc diff *)
                                            let index = l
                                            let diffBuf = ref 0
                                            let currentDiff = 
                                                if index < 2
                                                then 0
                                                else 
                                                    for i in 1..index-1 do
                                                        diffBuf := (!diffBuf + i)
                                                    !diffBuf
                                            let index = ( l * rowSize + i - currentDiff ) * nTermsCount + ruleName - 1 
                                            recTable.[index].rData <- currentElem
                                            recTable.[index]._k <- uint32 k
                    
        @>

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
        
        let realRecTableLen = extRecTable.Length
        printfn "real rec table len: %d" realRecTableLen
        let recTable = fillArray extRecTable createEmptyCellData        
        printfn "rec table len: %d" recTable.Length                          
        
        let createEmptyRuleIndexed () =
            new RuleIndexed((buildRule 0 0 0 0 0),0)

        let realRulesIndexedLen = extRulesIndexed.Length
        printfn "real rules indexed len: %d" realRulesIndexedLen
        let rulesIndexed = fillArray extRulesIndexed createEmptyRuleIndexed
        printfn "rules indexed len: %d" rulesIndexed.Length
        
        let rules = fillArray extRules (fun () -> uint64 0)
            
        // Compile&Run
        // kernel function compilation
        let str = ref ""
        let kernel, kernelPrepare, kernelRun = provider.Compile(command,_outCode = str)
        printfn "%s" !str
        
        // computation grid configuration: 2D grid with size = rows*columns and local block with size=localWorkSize*localWorkSize
        let d =(new _3D(rulesIndexed.Length, recTable.Length, recTable.Length, maxLocalWorkSize, maxLocalWorkSize, maxLocalWorkSize))
        // Prepare kernel. Pass actual parameters for computation:
        kernelPrepare d rulesIndexed recTable rules [|realRulesIndexedLen|] [|realRecTableLen|] [|rowSize|] [|nTermsCount|]
        // Add command into queue
        let _ = commandQueue.Add(kernelRun())
    
        // Get result
        let _ = commandQueue.Add(recTable.ToHost(provider)).Finish()
        
        printfn "done."

    member this.Run() = run

    member this.Finish() =
        commandQueue.Finish()

    member this.Dispose() =
        // Releasing of resources
        commandQueue.Dispose()
        provider.Dispose()


