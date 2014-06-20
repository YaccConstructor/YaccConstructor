namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

//(32        |16       |8       |8        )
//(ruleIndex |lblState |lblName |lblWeght )
//..
type tblData = uint64

type coordinate = uint32

type symbol = Microsoft.FSharp.Core.uint16 

[<Struct>]
type CellData =
    val mutable rData : tblData
    val mutable _k : coordinate
    new (r, k) = {rData=r;_k=k}

type LblState =
     | Defined = 0
     | Undefined = 1
     | Conflict = 2

[<Struct>]
type CellDataInside =
    val LabelState : LblState
    val Label : byte
    val Weight : byte
    new (lblState, lbl, weight) = { LabelState = lblState; Label = lbl; Weight = weight }

[<Struct>]
type LabelWithState = 
    val mutable Label : byte
    val mutable State : LblState
    new (lbl, lblState) = { Label = lbl; State = lblState }

[<Struct>]
type RuleIndexed = 
    val mutable Rule : rule
    val mutable Index : int
    new (rl, ind) = { Rule = rl; Index = ind }


[<Struct>]
type SymbolRuleMapItem =
    val Symbol : symbol
    val Rules : RuleIndexed[]
    new (symbol : symbol, rules : RuleIndexed[]) = 
        { Symbol = symbol; Rules = rules }

[<AutoOpen>]
module CellHelpers =
    
    let buildData rNum (lState:LblState) (lblName:byte) (lblWeight:byte) =
        let lbl = (uint16 lblName <<< 8) ||| uint16 lblWeight
        let r2 = (uint32 lState <<< 16) ||| uint32 lbl
        let r =  (uint64 rNum <<< 32) ||| uint64 r2
        r

    let getData (rule:tblData) =
        let rNum,r2 = uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL), uint32 (rule &&& 0xFFFFFFFFUL)
        let lState,lbl = uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), uint16 (r2 &&& 0xFFFFFFFFu)
        let lblName,lblWeight = uint8 ((lbl >>> 8) &&& 0xFFus), uint8 (lbl &&& 0xFFFFus)
        let lblState =
            match lState with
            | 0us -> LblState.Defined
            | 1us -> LblState.Undefined
            | 2us -> LblState.Conflict
            | _ -> failwith "Unexpected lblState value!"
        rNum, lblState, lblName, lblWeight

    // возвращает i-ые состояние метки, метку и вес массива указанной ячейки
    let getCellDataCortege (cellData:CellData) =
        let _,curlblState,curcl,curcw = getData cellData.rData
        curlblState,curcl,curcw

    let getCellDataStruct(cellData:CellData) = 
        let _,curlblState,curcl,curcw = getData cellData.rData
        new CellDataInside(curlblState, curcl, curcw)

    // возвращает нетерминал A правила A->BC, правило из i-го элемента массива указанной ячейки
    let getCellRuleTop (cellData:CellData) (rules:rule[]) =
        let curRuleNum,_,_,_ = getData cellData.rData
        let rule = getRuleStruct rules.[int curRuleNum]
        rule.RuleName

    // возвращает координаты дочерних ячеек 
    // i l - координаты текущей ячейки
    // k - число, определяющее координаты
    let getSubsiteCoordinates i l k =
        (i,k),(k+i+1,l-k-1)

[<AutoOpen>]
module CommonHelpers =

    [<Literal>]
    let noLbl = 0uy

    let initSymbol (s:uint16) : symbol = 
        s

    let createEmptyCellData () = 
        new CellData(System.UInt64.MaxValue, 0ul)

    let isCellDataEmpty (cd:CellData) = 
        cd.rData = System.UInt64.MaxValue && cd._k = 0ul

    let toState value = 
        match value with
        | 0 -> LblState.Defined
        | 1 -> LblState.Undefined
        | 2 -> LblState.Conflict
        | _ -> failwith "Unexpected label state value" 