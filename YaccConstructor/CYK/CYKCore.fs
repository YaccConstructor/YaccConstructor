namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

//(32        |16       |8       |8        )
//(ruleIndex |lblState |lblName |lblWeght )
type tblData = uint64

[<Struct>]
type CellData =
    val rData : tblData
    val _k : uint32
    new (r, k) = {rData=r;_k=k}

type CellContent = 
    struct
        val Data: tblData
        val K: int
        new(data: tblData, k: int) = {Data = data; K = k}
    end

type LblState =
     | Defined = 0
     | Undefined = 1
     | Conflict = 2

[<AutoOpen>]
module TableDataHelpers =
    
    let buildData rNum lState (lblName:byte) lblWeight =
        let lbl = (uint16 lblName <<< 8) ||| uint16 lblWeight
        let r2 = (uint32 lState <<< 16) ||| uint32 lbl
        let r =  (uint64 rNum <<< 32) ||| uint64 r2
        r

    let getData (rule:tblData) =
        let rNum,r2 = uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL), uint32 (rule &&& 0xFFFFFFFFUL)
        let lState,lbl = uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), uint16 (r2 &&& 0xFFFFFFFFu)
        let lblName,lblWeight = uint8 ((lbl >>> 8) &&& uint16 0xFFFFFFFFu), uint8 (lbl &&& uint16 0xFFFFFFFFu)
        let lblState =
            match lState with
            | 0us -> LblState.Defined
            | 1us -> LblState.Undefined
            | 2us -> LblState.Conflict
            | _ -> failwith "Unexpected lblState value!"
        rNum, lblState, lblName, lblWeight

type CYKCore() =

    // правила грамматики, инициализируются в Recognize
    let mutable rules: rule[] = null

    let mutable recTable = null

    [<Literal>]
    let noLbl = 0uy

    // возвращает нетерминал A правила A->BC, правило из i-го элемента массива указанной ячейки
    let getCellRuleTop (cellContent:ResizeArray<CellContent>) i =
        let curRuleNum,_,_,_ = getData cellContent.[i].Data
        let curNT,_,_,_,_ = getRule rules.[(int)curRuleNum]
        curNT

    // возвращает i-ые состояние метки, метку и вес массива указанной ячейки
    let getCellData (cellContent:ResizeArray<CellContent>) i =
        let _,curlblState,curcl,curcw = getData cellContent.[i].Data
        curlblState,curcl,curcw

    // возвращает координаты дочерних ячеек 
    // i l - координаты текущей ячейки
    // k - число, определяющее координаты
    let getSubsiteCoordinates i l k =
        (i,k),(k+i+1,l-k-1)

    let recognitionTable (_,_) (s:uint16[]) weightCalcFun =

        recTable <- Microsoft.FSharp.Collections.Array2D.create s.Length s.Length (new ResizeArray<_>())

        let chooseNewLabel (ruleLabel:uint8) (lbl1:byte) (lbl2:byte) lState1 lState2 =
            let conflictLbl = (noLbl,LblState.Conflict)
            match lState1,lState2 with
            |LblState.Conflict,_ -> conflictLbl
            |_,LblState.Conflict -> conflictLbl
            |LblState.Undefined,LblState.Undefined when ruleLabel = 0uy -> noLbl,LblState.Undefined
            |_ ->
                let notEmptyLbls = Array.filter ((<>) noLbl) [|lbl1;lbl2;ruleLabel|]
                if notEmptyLbls.Length = 1
                then notEmptyLbls.[0],LblState.Defined
                elif Array.forall ((=)notEmptyLbls.[0]) notEmptyLbls
                then notEmptyLbls.[0],LblState.Defined
                else noLbl,LblState.Conflict

        let processRule (rule:rule) i k l =
            let a,b,c,rl,rw = getRule rule
            let ruleIndex = Array.findIndex ((=)rule) rules
            match c with
            |v when v<>0us ->
                let left = recTable.[i,k]
                let right = recTable.[k+i+1,l-k-1]
                let count1 = left.Count
                let count2 = right.Count
                if (count1 > 0) && (count2 > 0)
                then
                    for m in 0..(count1 - 1) do
                        for n in 0..(count2 - 1) do
                            if (getCellRuleTop left m = b) && (getCellRuleTop right n = c)
                            then
                                let lState1,lbl1,weight1 = getCellData left m
                                let lState2,lbl2,weight2 = getCellData right n
                                let newLabel,newlState = chooseNewLabel rl lbl1 lbl2 lState1 lState2
                                let newWeight = weightCalcFun rw weight1 weight2
                                let currentElem = buildData ruleIndex newlState newLabel newWeight
                                let updatedData = ResizeArray.append (recTable.[i,l]) (ResizeArray.ofArray [|new CellContent(currentElem,k)|])
                                recTable.[i,l] <- updatedData
            |_ -> ()   

        let elem i l = rules |> Array.iter (fun rule -> for k in 0..(l-1) do processRule rule i k l)

        let rec fillTable i l =
                if l = s.Length-1
                then elem i l
                elif i+l <= s.Length-1
                then
                     elem i l
                     fillTable (i+1) l
                else
                     fillTable 0 (l+1)

        for rule in rules do
            for k in 0..(s.Length-1) do
                let a,b,c,rl,rw = getRule rule
                let ruleIndex = Array.findIndex ((=)rule) rules
                match (int)c with
                |0 -> if b = s.[k] then
                        let lState =
                            match rl with
                            | 0uy -> LblState.Undefined
                            | _   -> LblState.Defined
                        let currentElem = buildData ruleIndex lState rl rw
                        let updatedArray = ResizeArray.append (recTable.[k,0]) (ResizeArray.ofArray [|new CellContent(currentElem,-1)|])
                        recTable.[k,0] <- updatedArray
                |_ -> ()

        fillTable 0 1

        recTable

    // на вход подаем элемент списка ячейки i l
    let rec trackConflict (cell:CellContent) i l =
         let (leftI,leftL),(rightI,rightL) = getSubsiteCoordinates i l cell.K
         let ruleIndex,_,_,_ = getData cell.Data
         let _,b,c,_,_ = getRule rules.[(int)ruleIndex]
         let left:ResizeArray<CellContent> = recTable.[leftI,leftL]
         let right:ResizeArray<CellContent> = recTable.[rightI,rightL]

         ResizeArray.iter (fun (a:CellContent) -> 
                                let ind,state,_,_ = getData a.Data
                                let curA,_,_,_,_ = getRule rules.[int ind]
                                if (curA = b && state = LblState.Conflict)
                                then trackConflict a leftI leftL
                                else ()) left

    let recognize ((grules, start) as g) s weightCalcFun =
        recTable <- recognitionTable g s weightCalcFun
        (*
        let rec derivation i l top = 
            let nonterminals,bs,cs,rls,rws,lStates,cls,cws = getCellData recTable.[i,l]
            let leftI,leftL,rightI,rightL = getCellCoordinates recTable.[i,l]
            let suitable = ResizeArray.mapi (fun i a -> match a with
                                                        |top -> i) nonterminals
            //for k in 0..(suitable.Count-1) do
            let k = 0
            let curInd = suitable.[k]
            let curRule = top,bs.[curInd],cs.[curInd],cls.[curInd],cws.[curInd]
            let leftNT,rightNT = bs.[curInd],cs.[curInd]
            match i,l with
            |_,0 -> [curRule]
            |_ ->  curRule::((derivation leftI leftL leftNT)@(derivation rightI rightL rightNT))
                    
        let startNTs,_,_,_,_,_,_,_ = getCellData recTable.[0,s.Length-1]
        let result = if ResizeArray.exists((=) start) startNTs
                     then derivation 0 (s.Length-1) start
                     else []

        result
        *)

        let getString state (lbl:uint8) (weight:uint8) : string = 
            let stateString = 
                match state with
                |LblState.Defined -> "defined"
                |LblState.Undefined -> "undefined"
                |LblState.Conflict -> "conflict"
                |_ -> ""

            String.concat " " [stateString;(lbl.ToString());(weight.ToString())]
            
        let rec out i last = 
            if (i <= last)
            then let state,lbl,weight = getCellData (recTable.[0, s.Length-1]) i
                 match i with
                 |v when (v = last) -> getString state lbl weight
                 |_ -> String.concat "\n" [(getString state lbl weight); out (i+1) last]
            else ""

        out 0 (recTable.[0,s.Length-1].Count - 1)

    member this.Recognize ((grules, start) as g) s weightCalcFun = 
        rules <- grules
        recognize g s weightCalcFun 