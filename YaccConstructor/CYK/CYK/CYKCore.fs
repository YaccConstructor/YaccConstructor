namespace Yard.Generators.CYKGenerator
//(32        |16       |8       |8        )
//(ruleIndex |lblState |lblName |lblWeght )
type tblData = uint64 

[<AutoOpen>]
module CellHelpers =
    
    let buildData rNum lState (lblName:byte) lblWeight =
        let lbl = (uint16 lblName <<< 8) ||| uint16 lblWeight
        let r2 = (uint32 lState <<< 16) ||| uint32 lbl
        let r =  (uint64 rNum <<< 32) ||| uint64 r2
        r

    let getData (rule:tblData) =
        let rNum,r2 = uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL), uint32 (rule &&& 0xFFFFFFFFUL)
        let lState,lbl = uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), uint16 (r2 &&& 0xFFFFFFFFu)
        let lblName,lblWeight = uint8 ((lbl >>> 8) &&& uint16 0xFFFFFFFFu), uint8 (lbl &&& uint16 0xFFFFFFFFu)
        rNum, lState, lblName, lblWeight

type CYKCore() =
    
    // правила грамматики, инициализируются в Recognize
    let mutable rules : ResizeArray<rule> = null

    // возвращает нетерминал A правила A->BC, правило из i-го элемента массива указанной ячейки
    let getCellRuleTop (cellContent:ResizeArray<tblData>,_) i =
        let curRuleNum,_,_,_ = getData cellContent.[i]
        let curNT,_,_,_,_ = getRule rules.[(int)curRuleNum]
        curNT

    // возвращает i-ые состояние метки, метку и вес массива указанной ячейки
    let getCellData (cellContent:ResizeArray<tblData>,_) i =
        let _,curlblState,curcl,curcw = getData cellContent.[i]
        curlblState,curcl,curcw

    // возвращает координаты дочерних ячеек 
    // i l - координаты текущей ячейки
    // k - число, определяющее координаты
    let getSubsiteCoordinates i l k =
        (i,k),(k+i+1,l-k-1)

    let recognitionTable (_,_) (s:uint16[]) weightCalcFun =
        
        let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length (new ResizeArray<tblData>(),new ResizeArray<int>())

        let chooseNewLabel (ruleLabel:uint8) (lbl1:byte) (lbl2:byte) lState1 lState2 = 
            let defined = 0us
            let undefined = 1us
            let conflict = 2us
            let nolbl = 0uy
            match lState1,lState2,lbl1,lbl2,ruleLabel with
            |conflict,_,_,_,_ -> (0uy,conflict)
            |_,conflict,_,_,_ -> (0uy,conflict)
            |_,_,v1,v2,v3 when (v1 = nolbl && v2 = nolbl && v3 = nolbl) -> (0uy,undefined)
            |_,_,v1,v2,_  when (v1 = nolbl && v2 = nolbl)-> (ruleLabel,defined)
            |_,_,v1,nolbl,v2 when (v2<>0uy && v1<>v2) -> (0uy,conflict)
            |_,_,nolbl,v1,v2 when (v2<>0uy && v1<>v2) -> (0uy,conflict)
            |_,_,v1,v2,_ when v1<>v2 -> (0uy,conflict)
            |_,_,v1,v2,v3 when (v1=v2 && v3<>0uy && v1<>v3) -> (0uy,conflict)
            |_ -> ((List.find ((<>) 0uy) [lbl1;lbl2;ruleLabel]),defined)

        
        let processRule (rule:uint64) i k l =
            let a,b,c,rl,rw = getRule rule
            let ruleIndex = ResizeArray.findIndex ((=)rule) rules
            match c with
            |v when v<>(uint16)0 ->
                        let left = recTable.[i,k]
                        let right = recTable.[k+i+1,l-k-1]
                        let count1 = (fun (array:ResizeArray<_>,_) -> array.Count) left
                        let count2 = (fun (array:ResizeArray<_>,_) -> array.Count) right
                        
                        if (count1 > 0) && (count2 > 0)
                        then
                            for m in 0..(count1 - 1) do
                                for n in 0..(count2 - 1) do
                                    if (getCellRuleTop left m = b) && (getCellRuleTop right n = (uint16)c)
                                    then
                                        let lState1,lbl1,weight1 = getCellData left m
                                        let lState2,lbl2,weight2 = getCellData right n
                                        let newLabel,newlState = chooseNewLabel rl lbl1 lbl2 lState1 lState2
                                        let newWeight = weightCalcFun rw weight1 weight2
                                        let currentElem = buildData ruleIndex newlState newLabel newWeight
                                        let updatedData = ResizeArray.append ((fun (array,_) -> array) recTable.[i,l]) (ResizeArray.ofArray [|currentElem|])
                                        let updatedCoord = ResizeArray.append ((fun (_,array) -> array) recTable.[i,l]) (ResizeArray.ofArray [|k|])
                                        recTable.[i,l] <- updatedData,updatedCoord    
            |_ -> ()   

        let elem i l = rules |> ResizeArray.iter (fun rule -> for k in 0..(l-1) do processRule rule i k l)

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
                let ruleIndex = ResizeArray.findIndex ((=)rule) rules
                match (int)c with
                |0 -> if b = s.[k] then
                        let lState =
                            let defined = (uint16)0
                            let undefined = (uint16)1 
                            match (int)rl with
                            |0 -> undefined
                            |_ -> defined
                        let currentElem = buildData ruleIndex lState rl rw
                        let updatedArray = ResizeArray.append ((fun (array,_) -> array) recTable.[k,0]) (ResizeArray.ofArray [|currentElem|])
                        recTable.[k,0] <- updatedArray,null
                |_ -> ()
    
        fillTable 0 1

        recTable

    let recognize ((grules, start) as g) s weightCalcFun =
        let recTable = recognitionTable g s weightCalcFun
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

        let getString (state:uint16) (lbl:uint8) (weight:uint8) : string = 
            let stateString = 
                match state with
                |0us -> "defined"
                |1us -> "undefined"
                |2us -> "conflict"
                |_ -> ""
            String.concat " " [stateString;(lbl.ToString());(weight.ToString())]
            
        let rec out i last= 
            if (i <= last)
            then let state,lbl,weight = getCellData recTable.[0, s.Length-1] i
                 match i with
                 |v when (v = last) -> getString state lbl weight
                 |_ -> String.concat "\n" [(getString state lbl weight); out (i+1) last]
            else ""

        let lastIndex = ((fun (array:ResizeArray<_>,_) -> array.Count) recTable.[0,s.Length-1]) - 1
        (string)(out 0 lastIndex)

    member this.Recognize ((grules, start) as g) s weightCalcFun = 
        rules <- grules
        recognize g s weightCalcFun 