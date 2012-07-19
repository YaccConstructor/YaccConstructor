namespace Yard.Generators.CYKGenerator
//(32        |16       |8       |8        )
//(ruleIndex |lblState |lblName |lblWeght )
type tblData = uint64 

type lblState =
     | Defined = 0
     | Undefined = 1
     | Conflict = 2
    
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


type CYKCore(rules:ResizeArray<uint64>) =
    
    [<Literal>]
    let noLbl = 0uy

    let getCellData (cellContent:ResizeArray<uint64>,(_,_),(_,_)) =
            let ruleNums = new ResizeArray<uint32>()
            let lStates = new ResizeArray<uint16>()
            let cls,cws = new ResizeArray<uint8>(),new ResizeArray<uint8>()
            let cellRules = new ResizeArray<uint64>()
            let aS,bS,cS = new ResizeArray<uint16>(),new ResizeArray<uint16>(),new ResizeArray<uint16>()
            let rls,rws = new ResizeArray<uint8>(),new ResizeArray<uint8>()
        
            for i in 0..(cellContent.Count-1) do
                let currn,curls,curcls,curcws = getData cellContent.[i]
                ruleNums.[i] <- currn
                lStates.[i] <- curls
                cls.[i] <- curcls
                cws.[i] <- curcws

                cellRules.[i] <- rules.[(int)ruleNums.[i]]
                let cas,cbs,ccs,crls,crws = getRule cellRules.[i]
                aS.[i] <- cas
                bS.[i] <- cbs
                cS.[i] <- ccs
                rls.[i] <- crls
                rws.[i] <- crws 

            aS,bS,cS,rls,rws,lStates,cls,cws

    let getCellCoordinates (_,(li,ll),(ri,rl)) = 
        li,ll,ri,rl

    let recognitionTable (rules:ResizeArray<uint64>,_) (s:uint16[]) weightCalcFun =
        
        let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length (new ResizeArray<tblData>(),(0,0),(0,0))

        let chooseNewLabel (ruleLabel:uint8) (lbl1:byte) (lbl2:byte) (lState1:uint16) (lState2:uint16) = 
            let conflictLbl = (noLbl,lblState.Conflict)            
            match lState1,lState2,lbl1,lbl2,ruleLabel with
            |conflict,_,_,_,_ -> conflictLbl
            |_,conflict,_,_,_ -> conflictLbl
            |_,_,v1,v2,v3 when (v1 = noLbl && v2 = noLbl && v3 = noLbl) -> (noLbl,lblState.Undefined)
            |_,_,v1,v2,_  when (v1 = noLbl && v2 = noLbl)-> (ruleLabel,lblState.Defined)
            |_,_,v1,noLbl,v2 when (v2<>noLbl && v1<>v2) -> conflictLbl
            |_,_,noLbl,v1,v2 when (v2<>noLbl && v1<>v2) -> conflictLbl
            |_,_,v1,v2,_ when v1<>v2 -> conflictLbl
            |_,_,v1,v2,v3 when (v1=v2 && v3<>noLbl && v1<>v3) -> conflictLbl
            |_ -> ((List.find ((<>) noLbl) [lbl1;lbl2;ruleLabel]),lblState.Defined)

        
        let processRule (rule:uint64) i k l =
            let a,b,c,rl,rw = getRule rule
            let ruleIndex = ResizeArray.findIndex ((=)rule) rules
            match c with
            |v when v<>0us ->
                        let nonTerminals1,_,_,_,_,lStates1,lbls1,weights1= getCellData recTable.[i,k]
                        let nonTerminals2,_,_,_,_,lStates2,lbls2,weights2 = getCellData recTable.[k+i+1,l-k-1]
                        let nonTerminals,_,_,_,_,lStates,lbls,weights = getCellData recTable.[i,l]

                        if (nonTerminals1.Count > 0) && (nonTerminals2.Count > 0)
                        then
                            for m in 0..(nonTerminals1.Count - 1) do
                                for n in 0..(nonTerminals2.Count - 1) do
                                    if (nonTerminals1.[m] = b) && (nonTerminals2.[n] = c)
                                    then
                                        let newLabel,newlState = chooseNewLabel rl lbls1.[m] lbls2.[n] lStates1.[m] lStates2.[n]
                                        let newWeight = weightCalcFun rw weights1.[m] weights2.[n]
                                        let currentElem = buildData ruleIndex newlState newLabel newWeight
                                        let updatedArray = ResizeArray.append ((fun (array,(_,_),(_,_)) -> array) recTable.[i,l]) (ResizeArray.ofArray [|currentElem|])
                                        recTable.[i,l] <- updatedArray,(i,k),(k+i+1,l-k-1)    
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
                        let nonTerminals,_,_,_,_,lStates,lbls,weights = getCellData recTable.[k,0]
                        let lState =
                            match rl with
                            | 0uy -> lblState.Undefined
                            | _   -> lblState.Defined
                        let currentElem = buildData ruleIndex lState rl rw
                        let updatedArray = ResizeArray.append ((fun (array,(_,_),(_,_)) -> array) recTable.[k,0]) (ResizeArray.ofArray [|currentElem|])
                        recTable.[k,0] <- updatedArray,(-1,-1),(-1,-1)
                |_ -> ()
    
        fillTable 0 1

        recTable

    let recognize ((_, start) as g) s weightCalcFun =
        let recTable = recognitionTable g s weightCalcFun

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
    //let testG = (ResizeArray.ofList [buildRule (uint16)1 (uint16)2 (uint16)3 (uint8)11 0uy; buildRule (uint16)2 (uint16)4 (uint16)0 11uy 1uy; buildRule (uint16)3 (uint16)5 (uint16)0 11uy 0uy]


    