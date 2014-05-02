namespace Yard.Generators.CYKGenerator

open System.Collections.Generic

type CYKCore() =

    // правила грамматики, инициализируются в Recognize
    let mutable rules : array<rule> = [||]

    let mutable recTable:Option<_>[][,] = null

    let mutable lblNameArr = [||]

    let mutable rowSize = 0

    let lblString lbl = 
        match lblNameArr with
        | [||] -> "0" 
        | _ -> 
                match lbl with 
                | 0uy -> "0"
                | _ -> lblNameArr.[(int lbl) - 1]

    let printTbl () =
            for i in 0..rowSize-1 do
                for j in 0..rowSize-1 do
                    let cd = recTable.[i,j] |> Array.filter (fun x -> x.IsSome) |> fun a-> a.Length
                    printf "! %s !" (string cd)
                printfn " "
            printfn "" 
    
    let recognitionTable (_,_) (s:uint16[]) weightCalcFun =

        let nTermsCount = 
            rules
            |> Array.map(fun r -> 
                            let a,_,_,_,_ = getRuleCortege r
                            a)
            |> Set.ofArray
            |> Set.count

        rowSize <- s.Length
        recTable <- Microsoft.FSharp.Collections.Array2D.init s.Length s.Length (fun _ _ -> Array.init nTermsCount (fun _ -> None))

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

        let processRule rule ruleIndex i k l =
            let a,b,c,rl,rw = getRuleCortege rule
            if c <> 0us then
                let left = recTable.[i, k] |> Array.choose id
                let right = recTable.[k+i+1, l-k-1] |> Array.choose id
                left |> Array.iter (fun lf ->
                    if getCellRuleTop lf rules = b
                    then
                        let lState1,lbl1,weight1 = getCellDataCortege lf
                        right |> Array.iter (fun r ->
                            if getCellRuleTop r rules = c
                            then
                                let lState2,lbl2,weight2 = getCellDataCortege r
                                let newLabel,newlState = chooseNewLabel rl lbl1 lbl2 lState1 lState2
                                let newWeight = weightCalcFun rw weight1 weight2
                                let currentElem = buildData ruleIndex newlState newLabel newWeight
                                recTable.[i,l].[int a - 1] <- new CellData(currentElem, uint32 k) |> Some
                        )
                )

        let elem i l = rules |> Array.iteri (fun ruleIndex rule -> for k in 0..(l-1) do processRule rule ruleIndex i k l)

        let fillTable () =
          [|1..s.Length-1|]
          |> Array.iter (fun l ->
                [|0..s.Length-1-l|]
                |> Array.Parallel.iter (fun i -> elem i l))

        rules
        |> Array.iteri 
            (fun ruleIndex rule ->
                for k in 0..(s.Length-1) do
                    let a,b,c,rl,rw = getRuleCortege rule               
                    if c = 0us && b = s.[k] then
                        let lState =
                            match rl with
                            | 0uy -> LblState.Undefined
                            | _   -> LblState.Defined
                        let currentElem = buildData ruleIndex lState rl rw
                        recTable.[k,0].[int a - 1] <- new CellData(currentElem,0u) |> Some)
        printfn "After 1st line fill:"
        printTbl()

        printfn "Fill table started %s" (string System.DateTime.Now)
        fillTable ()
        printfn "Fill table finished %s" (string System.DateTime.Now)
        
        recTable

    let recognize ((grules, start) as g) s weightCalcFun =
        let recTable = recognitionTable g s weightCalcFun
        
        printTbl ()

        let getString state lbl weight = 
            let stateString = 
                match state with
                |LblState.Defined -> "defined"
                |LblState.Undefined -> "undefined"
                |LblState.Conflict -> "conflict"
                |_ -> ""

            String.concat " " [stateString; ":"; "label ="; lblString lbl; "weight ="; string weight]
            
        let rec out i last =
            let cellDatas = recTable.[0, s.Length-1]
            if i <= last 
            then 
                if cellDatas.[i].IsSome 
                then
                    let state,lbl,weight = getCellDataCortege (cellDatas.[i].Value)
                    if i = last
                    then [getString state lbl weight]
                    else getString state lbl weight :: out (i+1) last
                else "" :: out (i+1) last
            else [""]

        let lastIndex = (recTable.[0,s.Length-1]).Length - 1
        
        out 0 lastIndex

    let print lblValue weight leftI rightL leftL =
        let out = String.concat " " ["label ="; lblString lblValue; "weight ="; string weight; 
                    "left ="; string leftI; "right ="; string (leftI+rightL+leftL+1)]
        printfn "%s" out

    let rec trackLabel i l (cell:CellData)  flag =
        let ruleInd,_,curL,curW = getData cell.rData
        let _,b,c,lbl,_ = getRuleCortege rules.[int ruleInd]
        let (leftI,leftL),(rightI,rightL) = getSubsiteCoordinates i l (int cell._k)
        if l = 0
        then if curL <> noLbl
             then print curL curW leftI rightL leftL
        else 
            let left =
                recTable.[leftI,leftL]
                |> Array.tryFind (fun (x:Option<CellData>) -> 
                                        match x with
                                        | Some x ->
                                            let ind,lSt,lbl,_ = getData x.rData
                                            let top,_,_,_,_ = getRuleCortege rules.[int ind]
                                            top = b
                                        | None -> false)
            let right = 
                recTable.[rightI,rightL]
                |> Array.tryFind (fun (x:Option<CellData>) -> 
                                        match x with
                                        | Some x -> 
                                            let ind,lSt,lbl,_ = getData x.rData
                                            let top,_,_,_,_ = getRuleCortege rules.[int ind]
                                            top = c
                                        | None -> false)

            
            match right with
            | Some (Some right) ->
                match left with 
                | Some (Some left) ->
                    let _,_,lLbl,_ = getData left.rData
                    let _,_,rLbl,_ = getData right.rData
                    if curL <> noLbl && lLbl = noLbl && rLbl = noLbl
                    then print curL curW leftI rightL leftL
                    else
                        trackLabel leftI leftL left  true
                        trackLabel rightI rightL right  true
                | _ -> ()
            | _ ->
                if flag && lbl <> noLbl
                then print curL curW leftI rightL leftL
            
    let labelTracking lastInd = 
        let i,l = 0,lastInd
        Array.iteri (fun k x ->
                    x |> Option.iter(fun x ->
                        let out = "derivation #" + string (k + 1)
                        printfn "%s" out
                        trackLabel i l x false)
        ) recTable.[i, l]
            
    
    member this.Recognize ((grules, start) as g) s weightCalcFun lblNames = 
        rules <- grules
        lblNameArr <- lblNames
        // Info about dialects of derivation
        // in format: "<lblState> <lblName> <weight>"
        // If dialect undefined or was conflict lblName = "0"
        let out = recognize g s weightCalcFun |> List.filter ((<>)"") |> String.concat "\n"
        match out with
        | "" -> "The string is not derivable in specified grammar"
        | _ -> 
            labelTracking (s.Length - 1)
            out
