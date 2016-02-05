open Yard.Unger

[<EntryPoint>]
let main _ = 
    //example for calc
    let grammar : Grammar = new Grammar("../../../../Tests/Unger/A.yrd")
    let parser : Parser = new Parser(grammar)
    let mutable parsingForest = parser.parse([|"NUM";"+";"NUM";"+";"NUM"|])
    printfn "%A" parsingForest
    //example for part of C grammar (IF THEN ELSE)
    let grammar2 : Grammar = new Grammar("../../../../Tests/Unger/B.yrd")
    let parser2 : Parser = new Parser(grammar2)
    let mutable parsingForest2 = parser2.parse([|"{";"WHILE";"(";"EXPR";")";"{";"RETURN";"EXPR";";";"}";"}";|])
    printfn "%A" parsingForest2
    (*printfn "%A\n\n" parsingForest
    if parsingForest.Length = 0 then printfn "" else
        for i in 0..parsingForest.Length-1 do
            for j in 0..parsingForest.[i].Length-1 do
                match parsingForest.[i].[j] with
                |(a,b) -> printfn "%A -> %A" a b

            if not (i = parsingForest.Length-1) then printfn " next: \n"*)
    0


