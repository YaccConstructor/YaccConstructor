open Yard.Unger

[<EntryPoint>]
let main _ = 
    let grammar : Grammar = new Grammar("../../../../Tests/Unger/A.yrd")
    let parser : Parser = new Parser(grammar)
    let parsingForest = parser.parse(["a";"+";"a";"*";"a"])
    0

