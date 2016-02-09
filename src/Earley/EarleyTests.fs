module EarleyTests

open Yard.Earley
open System.IO

[<EntryPoint>]
let main _ =
    let g = new Grammar("../../../../Tests/Earley/test.yrd")
    let r = Recognizer(g)
    assert (r.recognize("AAB") = false)
    assert (r.recognize("AABD") = true)
    0