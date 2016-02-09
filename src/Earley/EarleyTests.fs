module EarleyTests

open Yard.Earley
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let g = new Grammar("../../../../Tests/Earley/test.yrd")
    let r = Recognizer(g)
    assert (r.recognize("AAB") = false)
    assert (r.recognize("AABD") = true)

    let g_json = new Grammar("D:\Y\YaccConstructor\src\JSON_Parser\JSON.yrd")
    let r_json = Recognizer(g_json)
    
    assert(r_json.recognize("false") = true)
    assert(r_json.recognize("\"str\"") = true)
    assert(r_json.recognize("-1") = true)

    assert(r_json.recognize("[false]") = true)
    assert(r_json.recognize("[111,2]") = true)
    assert(r_json.recognize("[1,2,\"some\"]") = true)
    
    assert(r_json.recognize("{null}") = false)
    assert(r_json.recognize("{1:2}") = false)
    assert(r_json.recognize("{\"str\":2}") = true)
    assert(r_json.recognize("{\"str\":2,\"another\":null,\"more\":true,\"again\":\"yes\"}") = true)
    assert(r_json.recognize(@"{""str"":{""s"":12}}") = true)

    let text = File.ReadAllText("../../../../Tests/Earley/test.json")
    let complexTest = Regex.Replace(text, @"\s+", "")
    assert(r_json.recognize(complexTest) = true)
    0