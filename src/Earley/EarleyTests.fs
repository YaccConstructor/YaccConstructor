module EarleyTests

open Yard.Earley
open System.IO

[<EntryPoint>]
let main _ =
    let g = new Grammar("../../../../Tests/Earley/test.yrd")
    let r = Recognizer(g)
    assert (r.recognize("AAB") = false)
    assert (r.recognize("AABD") = true)

    let g_sql = new Grammar("../../../YC.GrammarZOO/SQL/TSQL/mssql.yrd")
    let r_sql = Recognizer(g_sql)
    assert(r_sql.recognize("selectIdentIdentFromIdentOrderByIdentDecs") = false)
    assert(r_sql.recognize("selectIdentIdentFromIdentOrderByIdentDesc") = true)
    
    0