module Earley.Tests

open Yard.Earley
open System.IO
open NUnit.Framework

[<TestFixture>]
type ``EarleyTests`` () =
    let g = new Grammar("../../../../Tests/Earley/test.yrd")
    let r = Recognizer(g)
    
    let g_sql = new Grammar("../../../YC.GrammarZOO/SQL/TSQL/mssql.yrd")
    let r_sql = Recognizer(g_sql)

    [<Test>]
    member test.``Simple accept`` () =
        Assert.IsTrue(r.recognize("AABD"))

    [<Test>]
    member test.``Simple decline`` () =
        Assert.IsFalse(r.recognize("AAB"))

    [<Test>]
    member test.``SQL accept`` () =
        Assert.IsTrue(r_sql.recognize("selectIdentIdentFromIdentOrderByIdentDesc"))

    [<Test>]
    member test.``SQL decline`` () =
        Assert.IsFalse(r_sql.recognize("selectIdentIdentFromIdentOrderByIdentDecs"))

    [<Test>]
    member test.``SQL decline syntax`` () =
        Assert.IsFalse(r_sql.recognize("select(IdentIdentFromIdent"))
