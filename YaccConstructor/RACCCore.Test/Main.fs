module RACCCoreTests

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RACCGenerator
module Lexer = UserLexer
open NUnit.Framework

type Test<'a,'b,'c,'d,'f,'g,'h when 'a:comparison and 'f:comparison and 'g:comparison> =
    {
        tables     : Tables<'a,'b,'c,'d,'f,'g>
        actionsMap : System.Collections.Generic.IDictionary<string,'h>
        path       : string
        rightValue : seq<obj>
    }

let testPath = "..\\..\\..\\..\\Tests\\RACC\\"

let tests =
    [   (1,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_1.yrd.in"
                rightValue = seq ["-" |> box]  
            })
        (2,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_2.yrd.in"
                rightValue = seq ["-;-" |> box]  
            })
        (3,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_3.yrd.in"
                rightValue = seq ["+" |> box]  
            })
        (4,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_4.yrd.in"
                rightValue = seq ["+;+" |> box]  
            })
        (5,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_5.yrd.in"
                rightValue = seq ["-;+" |> box]  
            })
        (6,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_6.yrd.in"
                rightValue = seq ["+;-" |> box]  
            })
        (7,
            {
                tables     =
                    {
                        gotoSet = Tables.gotoSet
                        automataDict = Tables.autumataDict
                        items = Tables.items
                    }
                actionsMap = RACC.Actions.ruleToAction
                path       = "test_cls\\test_cls_7.yrd.in"
                rightValue = seq ["-;+;+;-;+;-;-" |> box]  
            })
        (8,
            {
                tables     =
                    {
                        gotoSet = Tables_Alt.gotoSet
                        automataDict = Tables_Alt.autumataDict
                        items = Tables_Alt.items
                    }
                actionsMap = RACC.Actions_Alt.ruleToAction
                path       = "test_alt\\test_alt_1.yrd.in"
                rightValue = seq ["Detected: +" |> box]  
            })
        (9,
            {
                tables     =
                    {
                        gotoSet = Tables_Alt.gotoSet
                        automataDict = Tables_Alt.autumataDict
                        items = Tables_Alt.items
                    }
                actionsMap = RACC.Actions_Alt.ruleToAction
                path       = "test_alt\\test_alt_2.yrd.in"
                rightValue = seq ["Detected: *" |> box]  
            })
        (10,
            {
                tables     =
                    {
                        gotoSet = Tables_Seq.gotoSet
                        automataDict = Tables_Seq.autumataDict
                        items = Tables_Seq.items
                    }
                actionsMap = RACC.Actions_Seq.ruleToAction
                path       = "test_seq\\test_seq_1.yrd.in"
                rightValue = seq [3.0 |> box]  
            })
    ]
    |> dict

let run_common path = 
    let content = System.IO.File.ReadAllText(path)    
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

let run path tables actions =
    let buf = run_common path 
    let l = UserLexer.Lexer(buf)        
    let trees = TableInterpreter.run l tables            
    Seq.map (fun tree -> ASTInterpretator.interp actions tree) trees    

[<TestFixture>]
type ``RACC core tests`` ()=    
    [<Test>] 
    member test.``Closure test 1`` () =
        let test = tests.[1]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 2`` () =
        let test = tests.[2]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 3`` () =
        let test = tests.[3]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 4`` () =
        let test = tests.[4]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 5`` () =
        let test = tests.[5]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 6`` () =
        let test = tests.[6]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 7`` () =
        let test = tests.[7]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 1`` () =
        let test = tests.[8]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 2`` () =
        let test = tests.[9]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Seq test 1`` () =
        let test = tests.[10]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)
