module IronyFrontendTests

open IronyFrontendTests
open Yard.Frontends.IronyFrontend
open Yard.Core
open NUnit.Framework
open Yard.Core.IL.Definition
open Yard.Core.IL
open Yard.Core.IL.Production

let dummyPos s = new Source.t(s, new Source.Position(), new Source.Position(),"")



let run ironyGrammar =
    let frontend = new IronyFrontend() :> Frontend
    let ilTree = frontend.ParseGrammar ironyGrammar
    ilTree

let pos419 = new Source.Position (-419, 0, 0)
let strToToken s = PToken <| new Source.t(s, pos419, pos419, "")

let seq_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
 {info = {fileName = "";};
 head = None;
 grammar =
  [{name = dummyPos"s";
    args = [];
    body = PSeq ([{omit = false;
                   rule = strToToken "MULT";
                   binding = None;
                   checker = None;};
                  {omit = false;
                   rule = strToToken "PLUS";
                   binding = None;
                   checker = None;}]
                  ,None, None);
    _public = true;
    metaArgs = [];}];
 foot = None;
 options = Map.empty}



let i22_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
    {info = {fileName = "";};
     head = None;
     grammar =
      [{name = dummyPos"start";
        args = [];
        body =
         PAlt
           (PAlt
              (PSeq ([{omit = false;
                       rule = strToToken "GREATER";
                       binding = None;
                       checker = None;}],None, None),
               PSeq ([{omit = false;
                       rule = strToToken "LESS";
                       binding = None;
                       checker = None;}],None, None)),
            PSeq ([{omit = false;
                    rule = strToToken "EQUAL";
                    binding = None;
                    checker = None;}],None,None));
        _public = true;
        metaArgs = [];}];
     foot = None;
     options = Map.empty}

let nTermName_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
    {info = {fileName = "";};
     head = None;
     grammar = [{name = dummyPos"myNonTerm";
                 args = [];
                 body = PSeq ([{omit = false;
                                rule = strToToken "LESS";
                                binding = None;
                                checker = None;}],None, None);
                 _public = true;
                 metaArgs = [];}];
     foot = None;
     options = Map.empty}

[<TestFixture>]
type ``Irony frontend tests`` () =
    [<Test>]
    member test.``Seq test 1`` () =
        let res = run (new GSeq())
        #if DEBUG
        printfn "tree: %A" res
        #endif
        Assert.AreEqual(res,seq_res)
        
    [<Test>] 
    member test.``Issue22 test`` () =
        let res = run (new TermName())
        #if DEBUG
        printfn "tree: %A" res        
        #endif
        Assert.AreEqual(res,i22_res)

    [<Test>] 
    member test.``Issue22 test non term name`` () =
        let res = run (new NTermName())
        #if DEBUG
        printfn "tree: %A" res        
        #endif
        Assert.AreEqual(res,nTermName_res)