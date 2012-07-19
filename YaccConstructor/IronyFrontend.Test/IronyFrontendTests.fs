module IronyFrontendTests

open IronyFrontendTests
open Yard.Frontends.IronyFrontend
open Yard.Core
open NUnit.Framework
open Yard.Core.IL.Definition
open Yard.Core.IL.Production

let run ironyGrammar =
    let frontend = new IronyFrontend() :> Frontend
    let ilTree = frontend.ParseGrammar ironyGrammar
    ilTree

let seq_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
 {info = {fileName = "";};
 head = None;
 grammar =
  [{name = "s";
    args = [];
    body = PSeq ([{omit = false;
                   rule = PToken ("MULT", (-419, -419,""));
                   binding = None;
                   checker = None;}; {omit = false;
                                      rule = PToken ("PLUS", (-419, -419,""));
                                      binding = None;
                                      checker = None;}],None);
    _public = true;
    metaArgs = [];}];
 foot = None;
 options = Map.empty}



let i22_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
    {info = {fileName = "";};
     head = None;
     grammar =
      [{name = "start";
        args = [];
        body =
         PAlt
           (PAlt
              (PSeq ([{omit = false;
                       rule = PToken ("GREATER", (-419, -419,""));
                       binding = None;
                       checker = None;}],None),
               PSeq ([{omit = false;
                       rule = PToken ("LESS", (-419, -419,""));
                       binding = None;
                       checker = None;}],None)),
            PSeq ([{omit = false;
                    rule = PToken ("EQUAL", (-419, -419,""));
                    binding = None;
                    checker = None;}],None));
        _public = true;
        metaArgs = [];}];
     foot = None;
     options = Map.empty}

let nTermName_res : IL.Definition.t<IL.Source.t,IL.Source.t> = 
    {info = {fileName = "";};
     head = None;
     grammar = [{name = "myNonTerm";
                 args = [];
                 body = PSeq ([{omit = false;
                                rule = PToken ("LESS", (-419, -419,""));
                                binding = None;
                                checker = None;}],None);
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