module CNFandBNFTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers


[<TestFixture>]
type ``CNFandBNF tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ToCNF")
    let path f = System.IO.Path.Combine(basePath, f)
      
    let applyConversion (conversion:Conversion) loadIL = 
        {
            loadIL
                with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
        }

    [<Test>]
    member test.``To CNF test`` () =
            let rules: Rule<_,_> list = 
                [{name = Source.t "yard_s_1";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "x",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t  "yard_s_2",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t  "yard_s_2";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "yard_s_2_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "x",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "yard_s_4",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t  "yard_s_1",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "x";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "yard_s_4",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t  "y",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "x";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "yard_s_2_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "y",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t  "yard_s_1";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t  "yard_s_2_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t  "x",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t  "y";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken( Source.t "CC");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t  "yard_s_2";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "x";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken(Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "x";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "y";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_s_4",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "y",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "y";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_s_2_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "y",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "y";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken(Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "y";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "yard_s_1";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "B");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_s_2_3";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "yard_s_4";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];}]

            runTest (path "grammar1.yrd") conversionCNF rules 

    [<Test>]
    member test.``To BNFconj test`` () =
            let rules: Rule<_,_> list = 
                [{name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "d",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "a";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_a_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "a",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name =  Source.t "yard_b_1";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "b",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "yard_b_1_4",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "b";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_b_5",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "yard_b_1",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "c";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_b_1_4",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "c",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "yard_d_2";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "d",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "yard_b_5",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "d";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PRef (Source.t "yard_a_3",None);
                                 binding = None;
                                 checker = None;}; {omit = false;
                                                    rule = PRef (Source.t "yard_d_2",None);
                                                    binding = None;
                                                    checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "d",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_d_2",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_1_4",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_d_2",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "a";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_b_1";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "C");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "c";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "C");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_d_2";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "d",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef ( Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name =Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_d_2",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_1_4",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_1_4",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "s";
                                    args = [];
                                    body = PSeq ([],None,None);
                                    isStart = true;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_a_3";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "A");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "yard_b_1_4";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "C");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_b_5";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}]

            runTest (path "grammar3.yrd") conversionBNFconj rules    

    [<Test>]
    member test.``To BNFbool test`` () =
            let rules: Rule<_,_> list = 
                [{name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "d",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "a";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "yard_b_1";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "b",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1_4",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "b";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "c";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_1_4",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "c",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "yard_d_2";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "d",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_5",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "d";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_d_2",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PNeg (PSeq ([],None,None)));
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "d",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_a_3",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "yard_d_2",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_b_1_4",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_a_3",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "yard_d_2",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_a_3",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "a",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_b_1_4",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "a",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "b",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_b_1_4",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "yard_a_3",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "yard_d_2",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "a";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_b_1";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "C");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "c";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "C");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_d_2";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];};
                 {name = Source.t "s";
                  args = [];
                  body =
                   PConj
                     (PSeq ([{omit = false;
                              rule = PRef (Source.t "yard_b_5",None);
                              binding = None;
                              checker = None;}; {omit = false;
                                                 rule = PRef (Source.t "yard_b_1",None);
                                                 binding = None;
                                                 checker = None;}],None,None),
                      PConj
                        (PNeg (PSeq ([{omit = false;
                                       rule = PRef (Source.t "d",None);
                                       binding = None;
                                       checker = None;}; {omit = false;
                                                          rule = PRef (Source.t "c",None);
                                                          binding = None;
                                                          checker = None;}],None,None)),
                         PNeg (PSeq ([],None,None))));
                  isStart = true;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "s";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "A");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = true;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_a_3";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "A");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}; {name = Source.t "yard_b_1_4";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (Source.t "C");
                                                   binding = None;
                                                   checker = None;}],None,None);
                                    isStart = false;
                                    isPublic = false;
                                    isInline = false;
                                    metaArgs = [];};
                 {name = Source.t "yard_b_5";
                  args = [];
                  body = PSeq ([{omit = false;
                                 rule = PToken (Source.t "B");
                                 binding = None;
                                 checker = None;}],None,None);
                  isStart = false;
                  isPublic = false;
                  isInline = false;
                  metaArgs = [];}]

            runTest (path "grammar4.yrd") conversionBNFbool rules  