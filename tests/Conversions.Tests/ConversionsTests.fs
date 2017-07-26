//  ConversionsTests.fs contains unuit test for Conversions
//
//  Copyright 2009-2011 Konstantin Ulitin <ulitin.k@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module ConversionsTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open Yard.Core.Conversions
open Yard.Frontends.YardFrontend
open Yard.Frontends.FsYaccFrontend
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.TreeDump
open Yard.Generators.YardPrinter
open Yard.Generators.RIGLRGenerator

let dummyPos s = new Source.t(s)
let dummyToken s = PToken <| new Source.t(s)

exception FEError of string

let ConversionsManager = [|new AddDefaultAC.AddDefaultAC(), new AddEOF.AddEOF(), new BuildAST.BuildAST(), new BuildAstSimple.BuildAstSimple(), new ToCNF.ToCNF(),
                            new ToCNF.DeleteChainRule(), new ToCNF.DeleteEpsRule(), new ToCNF.SplitLongRule(), new ToCNF.RenameTerm(), new EliminateLeftRecursion.EliminateLeftRecursion(),
                            new ExpandTopLevelAlt.ExpandTopLevelAlt(), new ExpandBrackets.ExpandBrackets(), new ExpandEbnfStrict.ExpandEbnf(), new ExpandInnerAlt.ExpandInnerAlt(),
                            new ExpandMeta.ExpandMeta(), new LeaveLast.LeaveLast(), new MergeAlter.MergeAlter(), new RemoveAST.RemoveAC(), new ExpandInline.ReplaceInline()
                            , new ReplaceLiterals.ReplaceLiterals(), new Linearize.Linearize(), new ExpandRepet.ExpandExpand(), new ExpandConjunction.ExpandConjunction()|] 
                            |> Seq.ofArray |> Seq.cast<Conversion>

let FrontendsManager = [|new FsYaccFrontend(), new YardFrontend()|] |> Seq.ofArray |> Seq.cast<Frontend>

let conversionTestPath = @"./data/Conversions/"
let GeneratorsManager = [|new GLL(), new RNGLR(), new TreeDump(), new YardPrinter(), new RIGLR()|] |> Seq.ofArray |> Seq.cast<Generator>


let getFrontend name =       
    match Seq.tryFind (fun (elem : Frontend) -> elem.Name = name) FrontendsManager with
    | Some fe -> fe
    | None -> failwith (name + " is not found.")

let getBE name =
    match Seq.tryFind (fun (elem : Generator) -> elem.Name = name) GeneratorsManager with
    | Some be -> be
    | None -> failwith (name + " is not found.")

let treeDump = new Yard.Generators.TreeDump.TreeDump ()

let dummyRule : elem<Source.t,Source.t> = {omit=false; binding=None; checker=None; rule=PToken (Source.t "DUMMY")}

let expandBrackets = new Conversions.ExpandBrackets.ExpandBrackets()
let expandMeta = new Conversions.ExpandMeta.ExpandMeta()
let expandEbnf = new Conversions.ExpandEbnfStrict.ExpandEbnf()
let expandInnerAlt = new Conversions.ExpandInnerAlt.ExpandInnerAlt()
let expandRepeat = new Conversions.ExpandRepet.ExpandExpand()
let expandTopLevelAlt = new Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt()
let expandSubSeq = new Conversions.ExpandBrackets.ExpandBrackets()
let eliminateLeftRecursion = new Conversions.EliminateLeftRecursion.EliminateLeftRecursion()
let conversionLongRules = new Conversions.ToCNF.SplitLongRule()
let conversionEps = new Conversions.ToCNF.DeleteEpsRule()
let conversionChain = new Conversions.ToCNF.DeleteChainRule()
let conversionRenamer = new Conversions.ToCNF.RenameTerm()
//let conversionCNF = new Conversions.ToCNF.ToCNF()
let conversionChomNormForm = new Conversions.ToChomNormForm.ToChomNormForm()
let conversionBinNormForm = new Conversions.ToBinNormForm.ToBinNormForm()
let conversionCNF = new Conversions.CNFandBNF.CNF()
let conversionBNFconj = new Conversions.CNFandBNF.BNFconj()
let conversionBNFbool = new Conversions.CNFandBNF.BNFconj()


let applyConversion (conversion:Conversion) loadIL = 
    {
        loadIL
            with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
    }

let fe = new YardFrontend()
let runTest inputFile conversion expectedResult =
    let loadIL = fe.ParseGrammar inputFile
    Namer.initNamer loadIL.grammar
    let result = loadIL |> applyConversion conversion
    let expected = defaultDefinition expectedResult
#if DEBUG    
    expected |> treeDump.Generate |> string |> printfn "%s"
    printfn "%s" "************************"
    result |> treeDump.Generate |> string |> printfn "%s"
#endif
    Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

[<TestFixture>]
type ``Conversions tests`` () =
    
    [<Test>]
    member test.``ExpandBrackets. Sequence as sequence element test.``()=
        //let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager() 
        let frontend = new YardFrontend()
        let ilTree = 
            System.IO.Path.Combine(conversionTestPath,"expandbrackets_1.yrd")
            |> frontend.ParseGrammar

        Namer.initNamer ilTree.grammar
        let ilTreeConverted = 
            ilTree 
            |> applyConversion expandMeta
            |> applyConversion expandEbnf
            |> applyConversion expandInnerAlt
            |> applyConversion expandBrackets
        let hasNotInnerSeq = 
            ilTreeConverted.grammar
            |> List.forall (fun m ->
                m.rules |> List.forall
                    (fun rule ->
                        let rec eachProd = function
                            | PAlt(a,b) -> eachProd a && eachProd b
                            | PSeq(elements, _, _) ->
                                elements |> List.forall
                                    (fun elem -> match elem.rule with PSeq _ -> false | _ -> true)
                            | _ -> true
                        eachProd rule.body
                    )
                )
            
#if DEBUG
        let generator = new TreeDump()
        printfn "%A\n" (generator.Generate(ilTreeConverted,true))
#endif

        //treeDump.Generate expected |> string |> printfn "%s"
        //treeDump.Generate ilTreeConverted |> string |> printfn "%s"
        Assert.True(hasNotInnerSeq)
   
[<TestFixture>]
type ``Expand top level alters`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ExpandTopLevelAlters")
    let path f = System.IO.Path.Combine(basePath, f)

    [<Test>]
    member test.``No alter`` () =     
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "d", None)}]
        ) @ (
            verySimpleNotStartRules "d"
                [{dummyRule with rule = PToken (Source.t "NUM")}]
        )
        |> runTest (path "noAlters.yrd") expandTopLevelAlt        

    [<Test>]
    member test.``One alter`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "c", None)}]
        ) @ (
            verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "d", None)}]
        )
        |> runTest (path "oneAlter.yrd") expandTopLevelAlt        

    [<Test>]
    member test.``Multi alters`` () =        
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "x", None)}]
        ) @ (
            verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "y", None)}]
        ) @ (
            verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "z", None)}]
        ) @ (
            verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "m", None)}]
        )
        |> runTest (path "multiAlters.yrd") expandTopLevelAlt 