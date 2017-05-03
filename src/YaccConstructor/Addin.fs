module Addin

open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text
open System.IO
open System.Reflection
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.TreeDump
open Yard.Generators.YardPrinter
open Yard.Generators.RIGLRGenerator
open Yard.Frontends.FsYaccFrontend
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions

let private createFrontendsInitialization() = 
    lazy( [|new FsYaccFrontend(), new YardFrontend()|] |> Seq.cast<Frontend> |> Seq.toArray)
let private createConversionsInitialization() = 
    lazy(
        [|new AddDefaultAC.AddDefaultAC(), new AddEOF.AddEOF(), new BuildAST.BuildAST(), new BuildAstSimple.BuildAstSimple(), new ToCNF.ToCNF(),
          new ToCNF.DeleteChainRule(), new ToCNF.DeleteEpsRule(), new ToCNF.SplitLongRule(), new ToCNF.RenameTerm(), new EliminateLeftRecursion.EliminateLeftRecursion(),
          new ExpandTopLevelAlt.ExpandTopLevelAlt(), new ExpandBrackets.ExpandBrackets(), new ExpandEbnfStrict.ExpandEbnf(), new ExpandInnerAlt.ExpandInnerAlt(),
          new ExpandMeta.ExpandMeta(), new LeaveLast.LeaveLast(), new MergeAlter.MergeAlter(), new RemoveAST.RemoveAC(), new ExpandInline.ReplaceInline(),
          new ReplaceLiterals.ReplaceLiterals(), new Linearize.Linearize(), new ExpandRepet.ExpandExpand(), new ExpandConjunction.ExpandConjunction()|] 
          |> Seq.cast<Conversion> |> Seq.toArray
    )
let private createGeneratorsInitialization() = 
    lazy( [|new GLL(), new RNGLR(), new TreeDump(), new YardPrinter(), new RIGLR()|] |> Seq.cast<Generator> |> Seq.toArray )

let mutable private currentFrontends = createFrontendsInitialization()
let mutable private currentConversions = createConversionsInitialization()
let mutable private currentGenerators = createGeneratorsInitialization()

let private createFrontendNamesInitialization() = 
    lazy( Seq.map (fun (elem : Frontend) -> elem.Name) currentFrontends.Value |> Seq.toArray )
let private createConversionNamesInitialization() = 
    lazy( Seq.map (fun (elem : Conversion) -> elem.Name) currentConversions.Value |> Seq.toArray )
let private createGeneratorNamesInitialization() = 
    lazy( Seq.map (fun (elem : Generator) -> elem.Name) currentGenerators.Value |> Seq.toArray )

let mutable private currentFrontendNames = createFrontendNamesInitialization()
let mutable private currentConversionNames = createConversionNamesInitialization()
let mutable private currentGeneratorNames = createGeneratorNamesInitialization()

let getX (x : 'a []) = 
    if x.Length = 0
    then 
        failwith "Something wrong with Addin Manager."
    else x

let GetFrontends() = getX currentFrontends.Value
let GetConversions() = getX currentConversions.Value
let GetGenerators() = getX currentGenerators.Value
let GetFrontendNames() = getX currentFrontendNames.Value
let GetConversionNames() = getX currentConversionNames.Value
let GetGeneratorNames() = getX currentGeneratorNames.Value

