//  RNGLRGenerator.fs contains implementation of interface Generator
//
//  Copyright 2011-2012 Avdyukhin Dmitry
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

namespace Yard.Generators.RNGLR

open Yard.Core
open Yard.Core.IL
open Yard.Generators.RNGLR.InitialConvert
open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.YardPrinter
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR.Printer
open Yard.Generators.RNGLR.TranslatorPrinter

type RNGLR() = 
    inherit Generator()
        override this.Name = "RNGLRGenerator"
        override this.Generate (definition, tokenType) =
            let start = System.DateTime.Now
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar);
            if grammar.EpsilonCyclicNonTerms.Length > 0 then
                eprintfn "Grammar contains non-terminals, which can infinitely infer epsilon:"
                grammar.EpsilonCyclicNonTerms
                |> List.iter (eprintf "%s ")
                box ()
            else
                let statesInterpreter = buildStates grammar
                let tables = new Tables(grammar, statesInterpreter)
                let tokenTypeOpt =
                    match tokenType with
                    | "" -> None
                    | s -> Some s
                use out = new System.IO.StreamWriter (definition.info.fileName + ".fs")
                printTables grammar definition.head tables out tokenTypeOpt
                printTranslator grammar newDefinition.grammar out
                match definition.foot with
                | None -> ()
                | Some (s : Source.t) ->
                    out.WriteLine (Source.toString s)
                out.Close()
                printfn "%A" <| System.DateTime.Now - start
                (new YardPrinter()).Generate newDefinition
        override this.Generate definition = this.Generate (definition, "")
        override this.AcceptableProductionTypes =
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>)
            |> List.map (fun unionCase -> unionCase.Name)