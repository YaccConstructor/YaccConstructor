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
open Yard.Generators.RNGLR.InitialConvert
open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.YardPrinter
open Yard.Generators.RNGLR.States

type RNGLR() = 
    inherit Generator()
        override this.Name = "RNGLRGenerator"
        override this.Generate definition =
            let start = System.DateTime.Now
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar);
            let kernelIndexator = new KernelIndexator(grammar)
            items grammar kernelIndexator
            printfn "%A" <| System.DateTime.Now - start
            (new YardPrinter()).Generate newDefinition
        override this.AcceptableProductionTypes =
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>)
            |> List.map (fun unionCase -> unionCase.Name)