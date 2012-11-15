//  TestConversion.fs
//
//  Copyright 2010 Anastasia Nishnevich <Anastasia.Nishnevich@gmail.com>
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

module TestConversion

open FsCheck
open FsCheck.Prop
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Yard.Core.Main
open Yard.Core.IL
open Yard.Core.IL.Production
open NUnit.Framework
open Yard.Core.Namer
open Yard.Core.Conversions.ExpandEBNF
open Yard.Core.Conversions.ExpandAlter
open Yard.Core.Conversions.ExpandMeta

open Yard.Generators.RecursiveAscent.GrammarPreparer

module module2 =
    [<TestFixture>]
    type ``Conversion tests`` () =
          let  correctForAlExp (t: t<Source.t,Source.t>) =
              match t with
            //  |PAlt     (a,b) -> correctForAlExp a  && correctForAlExp b 
             // |PRef   _ 
              //|PLiteral _
            
              |PToken   _    -> true
             // |PAlt _-> true
              | _ -> false

        
              
          let ruleIsAfterEBNF (t: Rule.t<Source.t,Source.t>)   = 
              match t.body with
              | POpt _ | PSome _ | PMany _   -> false
              | _ -> true 
          
          let ruleIsAfterMeta (t: Rule.t<Source.t,Source.t>)   = 
              match t.body with
              | PMetaRef  _   -> false
              | _ -> true

          let ruleIsAfterAlter (t: Rule.t<Source.t,Source.t>)   = 
              match t.body with
              | PAlt _   -> false
              | _ -> true

          let isOk t = convertToMeta t |> List.forall (fun x -> ruleIsAfterEBNF x) 

// когда начальное условие не выполняется для большого числа вариантов все стоит 
          let ExpandAlterTest(t : Rule.t<Source.t,Source.t> ) = (correctForAlExp t.body) ==> lazy ( isOk t)
                                                               
          let ExpandEBNFTest(t : Rule.t<Source.t,Source.t> ) =  convertToMeta t |> List.forall (fun x -> ruleIsAfterEBNF x)


          let ExpandMetaRulesTest(l : Rule.t<Source.t,Source.t> list) =  (expandMetaRules l |> List.forall (fun x -> ruleIsAfterMeta x))


          [<Test>] member test.``ExpandEBNF Test`` () =
                     Check.Quick ExpandEBNFTest  

          [<Test>] member test.``ExpandAlter Test`` () =
                     Check.Quick ExpandAlterTest                         
                     
          [<Test>] member test.``ExpandMeta Test`` () =
                     Check.Quick ExpandMetaRulesTest 