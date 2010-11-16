module TestConvertion

open FsCheck
open FsCheck.Prop
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Yard.Core.Main
open Yard.Core.IL
open Yard.Core.IL.Production
open NUnit.Framework
open Yard.Core.Namer
open Yard.Core.Convertions.ExpandEBNF
open Yard.Core.Convertions.ExpandAlter
open Yard.Core.Convertions.ExpandMeta

open Yard.Generators.RecursiveAscent.GrammarPreparer

module module2 =
    [<TestFixture>]
    type ``Convertion tests`` () =
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