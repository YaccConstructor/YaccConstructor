namespace Yard.Frontends.IronyFrontend

open Yard.Core
open Irony.Parsing

module Converter =

    let Convert (ironyGrammar : Irony.Parsing.Grammar) = 
        let findBnfTerms bnfTerms (rule:BnfTerm) = 
            match rule with
            | :? NonTerminal as nt -> 1
            | :? BnfExpression as expr -> 2
            | :? KeyTerm as kt -> 3
            | :? Terminal as t -> 4 // Derived types also match
            | _ -> 5
   //     let d = new Irony.Parsing.BnfExpression()
  //      d.Data
        let root = ironyGrammar.Root
        //root.Rule.
        IL.Definition.empty

