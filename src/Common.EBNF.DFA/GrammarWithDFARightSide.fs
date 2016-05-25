namespace Yard.EBNF.GrammarWithDFARightSide

open Yard.Core.IL
open Yard.Generators.Common

module DFAProduction =
  
    type t = {
    
        numberOfStates : int
        startStates : int[]
        stateToVertex : Vertex<int, int>[]
        finishStates : Set<int>
    }  

module DFARule =
    
    type t<'patt,'expr> = {
        /// Rule name. Used to start from this or to be referenced to from other rules.
        name    : Source.t
        /// Heritable arguments of rule
        args    : 'patt list
        /// Rule body (production).
        body    : DFAProduction.t
        /// Is this rule a start non-terminal (in this case '[<Start>]' is used before rule)
        isStart : bool
        /// Can this rule be seen from another module.
        /// It's true if ('public' is used before rule) or (module is marked as AllPublic and rule isn't marked as private)
        isPublic : bool
        /// List of meta-arguments - names of rules, parametrizing this rule.
        metaArgs: 'patt list
    }
