namespace Yard.EBNF.DFA.GrammarWithNFARightSide

open Yard.Core.IL
open Yard.Generators.Common

module NFAProduction =
  
    type t = {
    
        numberOfStates : int
        startState : Vertex<int,int>
        //массив ко всем состояним автомата
        stateToVertex : Vertex<int, int>[]
    }  

module NFARule =
    
    type t<'patt,'expr> = {
        /// Rule name. Used to start from this or to be referenced to from other rules.
        name    : Source.t
        /// Heritable arguments of rule
        args    : 'patt list
        /// Rule body (production).
        body    : NFAProduction.t
        /// Is this rule a start non-terminal (in this case '[<Start>]' is used before rule)
        isStart : bool
        /// Can this rule be seen from another module.
        /// It's true if ('public' is used before rule) or (module is marked as AllPublic and rule isn't marked as private)
        isPublic : bool
        /// List of meta-arguments - names of rules, parametrizing this rule.
        metaArgs: 'patt list
    }


