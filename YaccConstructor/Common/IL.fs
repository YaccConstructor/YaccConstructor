//  Copyright 2009-2011 Jake Kirilenko
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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


module Yard.Core.IL
module Source = begin
    /// Type of elementary part of source grammar
    type t = string * (int * int) 
    // TODO: make something with toString overriding of Source.t   
    let toString ((r,_):t):string = r
end
  
module Production = begin
    type IRuleType = interface end
    type elem<'patt,'expr> = {
        /// Don't include rule into AST
        omit:bool;
        /// Production rule itself.
        rule:(t<'patt,'expr>);
        /// Binding :) like f:F or f:=F.... Seal 
        binding:'patt option;
        /// Almost resolver (condition in production).
        checker:'expr option
    }
    /// <summary>
    /// <para>t&lt;'patt,'expr&gt; - Type of production node in derivation tree. </para>
    /// <para>  'patt - type of attributes (arguments). </para>
    /// <para>  'expr - type of expressions in action code. </para>
    /// </summary>
    and t<'patt,'expr> = 
        /// Alternative (e1 | e2)
        |PAlt     of (t<'patt,'expr>) * (t<'patt,'expr>)
        /// Sequence * attribute. (Attribute is always applied to sequence) 
        |PSeq     of (elem<'patt,'expr>) list * 'expr option
        /// Token itself. Final element of parsing.
        |PToken   of Source.t 
        /// Reference to other rule inside production. With an optional args list.
        |PRef     of Source.t * 'expr option
        /// expr*
        |PMany    of (t<'patt,'expr>)
        /// Reference to metarule inside production (mr<x> in rule "a: mr<x> y z")
        |PMetaRef of Source.t * 'expr option * t<'patt,'expr> list
        /// Literal. We can use constants ("if" and "then" in ' .."if" expr "then" expr...')
        |PLiteral of Source.t 
        /// Extended regexp repetition, "man egrep" for details
        |PRepet   of (t<'patt,'expr>) * int option * int option
        /// Permutation (A || B || C)
        |PPerm    of (t<'patt,'expr>) list
        // The following are obsolete and reduction to PRepet should be discussed.
        /// expr+
        |PSome    of (t<'patt,'expr>)
        /// expr?
        |POpt     of (t<'patt,'expr>) 
end

module Rule = begin
    /// <summary>
    /// <para>t&lt;'patt,'expr&gt; - Type of rule. </para>
    /// <para>  'patt - type of attributes (arguments). </para>
    /// <para>  'expr - type of expressions in action code. </para>
    /// <para>Rule have the following format: </para>
    /// <para>  [+]name&lt;&lt; metaArgs &gt;&gt;[args] : body; </para>
    /// </summary>
    type t<'patt,'expr> = {
        /// Rule name. Used to start from this or to be referenced to from other rules.
        name    : string;
        /// Heritable arguments of rule
        args    : 'patt list;
        /// Rule body (production).
        body    : (Production.t<'patt,'expr>);
        /// Is this rule a start non-terminal (in this case '+' is used before rule)
        _public : bool;
        /// List of meta-arguments - names of rules, parametrizing this rule.
        metaArgs: 'patt list
    }
end


module Grammar =  begin
    /// Grammar is a list of rules
    type t<'patt,'expr> = (Rule.t<'patt,'expr>) list
end 

module Definition = begin
    type info = { fileName: string }
    type t<'patt,'expr>  = { 
     /// Contains information (e.g. origin) about this grammar description
     info    : info;
     /// Text before a grammar description ( e.g. some open-s), what will be simply copied
     head    :'expr option; 
     /// Grammar description itself
     grammar : Grammar.t<'patt,'expr>;
     /// Text after a grammar description, what will be simply copied
     foot    :'expr option
    }    
    
    /// Empty grammar
    let empty = { info = {fileName = ""}; head = None; foot = None; grammar = []}
end