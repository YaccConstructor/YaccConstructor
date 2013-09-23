//  Copyright 2009, 2010, 2011, 2012 Jake Kirilenko
//                        2011, 2012 Dmitry Avdyukhin
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
    open Microsoft.FSharp.Text
    [<Struct>]
    type Position =
        val absoluteOffset : int
        val line : int
        val column : int
        new (absoluteOffset, line, column) = {absoluteOffset = absoluteOffset; line = line; column = column}
        new (fslexPos : Lexing.Position) =
            {absoluteOffset = fslexPos.AbsoluteOffset; line = fslexPos.Line; column = fslexPos.Column}
    /// Type of elementary part of source grammar
    [<Struct>]
    type t =
        val text : string
        val startPos : Position
        val endPos : Position
        val file : string

        new (text, startPos, endPos, file) =
            {text = text; startPos = startPos; endPos = endPos; file = file}
        new (text, origin : t) =
            {text = text; startPos = origin.startPos; endPos = origin.endPos; file = origin.file}
        new (text, startPos : Lexing.Position, endPos : Lexing.Position) =
            t (text, new Position(startPos), new Position(endPos), startPos.FileName)
        new (text, lexbuf : Lexing.LexBuffer<_>) =
            t (text, lexbuf.StartPos, lexbuf.EndPos)
        new (text) =
            t (text, new Position(), new Position(), "")
        override this.ToString() = this.text
    // TODO: make something with toString overriding of Source.t   
    let toString (x : t) = x.text
end
  
module Production = begin
    //let num = ref 0
    type IRuleType = interface end
    type DLabel = {
        label: string;
        weight: float option
    }
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
    /// <para>  'patt - type of l-attributes. </para>
    /// <para>  'expr - type of expressions in action code. </para>
    /// </summary>
    and t<'patt,'expr> = 
        /// Alternative (e1 | e2)
        |PAlt     of (t<'patt,'expr>) * (t<'patt,'expr>)
        /// Sequence * attribute. (Attribute is always applied to sequence) 
        |PSeq     of (elem<'patt,'expr>) list * 'expr option * DLabel option
        /// Token itself. Final element of parsing.
        |PToken   of Source.t 
        /// Reference to other rule inside production. With an optional args list.
        |PRef     of Source.t * 'expr option
        /// expr*
        |PMany    of (t<'patt,'expr>)
        /// Reference to metarule inside production (mr<<x>> in rule "a: mr<<x>> y z")
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

        with
        override this.ToString() =
//            incr num
//            printfn "%d %A" !num this
            let argsToString = function
                | None -> ""
                | Some x -> "[" + x.ToString() + "]"
                    
            let metaArgsToString metaArgs =
                if ((metaArgs : 'a list).IsEmpty) then ""
                else "<<" + (metaArgs
                             |> List.map (fun x -> x.ToString())
                             |> String.concat " ")
                        + ">>"
                    
            match this with
            |PAlt (x, y) -> x.ToString() + " | " + y.ToString()
            |PSeq (ruleSeq, attrs, l) ->
                let strAttrs =
                    match attrs with
                    | None -> ""
                    | Some x -> "{" + x.ToString() + "}"
                let elemToString (x:elem<_,_>) =
                    let check =
                        match x.checker with
                        | None -> ""
                        | Some c -> "=>{" + c.ToString() + "}=>"
                    let omit = if (x.omit) then "-" else ""
                    let bind =
                        match x.binding with
                        | None -> ""
                        | Some var -> var.ToString() + "="
                    check + omit + bind + x.rule.ToString()
                "<" + String.concat " " (List.map (fun x -> (*printfn "%A" x;*) "(" + (elemToString x) + ")") ruleSeq) + ">" + strAttrs
            |PToken src -> Source.toString src
            |PRef (name, args) ->
                Source.toString name + argsToString args
            |PMany x -> "(" + x.ToString() + ")*"
            |PMetaRef (name, args, metaArgs) ->
                Source.toString name + metaArgsToString metaArgs + argsToString args
            |PLiteral src -> Source.toString src
            |PRepet _ -> failwith "Repetition was not realized yet"
            |PPerm src ->
                src
                |> List.map (fun x -> x.ToString())
                |> String.concat " "
                |> fun res -> "[|" + res + "|]"
            |PSome x -> "(" + x.ToString() + ")+"
            |POpt x -> "(" + x.ToString() + ")?"
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
        name    : Source.t
        /// Heritable arguments of rule
        args    : 'patt list
        /// Rule body (production).
        body    : (Production.t<'patt,'expr>)
        /// Is this rule a start non-terminal (in this case '[<Start>]' is used before rule)
        isStart : bool
        /// Can this rule be seen from another module.
        /// It's true if ('public' is used before rule) or (module is marked as AllPublic and rule isn't marked as private)
        isPublic : bool
        /// List of meta-arguments - names of rules, parametrizing this rule.
        metaArgs: 'patt list
    }

    let defaultRule name body =
        {name = name; body = body; args = []; isStart = false; isPublic = false; metaArgs = []}
end


module Grammar =  begin
    type Module<'patt,'expr> = {
        /// Module is a list of rules
        rules : Rule.t<'patt,'expr> list
        openings : Source.t list
        name : Source.t option
        /// Are all rules public (can be seen form another module), except explicitly marked as private.
        /// Otherwise rule must be directly marked as public to be seen.
        allPublic : bool
    }
    /// Grammar is a list of modules
    type t<'patt,'expr> = Module<'patt,'expr> list
end 

module Definition = begin
    
    type info = { fileName: string }
    type t<'patt,'expr when 'patt : comparison and 'expr : comparison>  = { 
     /// Contains information (e.g. origin) about this grammar description
     info    : info;
     /// Text before a grammar description ( e.g. some open-s), what will be simply copied
     head    :'expr option;
     /// Grammar description itself
     grammar : Grammar.t<'patt,'expr>;
     /// Text after a grammar description, what will be simply copied
     foot    :'expr option;
     options : Map<string, string>
     tokens : Map<string, string option>
    }    
    
    /// Empty grammar
    let empty = { info = {fileName = ""}; head = None; foot = None; grammar = []; options = Map.empty; tokens = Map.empty}
end