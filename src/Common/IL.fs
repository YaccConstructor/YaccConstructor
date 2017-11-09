//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module Yard.Core.IL

open Microsoft.FSharp.Text

[<Struct>]
type SourcePosition =
    val absoluteOffset : int
    val line : int
    val column : int
    new (absoluteOffset, line, column) = {absoluteOffset = absoluteOffset; line = line; column = column}
    new (fslexPos : Lexing.Position) =
        {absoluteOffset = fslexPos.AbsoluteOffset; line = fslexPos.Line; column = fslexPos.Column}
/// Type of elementary part of source grammar
[<Struct>]
[<StructuralEquality; StructuralComparison>]
type Source =
    val text : string
    val startPos : SourcePosition
    val endPos : SourcePosition
    val file : string

    new (text, startPos, endPos, file) =
        {text = text; startPos = startPos; endPos = endPos; file = file}
    new (text, origin : Source) =
        {text = text; startPos = origin.startPos; endPos = origin.endPos; file = origin.file}
    new (text, startPos : Lexing.Position, endPos : Lexing.Position) =
        Source (text, new SourcePosition(startPos), new SourcePosition(endPos), startPos.FileName)
    new (text, lexbuf : Lexing.LexBuffer<_>) =
        Source (text, lexbuf.StartPos, lexbuf.EndPos)
    new (text) =
        Source (text, new SourcePosition(), new SourcePosition(), "")
    override this.ToString() = this.text
    //override this.GetHashCode t = hash this.text
// TODO: make something with toString overriding of Source
let sourceToString (x : Source) = x.text

type DLabel = {
    label: string;
    weight: float option
}

[<StructuralEquality; StructuralComparison>]
type ProductionElem<'patt,'expr> = {
    /// Don't include rule into AST
    omit:bool;
    /// Production rule itself.
    rule:(Production<'patt,'expr>);
    /// Binding :) like f:F or f:=F.... Seal
    binding:'patt option;
    /// Almost resolver (condition in production).
    checker:'expr option
}
with override x.ToString() =
        let check =
            match x.checker with
            | None -> ""
            | Some c -> sprintf "=>{%O}=>" c
        let omit = if x.omit then "-" else ""
        let bind =
            match x.binding with
            | None -> ""
            | Some var -> var.ToString() + "="
        check + omit + bind + x.rule.ToString()

/// <summary>
/// <para>t&lt;'patt,'expr&gt; - Type of production node in derivation tree. </para>
/// <para>  'patt - type of l-attributes. </para>
/// <para>  'expr - type of expressions in action code. </para>
/// </summary>
and Production<'patt,'expr> =
    /// Shuffle (e1 || e2)
    |PShuff     of (Production<'patt,'expr>) * (Production<'patt,'expr>)
    /// Alternative (e1 | e2)
    |PAlt     of (Production<'patt,'expr>) * (Production<'patt,'expr>)
    /// Conjunction (e1 & e2)
    |PConj    of (Production<'patt,'expr>) * (Production<'patt,'expr>)
    /// Negation
    |PNeg of (Production<'patt,'expr>)
    /// Sequence * attribute. (Attribute is always applied to sequence)
    |PSeq     of (ProductionElem<'patt,'expr>) list * 'expr option * DLabel option
    /// Token itself. Final element of parsing.
    |PToken   of Source
    /// Reference to other rule inside production. With an optional args list.
    |PRef     of Source * 'expr option
    /// expr*
    |PMany    of (Production<'patt,'expr>)
    /// Reference to metarule inside production (mr<<x>> in rule "a: mr<<x>> y z")
    |PMetaRef of Source * 'expr option * Production<'patt,'expr> list
    /// Literal. We can use constants ("if" and "then" in ' .."if" expr "then" expr...')
    |PLiteral of Source
    /// Extended regexp repetition, "man egrep" for details
    |PRepet   of (Production<'patt,'expr>) * int option * int option
    /// Permutation (A || B || C)
    |PPerm    of (Production<'patt,'expr>) list
    // The following are obsolete and reduction to PRepet should be discussed.
    /// expr+
    |PSome    of (Production<'patt,'expr>)
    /// expr?
    |POpt     of (Production<'patt,'expr>)

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
        |PConj (x, y) -> x.ToString() + " & " + y.ToString()
        |PNeg x -> "!" + x.ToString()
        |PSeq (ruleSeq, attrs, l) ->
            let strAttrs =
                match attrs with
                | None -> ""
                | Some x -> "{" + x.ToString() + "}"
            "<" + String.concat " " (List.map (sprintf "(%O)") ruleSeq) + ">" + strAttrs
        |PToken src -> sourceToString src
        |PRef (name, args) ->
            sourceToString name + argsToString args
        |PMany x -> "(" + x.ToString() + ")*"
        |PMetaRef (name, args, metaArgs) ->
            sourceToString name + metaArgsToString metaArgs + argsToString args
        |PLiteral src -> sourceToString src
        |PRepet _ -> failwith "Repetition was not realized yet"
        |PPerm src ->
            src
            |> List.map (fun x -> x.ToString())
            |> String.concat " "
            |> fun res -> "[|" + res + "|]"
        |PSome x -> "(" + x.ToString() + ")+"
        |POpt x -> "(" + x.ToString() + ")?"


/// <summary>
/// <para>t&lt;'patt,'expr&gt; - Type of rule. </para>
/// <para>  'patt - type of attributes (arguments). </para>
/// <para>  'expr - type of expressions in action code. </para>
/// <para>Rule have the following format: </para>
/// <para>  [+]name&lt;&lt; metaArgs &gt;&gt;[args] : body; </para>
/// </summary>
[<StructuralEquality; StructuralComparison>]
type Rule<'patt,'expr> = {
    /// Rule name. Used to start from this or to be referenced to from other rules.
    name    : Source
    /// Heritable arguments of rule
    args    : 'patt list
    /// Rule body (production).
    body    : (Production<'patt,'expr>)
    /// Is this rule a start non-terminal (in this case '[<Start>]' is used before rule)
    isStart : bool
    /// Can this rule be seen from another module.
    /// It's true if ('public' is used before rule) or (module is marked as AllPublic and rule isn't marked as private)
    isPublic : bool
    isInline : bool
    /// List of meta-arguments - names of rules, parametrizing this rule.
    metaArgs: 'patt list
}
with override this.ToString() = sprintf "%O -> %O" this.name this.body

let defaultRule name body =
    {name = name; body = body; args = []; isStart = false; isPublic = false; isInline = false; metaArgs = []}

/// Module is a list of rules
type Module<'patt,'expr> = {
    rules : Rule<'patt,'expr> list
    openings : Source list
    name : Source option
    /// Are all rules public (can be seen form another module), except explicitly marked as private.
    /// Otherwise rule must be directly marked as public to be seen.
    allPublic : bool
}
with override this.ToString() = String.concat "\n" <| List.map (fun x -> x.ToString()) this.rules

/// Grammar is a list of modules
type Grammar<'patt,'expr> = Module<'patt,'expr> list

type DefinitionInfo = { fileName: string }
type Definition<'patt,'expr when 'patt : comparison and 'expr : comparison>  = {
    /// Contains information (e.g. origin) about this grammar description
    info    : DefinitionInfo;
    /// Text before a grammar description ( e.g. some open-s), what will be simply copied
    head    :'expr option;
    /// Grammar description itself
    grammar : Grammar<'patt,'expr>;
    /// Text after a grammar description, what will be simply copied
    foot    :'expr option;
    options : Map<string, string>
    ///
    tokens : Map<string, string option>
}
with override this.ToString() = this.grammar.ToString()

/// Empty grammar
let emptyGrammarDefinition = { info = {fileName = ""}; head = None; foot = None; grammar = []; options = Map.empty; tokens = Map.empty}
