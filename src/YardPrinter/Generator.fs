//  Copyright 2010-2011 by Konstantin Ulitin, Dmitry Avdyukhin
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

module Yard.Generators.YardPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let endl = System.Environment.NewLine
let printSourceOpt = function None -> "" | Some (arg : Source.t) -> endl + "{" + arg.text + "}" + endl + endl

type TextBox =
| Tabbed of seq<TextBox> 
| Line   of seq<TextBox>
| StrSeq of seq<TextBox>
| Str    of string
with 
  member x.IsTab with get() = match x with Tabbed _ -> true | _ -> false
  
let str2line s = s |> Str |> Seq.singleton |> Line

let printTextBox tabSize windowSize (tbSeq : seq<_>) =

    /// Return new sequence with same elements as in tbSeq,
    /// with proper indent before each element.
    let rec printIndentedSeq tbSeq indent =
        Seq.collect (function
            | Str s -> Seq.singleton (s, indent)
            | StrSeq tbSeq -> printIndentedSeq tbSeq indent
            | Tabbed tbSeq | Line tbSeq as x -> 
             seq { yield endl, indent
                   yield! printIndentedSeq tbSeq (indent + (if x.IsTab then tabSize else 0) ) } 
        ) tbSeq

    let strSeq = printIndentedSeq tbSeq 0

    // Return a string, consisting of established number of spaces
    let write_spaces i = String.replicate i " "

    /// Convert obtained sequence to resulting string.
    /// Newline is true if there was a line feed.
    let (text,_,_) =
        strSeq
        |> Seq.fold (fun (str_acc, newline, chars_in_line) (word,indent) -> 
            if word=endl then 
                if newline then str_acc
                else str_acc + word
                |> fun text -> (text, true, 0)
            else
                let spaces_count =
                    if newline then indent
                    elif word.Length = 0 || word.[0] = ';' || word.[0] = ' ' then 0
                    else 1
                if String.length word <= windowSize - chars_in_line - spaces_count then
                    let appended = write_spaces spaces_count + word
                    (str_acc + appended, false, chars_in_line + String.length appended)
                else
                    let newlineStr = write_spaces (indent + tabSize) + word
                    str_acc + endl + newlineStr, false, String.length newlineStr
            ) ("", true, 0)
    text

let printSeqBrackets l_br r_br metaArgs =
    if Seq.isEmpty metaArgs then ""
    else
        String.concat " " metaArgs
        |> (fun s ->
                if s.Length > 0 && (s.Chars (s.Length - 1) = '>' || s.Chars 0 = '<')
                then " " + s + " "
                else s
        ) |> fun s -> l_br + s + r_br

let printProduction =
    let rec unboxText textBoxSeq =
        Seq.collect (function | Tabbed s | Line s | StrSeq s -> unboxText s
                              | Str s -> Seq.singleton s) textBoxSeq

    let rec printMetaArgs metaArgs = 
        List.map (printProduction true >> unboxText) metaArgs
        |> List.map2 (fun init s ->
                match init with | PSeq _ | PAlt _ -> seq {yield "("; yield! s; yield ")"}
                                | _ -> s
             ) metaArgs
        |> Seq.concat
        |> printSeqBrackets "<" ">"

    // wasAlt is used for dealing with one set of alternatives (if it's true, we are inside the set).
    and printProduction wasAlt (production:Production.t<Source.t,Source.t>)  = 
        let rec priority = function 
            | PAlt _ -> 1
            | PSeq ([elem],None,_) -> 
                match elem.binding with
                | Some _ -> 10
                | None -> priority elem.rule
            | PSeq _ -> 1
            | PToken _ | PRef _ | PMetaRef _ | PLiteral _ -> 100
            | PMany _ | POpt _ | PSome _ -> 50
            | _ -> -1

        let bracketsIf cond s =
            if cond then seq { yield Str "("; yield! s; yield Str ")"}
            else s
        let printAttr = function
            | Some attr -> "{" + Source.toString attr + "}"
            | None -> ""
        let printArg = function
            | Some attr  -> "<<" + Source.toString attr + ">>"
            | None -> ""
        let printElem (elem:elem<Source.t,Source.t>) = 
            let binding = function
                | Some x when String.forall (fun x -> System.Char.IsLetterOrDigit x || x = '_')  (Source.toString x) -> Source.toString x + " ="
                | Some x  -> "{" + Source.toString x + "} ="
                | None -> ""
            let omit = if elem.omit then "-" else ""
            let needBrackets =  let prio = priority elem.rule in if elem.binding.IsSome then prio < 50 else prio <= 1
            seq { yield Str <| omit + binding elem.binding
                  yield! bracketsIf needBrackets (printProduction false elem.rule) }
        let printEbnf sign expr =
            seq { yield! bracketsIf (priority expr < 50) (printProduction false expr);
                  yield Str sign}

        match production with
        // Alternatives
        | PAlt(alt1, alt2) ->
            if not wasAlt then
                seq {yield Str " "
                     yield! printProduction true production}
                |> Tabbed
                |> Seq.singleton
            else seq {yield StrSeq <| printProduction true alt1
                      yield Line (seq {yield Str "|"
                                       yield! printProduction true alt2}
                                 )
                     } 
        // Sequence * attribute.(attribute is always applied to sequence) 
        | PSeq(elem_seq, attr_option,lbl) -> 
            let isLbl = lbl.IsSome
            let isWght = isLbl && lbl.Value.weight.IsSome

            seq {if isLbl then yield Str <| lbl.Value.label + "("
                 if isWght then yield Str <| "[" + (lbl.Value.weight.Value |> int |> string) + "]"
                 yield! Seq.collect printElem elem_seq
                 if isLbl then yield Str ")"
                 yield Str <| printAttr attr_option
                }
        // Token
        | PToken source -> Seq.singleton <| Str (Source.toString source)
        // Vanilla rule reference with an optional args list.
        | PRef(source, attr_option) -> Seq.singleton <| Str (Source.toString source + printArg attr_option)
        // expr*
        | PMany many -> printEbnf "*" many
        // Metarule reference like in "a: mr<x> y z"
        | PMetaRef(rule_name, opt_arg, metaArgs) ->
            Source.toString rule_name + printMetaArgs metaArgs + printArg opt_arg
            |> Str |> Seq.singleton
        // Literal. Often one wants to write explicitly, e.g.: .."if" expr "then" expr...
        | PLiteral source ->
            Source.toString source
            |> fun s -> Str ("'" + s + "'") 
            |> Seq.singleton
    //        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
    //        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
    ///// The following are obsolete and reduction to PRepet should be discussed.
        // expr+
        | PSome some -> printEbnf "+" some
        // expr?
        | POpt opt -> seq {yield Str "["; yield! printProduction false opt; yield Str "]"}
        | _ -> Seq.singleton <| Str "ERROR"
    printProduction

let printRule isPublicModule (rule : Rule.t<Source.t, Source.t>) =
    let printArgs args =
        args
        |> List.map (fun src -> "<<" + Source.toString src + ">>")
        |> String.concat ""
    let startSign = if rule.isStart then "[<Start>]" + endl else ""
    let accessModifier =
        if isPublicModule = rule.isPublic then ""
        elif rule.isPublic then "public "
        else "private "
    seq {yield Line(seq{yield Str(startSign + accessModifier + rule.name.text
                                    + (rule.metaArgs |> List.map Source.toString |> printSeqBrackets "<" ">")
                                    + (printArgs rule.args) + ":");
                        yield Str " ";
                        yield! printProduction false rule.body;
                        yield Str <| ";" + endl
                       }
                   )
        }

let generate (input_grammar: Definition.t<Source.t,Source.t>) =
    let print : seq<_> -> _ = printTextBox 4 80 
    let tab = "    "
    let tokens =
        let map = input_grammar.tokens
        if map.IsEmpty then ""
        else
            [
                "tokens {"
                String.concat endl [for p in map -> tab + "| " + p.Key + match p.Value with | None -> "" | Some v -> " of " + v]
                "}"
                ""
            ]
            |> String.concat endl
            
    let options =
        let map = input_grammar.options
        if map.IsEmpty then ""
        else
            [
                "options {"
                String.concat endl [for p in map -> tab + p.Key + " = \"" + p.Value + "\""]
                "}"
                ""
            ]
            |> String.concat endl
            
    let tbSeq =
        input_grammar.grammar
        |> Seq.collect (fun m ->
            seq {
                match m.name with
                | Some name ->
                    if m.allPublic then yield Line <| Seq.singleton (Str "[<AllPublic>]")
                    yield str2line <| "module " +  name.text
                    if not m.openings.IsEmpty then
                        yield
                            m.openings |> List.map (fun op -> op.text)
                            |> String.concat ", "
                            |> (fun ops -> str2line <| "open " + ops)
                        
                | None -> ()
                yield! Seq.collect (fun rule -> printRule m.allPublic rule) m.rules
            }
        )
    String.concat ""
        [
            printSourceOpt input_grammar.head
            tokens
            options
            print tbSeq
            printSourceOpt input_grammar.foot
        ]
