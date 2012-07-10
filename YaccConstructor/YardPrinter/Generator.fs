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

let printSourceOpt = function None -> "" | Some arg -> "\n{"+(fst arg)+"}\n\n"

type TextBox =
| Tabbed of seq<TextBox> 
| Line   of seq<TextBox>
| StrSeq of seq<TextBox>
| Str    of string
with 
  member x.IsTab with get() = match x with Tabbed _ -> true | _ -> false
  
let printTextBox tabSize windowSize tbSeq =

    /// Return new sequence, what has the same elements, what tbSeq has,
    ///    with proper indent before each element.
    let rec printIndentedSeq tbSeq indent =
        Seq.collect (function
            | Str s -> Seq.singleton (s, indent)
            | StrSeq tbSeq -> seq {yield! printIndentedSeq tbSeq (indent)}
            | Tabbed tbSeq | Line tbSeq as x -> 
             seq { yield ("\n", indent);
                   yield! printIndentedSeq tbSeq (indent + (if x.IsTab then tabSize else 0) ) } 
        ) tbSeq

    let strSeq = printIndentedSeq tbSeq 0

    // Return a string, consisting of established number of spaces
    let write_spaces i = String.replicate i " "

    /// Convert obtained sequence to resulting string.
    /// newline is true iff there was a line feed.
    let (text,_,_) = 
        Seq.fold (fun (str_acc, newline, chars_in_line) (word,indent) -> 
            if word="\n" then 
                if newline then
                    (str_acc, true, 0)
                else
                    (str_acc+word, true, 0)
            else
                let spaces_count = if newline then indent else if word.Length=0 || word.[0]=';' || word.[0]=' ' then 0 else 1
                if (String.length word <= windowSize - chars_in_line - spaces_count) then
                    let appended = (write_spaces spaces_count) + word
                    (str_acc + appended, false, chars_in_line + (String.length appended))
                else
                    let newlineStr = (write_spaces (indent + tabSize)) + word
                    (str_acc + "\n" + newlineStr, false, String.length newlineStr)
            ) ("", true, 0) strSeq
    text

let printRule (rule:Rule.t<Source.t, Source.t>) = 
    let bracketsIf cond s = if cond then seq { yield Str "("; yield! s; yield Str ")"} else s
    let startSign = if rule._public then "+" else ""
    let printAttr = function
        | Some(attr) -> "{"+(Source.toString attr)+"}"
        | None -> ""
    let printArg = function
        | Some(attr) -> "["+(Source.toString attr)+"]"
        | None -> ""
    let seqCount seq =
        Seq.fold(fun acc a -> acc+1) 0 seq
    let printSeqBrackets l_br r_br metaArgs =
        if (Seq.isEmpty metaArgs) then ""
        else if (seqCount metaArgs) > 1 then l_br + (String.concat " " metaArgs) + r_br
        else l_br + (Seq.head metaArgs) + r_br
    let printArgs args = List.map Source.toString args |> printSeqBrackets "[" "]"
    let rec priority = function 
        | PAlt(_) -> 1
        | PSeq([elem],None,_) -> 
            match elem.binding with
            | Some(_) -> 10
            | None -> priority elem.rule
        | PSeq(_) -> 1
        | PToken(_) | PRef(_) | PMetaRef(_) | PLiteral(_) -> 100
        | PMany(_) | POpt(_) | PSome(_) -> 50
        | _ -> -1

    let rec unboxText textBoxSeq = Seq.collect (function Tabbed(s) | Line(s) | StrSeq(s) -> unboxText s | Str(s) -> Seq.singleton s) textBoxSeq
    let rec printMetaArgs metaArgs = 
        List.map (printProduction true >> unboxText) metaArgs |> Seq.concat |> printSeqBrackets "<<" ">>"
    // wasAlt is used for dealing with one set of alternatives (if it's true, we are inside the set).
    and printProduction wasAlt (production:Production.t<Source.t,Source.t>)  = 
        let printElem (elem:elem<Source.t,Source.t>) = 
            let binding = function
                | Some x when String.forall System.Char.IsLetter (Source.toString x) -> (Source.toString x) + " ="
                | Some x  -> "<" + Source.toString x + "> ="
                | None -> ""
            let omit = if elem.omit then "-" else ""
            let needBrackets =  let prio = priority elem.rule in if elem.binding.IsSome then prio < 50 else prio <= 1
            seq { yield Str(omit + binding elem.binding); yield! bracketsIf needBrackets (printProduction false elem.rule) }
        match production with
        // Alternatives
        | PAlt(alt1, alt2) ->
            if not wasAlt then seq {yield Tabbed(seq {yield Str (" "); yield! printProduction true production})}
            else seq {yield StrSeq(printProduction true alt1); yield Line (seq {yield Str ("|"); yield! printProduction true alt2})} 
        // Sequence * attribute.(attribute is always applied to sequence) 
        | PSeq(elem_seq, attr_option,_) -> seq {yield! (Seq.collect printElem elem_seq); yield Str(printAttr attr_option)}
        // Token
        | PToken(source) -> Seq.singleton <| Str (Source.toString source)
        // Vanilla rule reference with an optional args list.
        | PRef(source, attr_option) -> Seq.singleton <| Str (Source.toString source + printArg attr_option)
        // expr*
        | PMany(many) -> seq { yield! bracketsIf (priority many < 50) (printProduction false many) ; yield Str "*"}
        // Metarule reference like in "a: mr<x> y z"
        | PMetaRef(rule_name, opt_arg, metaArgs) -> Seq.singleton <| Str((Source.toString rule_name)+(printMetaArgs metaArgs)+(printArg opt_arg))
        // Literal. Often one wants to write explicitly, e.g.: .."if" expr "then" expr...
        | PLiteral(source) -> Seq.singleton <| Str ("\"" + Source.toString source + "\"") 
//        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
//        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
///// The following are obsolete and reduction to PRepet should be discussed.
        // expr+
        | PSome(some) -> seq {yield! (bracketsIf (priority some<50) (printProduction false some)); yield Str("+")}
        // expr?
        | POpt(opt) -> seq {yield! (bracketsIf (priority opt<50) (printProduction false opt)); yield Str("?")}
        | _ -> Seq.singleton <| Str("ERROR")

    seq {yield Line(seq{yield Str(startSign + rule.name + (rule.metaArgs |> List.map Source.toString |> printSeqBrackets "<<" ">>"  ) + (printArgs rule.args) + ":");
         yield Str(" "); yield! printProduction false rule.body; yield Str(";\n")})}

let generate (input_grammar:Definition.t<Source.t,Source.t>) =
    let tbSeq = Seq.collect (fun rule -> printRule rule) input_grammar.grammar
    printSourceOpt(input_grammar.head)+(printTextBox 4 80 tbSeq)+printSourceOpt(input_grammar.foot)
