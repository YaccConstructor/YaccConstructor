module Yard.Generators.YardPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let printSourceOpt = function None -> "" | Some arg -> (fst arg)

type TextBox =
| Tabbed of seq<TextBox> 
| Line of seq<TextBox>
| Str of string
with 
  member x.IsTab with get() = match x with Tabbed _ -> true | _ -> false


let to_seq x = seq { yield x }
let printTextBox tabSize splitTabSize windowSize tbSeq =
    let rec printIdentedSeq tbSeq ident =
        Seq.collect (function
            | Str s -> to_seq (s, ident)
            | Tabbed tbSeq | Line tbSeq   as x -> 
             seq { yield! printIdentedSeq tbSeq (ident + (if x.IsTab then tabSize else 0) ) 
                   yield ("\n", ident) } //[("\n", ident)]@
        ) tbSeq

    let strSeq = printIdentedSeq tbSeq 0

    let (text,_,_) = 
        Seq.fold (fun (str_acc, newline, chars_in_line) (word,ident) -> 
            if word="\n" then 
                if newline then
                    (str_acc, true, 0)
                else
                    (str_acc+word, true, 0)
            else
                let spaces_count = if newline then ident else 2
                if (String.length word <= windowSize - chars_in_line - spaces_count) then
                    let appended = (String.replicate spaces_count " ")+word
                    (str_acc + appended, false, chars_in_line + (String.length appended))
                else
                    let newlineStr = (String.replicate (ident + splitTabSize) " ") + word
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
    let printListBrackets  l_br r_br metaArgs =
        if (List.isEmpty metaArgs) then ""
        else l_br + (String.concat " " (List.map Source.toString metaArgs)) + r_br
    let printMetaArgs = printListBrackets "<<" ">>"
    let printArgs = printListBrackets "[" "]"
    let rec priority = function 
        | PAlt(_) -> 1
        | PSeq([elem],None) -> 
            match elem.binding with
            | Some(_) -> 10
            | None -> priority elem.rule
        | PSeq(_) -> 10
        | PToken(_) | PRef(_) | PMetaRef(_) | PLiteral(_) -> 100
        | PMany(_) | POpt(_) | PSome(_) -> 50
        | _ -> -1
    let rec printProduction (production:Production.t<Source.t,Source.t>) = 
        let printElem (elem:elem<Source.t,Source.t>) = 
            let binding = function
                | Some x when String.forall System.Char.IsLetter (Source.toString x) -> (Source.toString x) + "="
                | Some x  -> "<" + Source.toString x + ">="
                | None -> ""
            let omit = if elem.omit then "-" else ""
            let needBrackets =  let prio = priority elem.rule in if elem.binding.IsSome then prio < 50 else prio = 1
            seq { yield Str(omit + binding elem.binding); yield! bracketsIf  needBrackets (printProduction elem.rule) }
        match production with
        | PAlt(alt1, alt2) -> seq {yield Tabbed(printProduction alt1); yield Line (seq {yield Str ("| "); yield! printProduction alt2})} //Альтернатива
        | PSeq(elem_seq,attr_option) -> seq {yield! (Seq.collect printElem elem_seq); yield Str(printAttr attr_option)} //Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
        | PToken(source) -> to_seq <| Str (Source.toString source) //собственно токен
        | PRef(source, attr_option) -> to_seq <| Str (Source.toString source + printArg attr_option)  //Vanilla rule reference with an optional args list.
        | PMany(many) -> seq { yield! bracketsIf (priority many < 50) (printProduction many) ; yield Str "*"} //expr*
        | PMetaRef(rule_name, opt_arg, metaArgs) -> to_seq <| Str((Source.toString rule_name)+(printMetaArgs metaArgs)+(printArg opt_arg)) // Metarule reference like in "a: mr<x> y z"
        | PLiteral(source) -> to_seq <| Str (Source.toString source) //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
//        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
//        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
///// The following are obsolete and reduction to PRepet should be discussed.
        | PSome(some) -> seq {yield! (bracketsIf (priority some<50) (printProduction some)); yield Str("+")} //expr+
        | POpt(opt) -> seq {yield! (bracketsIf (priority opt<50) (printProduction opt)); yield Str("?")} //expr?
        | _ -> to_seq <| Str("ERROR")

    seq {yield Line(to_seq <| Str(startSign+rule.name+(printMetaArgs rule.metaArgs)+(printArgs rule.args)+": "));
        yield Tabbed(seq{yield! printProduction rule.body; yield Str(";")})}

let generate (input_grammar:Definition.t<Source.t,Source.t>) =
    let tbSeq = Seq.collect (fun rule -> printRule rule) input_grammar.grammar
    printSourceOpt(input_grammar.head)+(printTextBox 4 4 80 tbSeq)+printSourceOpt(input_grammar.foot)
