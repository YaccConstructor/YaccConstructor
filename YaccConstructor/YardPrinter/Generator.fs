module Yard.Generators.YardPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let printSourceOpt = function None -> "" | Some arg -> (fst arg)

type TextBox =
| Tabbed of TextBox list
| Line of TextBox list
| Str of string

let printTextBox tabSize splitTabSize windowSize tbList =
    let rec printIdentedList tbList ident =
        List.collect (fun t ->
            match t with
            | Str(s) -> [(s, ident)]
            | Tabbed(tbList) -> (printIdentedList tbList (ident+tabSize))@[("\n", ident)] //[("\n", ident)]@
            | Line(tbList)   -> (printIdentedList tbList (ident        ))@[("\n", ident)] //[("\n", ident)]@
        ) tbList

    let strList = printIdentedList tbList 0

    let (text,_,_) = 
        List.fold (fun (str_acc, newline, chars_in_line) (word,ident) -> 
            if word="\n" then 
                if newline then
                    (str_acc, true, 0)
                else
                    (str_acc+word, true, 0)
            else
                let spaces_count = if newline then ident else 2
                if (String.length word<=windowSize-chars_in_line-spaces_count) then
                    let appended = (String.replicate spaces_count " ")+word
                    (str_acc+appended,false, chars_in_line+(String.length appended))
                else
                    let newlineStr = (String.replicate (ident+splitTabSize) " ")+word
                    (str_acc+"\n"+newlineStr,false, String.length newlineStr)
            ) ("", true, 0) strList
    text

let printRule (rule:Rule.t<Source.t,Source.t>) = 
    let bracketsIf cond s = if cond then [Str("(")]@s@[Str(")")] else s
    let startSign = if rule._public then "+" else ""
    let printAttr = function
        | Some(attr) -> "{"+(Source.toString attr)+"}"
        | None -> ""
    let printArg = function
        | Some(attr) -> "["+(Source.toString attr)+"]"
        | None -> ""
    let printListBrackets  l_br r_br metaArgs = if (List.isEmpty metaArgs) then "" else l_br+(String.concat " " (List.map Source.toString metaArgs))+r_br
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
                | Some(x) when String.forall System.Char.IsLetter (Source.toString x) -> (Source.toString x) + "="
                | Some(x) -> "<"+(Source.toString x)+">="
                | None -> ""
            [Str((if elem.omit then "-" else "") + (binding elem.binding))]@(bracketsIf ((match elem.binding with Some(_) -> true | None -> priority elem.rule=1) && priority elem.rule<50) (printProduction elem.rule))
        match production with
        | PAlt(alt1, alt2) -> [Tabbed(printProduction alt1); Line([Str("| ")]@(printProduction alt2))] //Альтернатива
        | PSeq(elem_list,attr_option) -> (List.collect printElem elem_list)@[Str(printAttr(attr_option))] //Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
        | PToken(source) -> [Str(Source.toString source)] //собственно токен
        | PRef(source, attr_option) -> [Str((Source.toString source) + printArg(attr_option))] //Vanilla rule reference with an optional args list.
        | PMany(many) -> (bracketsIf (priority many<50) (printProduction many))@[Str("*")] //expr*
        | PMetaRef(rule_name, opt_arg, metaArgs) -> [Str((Source.toString rule_name)+(printMetaArgs metaArgs)+(printArg opt_arg))] // Metarule reference like in "a: mr<x> y z"
        | PLiteral(source) -> [Str(Source.toString source)] //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
//        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
//        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
///// The following are obsolete and reduction to PRepet should be discussed.
        | PSome(some) -> (bracketsIf (priority some<50) (printProduction some))@[Str("+")] //expr+
        | POpt(opt) -> (bracketsIf (priority opt<50) (printProduction opt))@[Str("?")] //expr?
        | _ -> [Str("ERROR")]

    [Line([Str(startSign+rule.name+(printMetaArgs rule.metaArgs)+(printArgs rule.args)+": ")]); Tabbed(printProduction rule.body@[Str(";")])]

let generate (input_grammar:Definition.t<Source.t,Source.t>) =
    let tbList = List.collect (fun rule -> printRule rule) input_grammar.grammar
    printSourceOpt(input_grammar.head)+(printTextBox 4 4 80 tbList)+printSourceOpt(input_grammar.foot)
