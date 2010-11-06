module Yard.Generators.YardPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let printSourceOpt = function None -> "" | Some arg -> (fst arg)

let printRule (rule:Rule.t<Source.t,Source.t>) = 
    let startSign = if rule._public then "+" else ""
    let printAttr = function
        | Some(attr) -> "{"+(Source.toString attr)+"}"
        | None -> ""

    let rec printProduction (production:Production.t<Source.t,Source.t>) = 
        let printElem (elem:elem<Source.t,Source.t>) = 
            let binding = function
                | Some(x) -> (Source.toString x) + " = "
                | None -> ""
            (binding elem.binding) + (printProduction elem.rule)
        match production with
        | PAlt(alt1, alt2) -> "(" + (printProduction alt1) + " | " + (printProduction alt2) + ")"//Альтернатива
        | PSeq(elem_list,attr_option) -> (String.concat " " (List.map printElem elem_list)) + printAttr(attr_option)//Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
        | PToken(source) -> Source.toString source //собственно токен
        | PRef(source, attr_option) -> (Source.toString source) + printAttr(attr_option) //Vanilla rule reference with an optional args list.
//        |PMany    of (t<'patt,'expr>) //expr*
//        |PMetaRef of Source.t * 'expr option * 'expr list // Metarule reference like in "a: mr<x> y z"
//        |PLiteral of Source.t //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
//        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
//        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
///// The following are obsolete and reduction to PRepet should be discussed.
//        |PSome    of (t<'patt,'expr>) //expr+
//        |POpt     of (t<'patt,'expr>) //expr?
        | x -> "ERROR"

    startSign + rule.name + ": " + (printProduction rule.body) + ";"

let generate (input_grammar:Definition.t<Source.t,Source.t>) =
    printSourceOpt(input_grammar.head)+(List.fold (fun concated rule -> concated+(printRule rule)+"\n" ) "" input_grammar.grammar)+printSourceOpt(input_grammar.foot)

