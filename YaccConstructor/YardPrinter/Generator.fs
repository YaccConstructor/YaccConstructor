module Yard.Generators.YardPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let printSourceOpt = function None -> "" | Some arg -> (fst arg)

let printRule (rule:Rule.t<Source.t,Source.t>) = 
    let bracketsIf cond s = if cond then "("+s+")" else s
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
            ((if elem.omit then "-" else "") + (binding elem.binding) + (bracketsIf ((match elem.binding with Some(_) -> true | None -> priority elem.rule=1) && priority elem.rule<50) (printProduction elem.rule)))
        match production with
        | PAlt(alt1, alt2) -> (printProduction alt1) + " | " + (printProduction alt2) //Альтернатива
        | PSeq(elem_list,attr_option) -> (String.concat " " (List.map printElem elem_list)) + printAttr(attr_option) //Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
        | PToken(source) -> Source.toString source //собственно токен
        | PRef(source, attr_option) -> (Source.toString source) + printArg(attr_option) //Vanilla rule reference with an optional args list.
        | PMany(many) -> (bracketsIf (priority many<50) (printProduction many))+"*" //expr*
        | PMetaRef(rule_name, opt_arg, metaArgs) -> (Source.toString rule_name)+(printMetaArgs metaArgs)+(printArg opt_arg) // Metarule reference like in "a: mr<x> y z"
        | PLiteral(source) -> Source.toString source //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
//        |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
//        |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)   
///// The following are obsolete and reduction to PRepet should be discussed.
        | PSome(some) -> (bracketsIf (priority some<50) (printProduction some))+"+" //expr+
        | POpt(opt) -> (bracketsIf (priority opt<50) (printProduction opt))+"?" //expr?
        | x -> "ERROR"

    startSign + rule.name + (printMetaArgs rule.metaArgs) + (printArgs rule.args) + ": " + (printProduction rule.body) + ";"

let generate (input_grammar:Definition.t<Source.t,Source.t>) =
    printSourceOpt(input_grammar.head)+(List.fold (fun concated rule -> concated+(printRule rule)+"\n" ) "" input_grammar.grammar)+printSourceOpt(input_grammar.foot)

