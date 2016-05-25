module Yard.Generators.RNGLR.ReadBack.AstActionCodeGenerator

open System
open Yard.Core.IL
open System.Text
open Yard.Core.IL.Production
open Yard.Core.IL.Rule

//supposed one nonterm - one rule
let setTranslateToTreeMeta (grammar : Rule.t<'patt,'expr> list) (prefix : string) =
    
    //let moduleName = prefix + "Ast"
    let astStringBuilder = new StringBuilder()
    let inline print (x : 'a) =
        Printf.kprintf (fun s -> astStringBuilder.Append s |> ignore) x

    //print "module %s\n" moduleName
    //print "\n"

    let upperFirstLetter (s : string) =
        if String.IsNullOrEmpty s || Char.IsUpper s.[0] then
            s
        else
            s.ToUpper().[0].ToString() + s.Substring(1)

    let typeCount = ref 0

    let defaultPrefix = "T"
    let prefix = if String.IsNullOrEmpty prefix then defaultPrefix else upperFirstLetter prefix
    let unionName name = prefix + (upperFirstLetter name)
    let typeName name = upperFirstLetter name
    
    let addType name = 
        let intro = 
            if !typeCount = 0 then
                "type "
            else
                "and "
        print "%s" (intro + (unionName name) + " = " + (typeName name) + " of ")
    
    //choice: 0 for no choice, 1 and 2 for the first and second alternatives
    let rec visitBody choice = function
    | PSeq (elems, _, dl) ->
        let newMeta = ref ""
        if elems.Length > 1 then
            newMeta := "("
        let newElems =
            let f i (e : elem<Source.t, Source.t>) = 
                let bnd = sprintf "%sbnd%d" prefix i
                if i > 0 then
                    print " * "
                    newMeta := !newMeta + ","
                newMeta := !newMeta + bnd
                { binding = Some (new Source.t(bnd)); omit = e.omit; rule = visitBody 0 e.rule; checker = e.checker }
            elems |> List.mapi f
        if elems.Length > 1 then
            newMeta := !newMeta + ")"
        if choice > 0 then
            newMeta := sprintf "Choice%dOf2 %s" choice !newMeta
        PSeq (newElems, Some (new Source.t(!newMeta)), dl)
    | x ->
        if choice > 0 then
            let e : elem<_,_> = {binding = None; omit = false; rule = x; checker = None}
            visitBody choice (PSeq ([e], None, None))
        else
            match x with
            | PAlt (first, second) -> 
                print "Choice<"
                let first = visitBody 1 first
                print ", "
                let second = visitBody 2 second
                print ">"
                PAlt(first, second)
            /// Token itself. Final element of parsing.
            |PToken x as ptoken ->
                print "Token"
                ptoken
            /// Reference to other rule inside production. With an optional args list.
            |PRef (name, _) as x ->
                print "%s" << unionName <| Source.toString name
                let newMeta = sprintf "|> %s" << typeName <| Source.toString name
                PRef (name, Some <| new Source.t(newMeta))
            |PMany x ->
                print "("
                let x = visitBody 0 x
                print ") list"
                PMany x
            /// Reference to metarule inside production (mr<<x>> in rule "a: mr<<x>> y z")
            |PLiteral x as pliteral->
                print "Token"
                pliteral
            |PSome x ->
                print "("
                let x = visitBody 0 x
                print ") list"
                PSome x 
            /// expr?
            |POpt x ->
                print "("
                let x = visitBody 0 x
                print ") option"
                POpt x 
            | x -> 
                x

    let newGrammar = grammar |> List.map (fun rule -> 
    
        addType <| Source.toString rule.name
        incr typeCount
        let newBody = visitBody 0 rule.body
        print "\n\n"
        {name = rule.name; args = rule.args; body = newBody; metaArgs = rule.metaArgs; isStart = rule.isStart; isPublic = rule.isPublic} )
    (newGrammar, astStringBuilder.ToString(), typeName)

