module LexerHelper

//open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Yard.Examples.MSParser
open Yard.Utils.SourceText
open Yard.Utils.StructClass

open System

type Collections.Generic.IDictionary<'k,'v> with
    member d.TryGetValue' k = 
        let mutable res = Unchecked.defaultof<'v> 
        let exist = d.TryGetValue(k, &res)
        if exist then Some res else None
    member d.Add'(k,v) =
        if not (d.ContainsKey k) then d.Add(k,v);true else false

exception IdentToken

let replacementDict =
    [
        '.', "dot"
        ',', "comma"
        ':', "semi"
        ';', "colon"
        '+', "plus"
        '-', "minus"
        '*', "star"
        '<', "less"
        '>', "more"
        '=', "equal"
        '/', "slash"
        '&', "and"
        '|', "or"
        '?', "question"
        '$', "dollar"
        '[', "left_square_bracket"
        ']', "right_square_bracket"
        '(', "left_bracket"
        ')', "right_bracket"
        '!', "not"
        '~', "tilda"
        '#', "sharp"
        '%', "percent"
        '^', "hat"
        '{', "left_figure_bracket"
        '}', "right_figure_bracket"
        '\\', "reverse_slash"
        '`', "reverse_quate"
        ''', "quate"
        '№', "number"
    ]
    |> dict

        

let getKwTokenOrIdent = 
    let nameToUnionCtor (uci:UnionCaseInfo) = (uci.Name, FSharpValue.PreComputeUnionConstructor(uci))
    let ucis = FSharpType.GetUnionCases (typeof<Token>) |> Array.map nameToUnionCtor  |> dict 
    let kws = getLiteralNames |> List.map (fun s -> s.ToLower()) |> Set.ofList
    fun (name:string) (defaultSourceText) ->
        let upperName = "KW_" + name.ToUpperInvariant()
        let kw = 
            ucis.TryGetValue' upperName
            |> Option.map (fun ctor ->  ctor [| defaultSourceText |] :?>Token) 
        match kw with 
        | None ->
            //if kws.Contains (name.ToLowerInvariant()) then
            let name = 
                name
                |> Seq.mapi  
                    (fun i ch ->
                        let exist,v = replacementDict.TryGetValue(ch)
                        if exist
                        then
                            if i = 0 
                            then v + "_"
                            elif i = name.Length - 1
                            then "_" + v
                            else "_" + v + "_"
                        else string ch
                    )
                |> String.concat ""
            match genLiteral name defaultSourceText with
            Some x as c -> c
            | None ->
            //else
                Some <| IDENT defaultSourceText
        | Some x -> kw

//let lexeme lexbuf = LexBuffer<_>.LexemeString lexbuf

//function

let commendepth = ref 0
//let startPos = ref Position.Empty
let str_buf = new System.Text.StringBuilder()

let appendBuf (str:string) = str_buf.Append(str) |> ignore
let clearBuf () = str_buf.Clear() |> ignore
  
let makeIdent notKeyWord (name:string) (startPos,endPos) =
  let prefix = 
    if String.length name >= 2
    then name.[0..1] 
    else ""
  let defaultSourceText =  
    "",startPos//new SourceText(name, new SourceRange(0UL, 0UL)),startPos// . ofTuple (startPos,endPos))
  if prefix = "@@" then GLOBALVAR defaultSourceText
  //else if prefix = "##" then GLOBALTEMPOBJ name
  elif name.[0] = '@' then LOCALVAR defaultSourceText
  //else if name.[0] = '#' then TEMPOBJ name
  elif prefix = "%%" then STOREDPROCEDURE defaultSourceText
  elif notKeyWord then IDENT defaultSourceText
  else 
    match getKwTokenOrIdent name defaultSourceText with
    | Some x -> x
    | None -> failwithf "Fail to get token with name %s " name


let defaultSourceText id brs value = ""
//    new SourceText(value
//        , SourceRange.ofTuple(new Pair (id,int64 1 * _symbolL)
//                               , new Pair(id, int64 1 * _symbolL)))

let getLiteral id brs (*lexbuf : LexBuffer<_>*) value =
//    let range = 
//        SourceRange.ofTuple(new Pair (id,int64 lexbuf.StartPos.AbsoluteOffset * _symbolL)
//                               , new Pair(id, int64 lexbuf.EndPos.AbsoluteOffset * _symbolL))
    (*match*) genLiteral value ((defaultSourceText id brs value),brs) //with
    (*| Some x -> x
    | None -> failwithf "Fail to get token with name %s " value*)
        
let tokenPos token =
    let data = tokenData token
    if isLiteral token then
        data :?> uint64 * uint64
    else
        let x = data :?> SourceText
        x.Range.Start, x.Range.End
