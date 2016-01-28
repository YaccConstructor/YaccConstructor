module LexerHelper

open Microsoft.FSharp.Text.Lexing
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

let getKwTokenOrIdent = 
    //let kws = getLiteralNames |> List.map (fun s -> s.ToLower()) |> Set.ofList
    fun (name:string) (defaultSourceText : SourceText) ->
        //if kws.Contains (name.ToLowerInvariant()) then
        let kw = genLiteral name defaultSourceText  
        match kw with
        | Some x -> x
        | None ->  IDENT defaultSourceText

let lexeme lexbuf = LexBuffer<_>.LexemeString lexbuf

//function

let commendepth = ref 0
let startPos = ref Position.Empty
let str_buf = new System.Text.StringBuilder()

let appendBuf (str:string) = str_buf.Append(str) |> ignore
let clearBuf () = str_buf.Clear() |> ignore
  
let makeIdent notKeyWord (name:string) (startPos, endPos) =
  let prefix = 
    if String.length name >= 2 
    then name.[0..1] 
    else ""
  let defaultSourceText =  
    new SourceText(name, SourceRange.ofTuple (startPos,endPos))
  if prefix = "@@" then GLOBALVAR defaultSourceText
  //else if prefix = "##" then GLOBALTEMPOBJ name
  elif name.[0] = '@' then LOCALVAR defaultSourceText
  //else if name.[0] = '#' then TEMPOBJ name
  elif prefix = "%%" then STOREDPROCEDURE defaultSourceText
  elif notKeyWord then IDENT defaultSourceText
  else getKwTokenOrIdent name defaultSourceText


let defaultSourceText id (lexbuf : LexBuffer<_>) value =
    new SourceText(value
        , SourceRange.ofTuple(new Pair (id,int64 lexbuf.StartPos.AbsoluteOffset * _symbolL)
                               , new Pair(id, int64 lexbuf.EndPos.AbsoluteOffset * _symbolL)))

let getLiteral id (lexbuf : LexBuffer<_>) value =
    let range = 
        SourceRange.ofTuple(new Pair (id,int64 lexbuf.StartPos.AbsoluteOffset * _symbolL)
                               , new Pair(id, int64 lexbuf.EndPos.AbsoluteOffset * _symbolL))
    match genLiteral value (SourceText(value,range)) with
    | Some x -> x
    | None -> failwith <| sprintf "Literal with value %A not defined." value
    
        
let tokenPos token =
    let data = tokenData token
    if isLiteral token then
        data :?> uint64 * uint64
    else
        let x = data :?> SourceText
        x.Range.Start, x.Range.End
