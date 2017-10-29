module LexerHelper

open System
open Microsoft.FSharp.Reflection
open GLL.MSParser

let gr = 0
let commendepth = ref 0

type Collections.Generic.IDictionary<'k,'v> with
    member d.TryGetValue' k = 
        let mutable res = Unchecked.defaultof<'v> 
        let exist = d.TryGetValue(k, &res)
        if exist then Some res else None
    member d.Add'(k,v) =
        if not (d.ContainsKey k) then d.Add(k,v);true else false

let getToken =
    let nameToUnionCtor (uci : UnionCaseInfo) = (uci.Name, FSharpValue.PreComputeUnionConstructor uci)
    let ucis = FSharpType.GetUnionCases (typeof<Token>) |> Array.map nameToUnionCtor |> dict
    fun (name: string) ->
      let upperName = name.ToUpper()
      ucis.TryGetValue' upperName
      |> Option.map (fun ctor -> ctor [| gr |] :?> Token)

let makeIdent (name : string) =
    let (|Prefix|_|) (p:string) (s:string) =
      if s.StartsWith(p) then
        Some (s.Substring(p.Length))
      else
        None

    match name with
      | Prefix "@@" rest -> GLOBALVAR(gr)
      | Prefix "@"  rest -> LOCALVAR(gr)
      | Prefix "%%" rest -> STOREDPROCEDURE(gr)
      | _ -> match getToken name with
              | Some (token) -> token
              | None -> IDENT(gr)
