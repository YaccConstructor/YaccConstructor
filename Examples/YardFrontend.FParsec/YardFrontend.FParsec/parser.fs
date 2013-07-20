module parser

open FParsec.Primitives

//  Copyright 2009 Jake Kirilenko
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

#nowarn "62";; 
open Microsoft.FSharp.Text
open Yard.Core.IL
open Yard.Core
open Yard.Core.IL.Production
open Yard.Core.IL.Grammar
open Yard.Core.IL.Definition
open System.Text.RegularExpressions
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception ParseError of Source.t * string
let parseFile = ref Unchecked.defaultof<_>
let currentFilename = ref ""
let allPublic = ref false
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []
let fst_ (x,_,_) = x
let snd_ (_,x,_) = x
let trd_ (_,_,x) = x

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let makeNewSeq seq (lbl:Source.t) (w:Option<Source.t>) = 
    match seq,w with 
     | PSeq(els,ac,_),_ -> match w with
                           | None -> PSeq (els,ac,Some {label=lbl.text; weight=None})
                           | _ -> let wVal = w.Value
                                  try
                                      PSeq (els,ac,Some {label=lbl.text; weight = Some (float wVal.text)})
                                  with 
                                  | :? System.FormatException as ex ->
                                      failwithf "Parse error on position (%i,%i) on token %s: %s" wVal.startPos.line wVal.startPos.column wVal.text "illegal weight. Number expected."
     | x,_ -> x

let missing name = System.Console.WriteLine("Missing " + name)
let createSeqElem bnd omitted r check =
    { binding = bnd; omit = omitted; rule = r; checker = check }

let parseRules (filename:string) : Definition.t<Source.t, Source.t> =
    let oldFileName = !currentFilename
    currentFilename := filename
    let ext = filename.Substring(filename.LastIndexOf(".") + 1)    
    let userDefs =
        let args = oldFileName.Trim().Split('%') in
        if args.Length = 2
        then args.[1]
        else ""
    let sameDirFilename = System.IO.Path.Combine(System.IO.Path.GetDirectoryName oldFileName, filename) in
    let res = !parseFile (sameDirFilename + "%" + userDefs)
    currentFilename := oldFileName
    res
let rec private kw   = (attempt (Lexer.pMODULE |>> fun (_S1) -> (_S1)   )) <|> ((attempt ((attempt ((attempt (Lexer.pINCLUDE |>> fun (_S1) -> (_S1)   )) <|> (Lexer.pOPEN |>> fun (_S1) -> (_S1)   ))) <|> (Lexer.pPUBLIC |>> fun (_S1) -> (_S1)   ))) <|> (Lexer.pPRIVATE |>> fun (_S1) -> (_S1)   ))

 and public file   = action_opt  >>= fun (_S1 ) -> (includes_or_options_or_tokens  >>= fun (_S2 ) -> (unnamed_module_opt  >>= fun (_S3 ) -> (modules  >>= fun (_S4 ) -> (foot_opt  |>> fun (_S5) -> ( 
        {
            info = { fileName = !currentFilename }
            head = _S1
            grammar = fst_ _S2 @ _S3 @ _S4
            foot = _S5
            options = snd_ _S2
            tokens = trd_ _S2
        }
      ) ))))  

 and private includes_or_options_or_tokens   = (attempt (preturn (  [],    Map.empty, Map.empty ))) <|> ((attempt ((attempt (include_  >>= fun (_S1 ) -> (includes_or_options_or_tokens  |>> fun (_S2) -> ( (_S1 @ fst_ _S2), snd_ _S2, trd_ _S2) )  )) <|> (option_block  >>= fun (_S1 ) -> (includes_or_options_or_tokens  |>> fun (_S2) -> ( fst_ _S2, joinMaps _S1 (snd_ _S2), trd_ _S2) )  ))) <|> (tokens_block  >>= fun (_S1 ) -> (includes_or_options_or_tokens  |>> fun (_S2) -> ( fst_ _S2, snd_ _S2, joinMaps _S1 (trd_ _S2)) )  ))

 and private tokens_block   = 
     Lexer.pTOKENS_BLOCK |>> fun (_S1) -> 
     (
        let block = _S1.text
        let inner = block.[block.IndexOf '{' + 1 .. block.LastIndexOf '}' - 1]
        inner.Split [|'|'|]
        |> Array.map (fun s -> s.Trim())
        |> Array.filter ((<>) "")
        |> Array.map (fun s ->
            let genError msg = raise <| ParseError (new Source.t(s, Source.Position(), Source.Position(), !currentFilename),
                                                    "Error in tokens block: " + msg)
            if Regex.IsMatch(s, @"^(_|[A-Z][A-Za-z0-9_]*)$") then s, None
            else
                let m = Regex.Match(s, @"^(_|[A-Z][A-Za-z0-9_]*)\s*of\s*(.*)$")
                if m.Success then
                    m.Groups.[1].Value, Some m.Groups.[2].Value
                else
                    if not (System.Char.IsUpper s.[0]) && not (s.[0] = '_' && s.Length > 1 && System.Char.IsWhiteSpace s.[1])
                    then genError "Terminal must start from upper letter"
                    else genError "Token type description is incorrect"
            
        ) |> Map.ofArray
      )   

 and private include_   = Lexer.pINCLUDE >>= fun (_ ) -> (Lexer.pSTRING |>> fun (_S2) -> (
        let grammar = (parseRules _S2.text).grammar
        if grammar |> List.exists (fun m -> m.name.IsNone) then
            eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
        grammar
    ) )  

 and private option_block   = Lexer.pOPTIONS_START >>= fun (_ ) -> (opts  >>= fun (_S2 ) -> (Lexer.pBLOCK_END |>> fun (_) -> ( Map.ofList _S2 : Map<_,_>) ))  

 and private opts   = (attempt (option  >>= fun (_S1 ) -> (opts  |>> fun (_S2) -> ( _S1::_S2 ) )  )) <|> (preturn  [] )

 and private option   = option_l_value  >>= fun (_S1 ) -> (Lexer.pEQUAL >>= fun (_ ) -> (option_value  |>> fun (_S3) -> ( (_S1 : Source.t).text, (_S3 : Source.t).text ) ))  

 and private option_value   = (attempt (ident  |>> fun (_S1) -> ( _S1 )   )) <|> ((attempt (Lexer.pSTRING |>> fun (_S1) -> ( _S1 )   )) <|> (kw  |>> fun (_S1) -> ( _S1 )   ))

 and private option_l_value   = (attempt (ident  |>> fun (_S1) -> ( _S1 )   )) <|> (kw  |>> fun (_S1) -> ( _S1 )   )

 and private unnamed_module_opt   = rule_nlist  |>> fun (_S1) -> (
        match _S1 with
        | [] -> []
        | x ->  defaultModules x
    )   

 and private modules   = (attempt (module_  >>= fun (_S1 ) -> (modules  |>> fun (_S2) -> ( _S1 :: _S2 ) )  )) <|> (preturn  [] )

 and private module_   = module_header  >>= fun (_S1 ) -> (ident  >>= fun (_S2 ) -> (openings  >>= fun (_S3 ) -> (rule_nlist  |>> fun (_S4) -> (
        {
            allPublic = _S1
            name = Some _S2
            openings = _S3
            rules = _S4
        }
    ) )))  

 and private ident   = (attempt (Lexer.pUIDENT |>> fun (_S1) -> ( _S1 )   )) <|> (Lexer.pLIDENT |>> fun (_S1) -> ( _S1 )   )

 and private module_header   = (attempt (Lexer.pALL_PUBLIC >>= fun (_ ) -> (Lexer.pMODULE |>> fun (_) -> (
                  (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                  allPublic := true; true) )  )) <|> (Lexer.pMODULE |>> fun (_) -> ( allPublic := false; false )   )

 and private openings   = (attempt (preturn  [] )) <|> (Lexer.pOPEN >>= fun (_ ) -> (ident  >>= fun (_S2 ) -> (open_list  |>> fun (_S3) -> ( _S2::_S3 ) ))  )

 and private open_list   = (attempt (Lexer.pCOMMA >>= fun (_ ) -> (ident  >>= fun (_S2 ) -> (open_list  |>> fun (_S3) -> ( _S2::_S3 ) ))  )) <|> (preturn  [] )

 and private action_opt   = (attempt (preturn  None )) <|> (Lexer.pACTION |>> fun (_S1) -> ( Some _S1 )   )

 and private foot_opt   = (attempt (preturn  None )) <|> (Lexer.pSEMICOLON >>= fun (_ ) -> (Lexer.pACTION |>> fun (_S2) -> ( Some _S2 ) )  )

 and private rule_nlist   = (attempt (rule  >>= fun (_S1 ) -> (semi_opt  >>= fun (_ ) -> (rule_nlist  |>> fun (_S3) -> ( _S1::_S3 ) ))  )) <|> (preturn  [] )

 and private rule   = start_rule_sign_opt  >>= fun (_S1 ) -> (access_modifier_opt  >>= fun (_S2 ) -> (Lexer.pLIDENT >>= fun (_S3 ) -> (formal_meta_param_opt  >>= fun (_S4 ) -> (param_list  >>= fun (_S5 ) -> (Lexer.pCOLON >>= fun (_ ) -> (alts  |>> fun (_S7) -> ( 
        {
            Rule.isStart = _S1
            Rule.isPublic = _S2
            Rule.name = _S3
            Rule.metaArgs = getList _S4
            Rule.body = _S7
            Rule.args = _S5
        }
    ) ))))))  

 and private start_rule_sign_opt   = (attempt (preturn false)) <|> (Lexer.pSTART_RULE_SIGN |>> fun (_) -> (true)   )

 and private access_modifier_opt   = (attempt (Lexer.pPUBLIC |>> fun (_) -> ( true )   )) <|> ((attempt (Lexer.pPRIVATE |>> fun (_) -> ( false )   )) <|> (preturn  !allPublic ))

 and private formal_meta_param_opt   = (attempt (preturn  None )) <|> (Lexer.pLESS >>= fun (_ ) -> (formal_meta_list  >>= fun (_S2 ) -> (Lexer.pGREAT |>> fun (_) -> (Some _S2) ))  )

 and private formal_meta_list   = (attempt (Lexer.pLIDENT |>> fun (_S1) -> ([_S1])   )) <|> (Lexer.pLIDENT >>= fun (_S1 ) -> (formal_meta_list  |>> fun (_S2) -> (_S1::_S2) )  )

 and private param_opt   = (attempt (preturn  None )) <|> (Lexer.pPARAM |>> fun (_S1) -> (Some _S1)   )

 and private param_list   = (attempt (preturn  [] )) <|> (Lexer.pPARAM >>= fun (_S1 ) -> (param_list  |>> fun (_S2) -> (_S1::_S2) )  )

 and private weight_opt   = (attempt (preturn  None )) <|> (Lexer.pSQR_LBR >>= fun (_ ) -> (Lexer.pNUMBER >>= fun (_S2 ) -> (Lexer.pSQR_RBR |>> fun (_) -> ( Some _S2) ))  )

 and private alts   = (attempt (seq  |>> fun (_S1) -> ( _S1 )   )) <|> (seq  >>= fun (_S1 ) -> (bar_seq_nlist  |>> fun (_S2) -> (PAlt (_S1,_S2)) )  )

 and private bar_seq_nlist   = (attempt (Lexer.pBAR >>= fun (_ ) -> (seq  >>= fun (_S2 ) -> (bar_seq_nlist  |>> fun (_S3) -> (PAlt(_S2,_S3) ) ))  )) <|> (Lexer.pBAR >>= fun (_ ) -> (seq  |>> fun (_S2) -> (_S2) )  )

 and private seq   = (attempt (lbl_seq  |>> fun (_S1) -> (_S1)   )) <|> (no_lbl_seq  |>> fun (_S1) -> (_S1)   )

 and private no_lbl_seq   = (attempt (seq_elem  >>= fun (_S1 ) -> (seq_elem_list  >>= fun (_S2 ) -> (action_opt  |>> fun (_S3) -> ( PSeq (_S1::_S2, _S3, None)) ))  )) <|> (Lexer.pACTION |>> fun (_S1) -> ( PSeq([], Some _S1, None) )   )

 and private lbl_seq   = Lexer.pDLABEL >>= fun (_S1 ) -> (weight_opt  >>= fun (_S2 ) -> (Lexer.pLPAREN >>= fun (_ ) -> (no_lbl_seq  >>= fun (_S4 ) -> (Lexer.pRPAREN |>> fun (_) -> (makeNewSeq _S4 _S1 _S2) ))))  

 and private seq_elem_list   = (attempt (preturn  [] )) <|> (seq_elem  >>= fun (_S1 ) -> (seq_elem_list  |>> fun (_S2) -> (_S1::_S2) )  )

 and private seq_elem   = omit_opt  >>= fun (_S1 ) -> (bound () >>= fun (_S2 ) -> (predicate_opt  |>> fun (_S3) -> ({_S2 with checker = _S3; omit = _S1 }) ))  

 and private omit_opt   = (attempt (preturn  false )) <|> (Lexer.pMINUS |>> fun (_) -> ( true )   )

 and private semi_opt   = (attempt (preturn  false )) <|> (Lexer.pSEMICOLON |>> fun (_) -> (true)   )

 and private predicate_opt   = (attempt (preturn  None )) <|> (Lexer.pPREDICATE |>> fun (_S1) -> ( Some _S1 )   )

 and private bound ()  = (attempt (patt  >>= fun (_S1 ) -> (Lexer.pEQUAL >>= fun (_ ) -> (prim () |>> fun (_S3) -> ( createSeqElem (Some _S1) false _S3 None ) ))  )) <|> (prim () |>> fun (_S1) -> ( createSeqElem None false _S1 None      )   )

 and private patt   = (attempt (Lexer.pLIDENT |>> fun (_S1) -> (_S1)   )) <|> (Lexer.pACTION |>> fun (_S1) -> (_S1)   )

 and private prim ()  = (attempt (prim () >>= fun (_S1 ) -> (Lexer.pSTAR |>> fun (_) -> (PMany _S1) )  )) <|> ((attempt ((attempt ((attempt ((attempt ((attempt ((attempt (prim () >>= fun (_S1 ) -> (Lexer.pPLUS |>> fun (_) -> (PSome _S1) )  )) <|> (prim () >>= fun (_S1 ) -> (Lexer.pQUESTION |>> fun (_) -> (POpt _S1) )  ))) <|> (Lexer.pSQR_LBR >>= fun (_ ) -> (alts  >>= fun (_S2 ) -> (Lexer.pSQR_RBR |>> fun (_) -> (POpt _S2) ))  ))) <|> (Lexer.pLPAREN >>= fun (_ ) -> (alts  >>= fun (_S2 ) -> (Lexer.pRPAREN |>> fun (_) -> (_S2) ))  ))) <|> (call  |>> fun (_S1) -> (_S1)   ))) <|> (lbl_seq  |>> fun (_S1) -> (_S1)   ))) <|> (Lexer.pLITERAL |>> fun (_S1) -> (PLiteral _S1)   ))

 and private meta_param   = prim () |>> fun (_S1) -> (_S1)   

 and private meta_params   = (attempt (meta_param  |>> fun (_S1) -> ([_S1])   )) <|> (meta_param  >>= fun (_S1 ) -> (meta_params  |>> fun (_S2) -> (_S1 :: _S2) )  )

 and private meta_param_opt   = (attempt (preturn  None )) <|> (Lexer.pLESS >>= fun (_ ) -> (meta_params  >>= fun (_S2 ) -> (Lexer.pGREAT |>> fun (_) -> (Some _S2) ))  )

 and private call   = (attempt (Lexer.pUIDENT |>> fun (_S1) -> (PToken _S1)   )) <|> (Lexer.pLIDENT >>= fun (_S1 ) -> (meta_param_opt  >>= fun (_S2 ) -> (param_opt  |>> fun (_S3) -> ( match _S2 with
        | None -> PRef  (_S1, _S3)
        | Some x -> PMetaRef (_S1,_S3,x)
      ) ))  )

 and private tada_rule   = Lexer.pSHARPLINE |>> fun (_) -> ()   