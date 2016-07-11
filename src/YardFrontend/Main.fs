//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Frontends.YardFrontend.Main

open Microsoft.FSharp.Text.Lexing
open System.Linq
open Yard.Core.IL
open Yard.Frontends.YardFrontend
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open Microsoft.FSharp.Collections

module Lexer = Yard.Frontends.YardFrontend.GrammarLexer
open GrammarParser

let private tokenFun f = function
    | ACTION st
    | BAR st
    | AND st
    | COLON st
    | COMMA st
    | GREAT st
    | DLABEL st
    | NUMBER st    
    | LESS st
    | STARTREPEAT st
    | ENDREPEAT st
    | EOF st
    //| ERROR st
    | EQUAL st
    | INCLUDE st
    | LIDENT st
    | LPAREN st
    | MINUS st
    | PARAM st
    | PLUS st
    | PREDICATE st
    | QUESTION st
    | RNGLR_EOF st
    | RPAREN st
    | SQR_LBR st
    | SQR_RBR st
    | SEMICOLON st
//    | SET st
    | SHARPLINE st
    | STAR st
    | START_RULE_SIGN st
    | STRING st
    | ALL_PUBLIC st
    | MODULE st
    | PUBLIC st
    | PRIVATE st
    | OPEN st
    | UIDENT st
    | BLOCK_END st
    | TOKENS_BLOCK st
    | LITERAL st
    | OPTIONS_START st 
    | DOUBLEDOT st ->
        f st
    //| OPTION_BLOCK _ -> failwith "Unexpected OPTION_BLOCK"

let private tokenToRange = tokenFun <| fun st -> st.startPos, st.endPos
let private tokenToFile = tokenFun <| fun st -> st.file

let private bufFromFile path = 
    let content = System.IO.File.ReadAllText(path)
    Lexer.currentFileContent := content
    Lexer.currentFile := path
    let reader = new System.IO.StringReader(content)
    let res = LexBuffer<_>.FromTextReader reader
    res.EndPos <- res.EndPos.NextLine
    res

let private bufFromString string =
    Lexer.currentFileContent := string;
    let reader = new System.IO.StringReader(string)
    LexBuffer<_>.FromTextReader reader

let (|IF|ELSE|ELIF|ENDIF|) (str:string) =
    let tStr = str.Trim()
    if tStr.StartsWith("#if")
    then IF (tStr.Split(' ').[1])
    elif tStr.StartsWith("#else")
    then ELSE
    elif tStr.StartsWith("#elif")
    then ELIF (tStr.Split(' ').[1])
    elif tStr.StartsWith("#endif")
    then ENDIF
    else "Unexpected macrocommand " + str |> failwith

let private filterByDefs (buf:LexBuffer<_>) userDefined =     
    let tokens =
        seq {
                while not buf.IsPastEndOfStream do
                   yield Lexer.main buf  
            }

    let currentDefined = ref [] 
    let currentState = ref true

    let filtered =
        seq{
            for token in tokens do
                match token with
                | GrammarParser.SHARPLINE str ->
                    match str.text with
                    | IF d -> 
                        let x = Array.exists ((=)d) userDefined
                        currentDefined := (x, x, !currentState)::!currentDefined
                        currentState := x && !currentState
                    | ELIF d ->
                        match !currentDefined with
                        | (_, prev, upper) :: tl -> 
                            let x = (Array.exists ((=)d) userDefined) && (not prev)
                            currentDefined :=  (x, prev || x, upper) :: tl
                            currentState := x && upper
                        | _ -> failwith "Unexpected #ELIF"
                    | ELSE ->
                        match !currentDefined with
                        | (_, prev, upper) :: tl -> 
                            currentDefined :=  (not prev, prev, upper) :: tl
                            currentState := (not prev) && upper
                        | _ -> failwith "Unexpected #ELSE"
                    | ENDIF ->
                        match !currentDefined with
                        | (_, _, upper) :: tl -> 
                            currentDefined := tl
                            currentState := upper
                        | _ -> failwith "Unexpected #ENDIF"
                | t -> if !currentState then yield t
            }
    filtered

let private parse buf userDefs =
    let rangeToString (b : Source.Position, e : Source.Position) =
        sprintf "((%d,%d)-(%d,%d))" b.line b.column e.line e.column
    //let tokens = List.ofSeq (filterByDefs buf userDefs)
   // tokens |> Seq.iter (fun t -> printfn "%A: %A" t (rangeToString <| tokenToRange t))
    match GrammarParser.buildAst (filterByDefs buf userDefs) with
    | Parser.Success (ast, _, dict) ->
        ast.collectWarnings tokenToRange
        |> ResizeArray.iter (fun (x,y) -> fprintfn stderr "Ambiguity: %s %A" (rangeToString x) y)
        let args = {
                tokenToRange = tokenToRange
                zeroPosition = new Source.Position()
                clearAST = false
                filterEpsilons = true
            }
        ast.ChooseLongestMatch()
        try
            (GrammarParser.translate args ast dict : Definition.t<Source.t, Source.t> list).Head
        with
        | ParseError (src, msg) ->
            failwithf "Parse error on position %s:%s. %s: %s" src.file
                        (rangeToString (src.startPos, src.endPos)) msg src.text
    | Parser.Error (_, token, msg, debugs, _) -> 
        debugs.drawGSSDot "res.dot"
        failwithf "Parse error on position %s on token %A: %s"  (token |> Array.map (tokenToRange >> rangeToString) |> String.concat "; ") token msg
        failwithf "Parse error on position %s on token %A: %s"  (token.[0] |> tokenToRange |> rangeToString) token msg
    
    
let posTo2D (source:string) pos =    
    source.ToCharArray(0, min (pos+1) (source.Length))
    |> Array.fold
        (fun (col,row) -> function
            | '\n' -> (col+1, 0)
            | '\r' -> (col, row)
            | _ -> (col, row+1)
        )
        (1,0)

let ParseText (s:string) path =    
    let userDefs = [||]
    GrammarParser.currentFilename := path
    Lexer.currentFile := path
    try parse (bufFromString s) userDefs
    with
        Lexer.Lexical_error (msg, pos) ->
            let line,col = posTo2D s pos
            failwith <| sprintf "\nLexical error in line %d position %d: %s\n" line col msg

let rec ParseFile (args:string) =
    Yard.Frontends.YardFrontend.GrammarParser.parseFile := ParseFile
    let path,userDefs =
        let args = args.Trim().Split('%')
        let defs = 
            if args.Length = 2
            then args.[1].Split(';')
            else [||]
        args.[0], defs
        
    let buf = bufFromFile path
    GrammarParser.currentFilename := args
    let posTo2D = System.IO.File.ReadAllText path |> posTo2D
    try
        parse buf userDefs
    with
    | Lexer.Lexical_error (msg, pos) ->
        let pos2D = posTo2D pos
        failwith <| sprintf "\nLexical error in line %d position %d: %s\n" (fst pos2D) (snd pos2D) msg
      
let LexString string =
    Lexer.currentFileContent := string;
    let reader = new System.IO.StringReader(string)
    let buf = LexBuffer<_>.FromTextReader reader
    seq {
            while not buf.IsPastEndOfStream do
               yield Lexer.main buf  
        }
