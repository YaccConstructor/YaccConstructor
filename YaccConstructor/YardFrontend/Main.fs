//  Copyright 2010, 2011, 2012 Jake Kirilenko
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

module Yard.Frontends.YardFrontend.Main

open Microsoft.FSharp.Text.Lexing
open System.Linq

module Lexer = Yard.Frontends.YardFrontend.GrammarLexer

let private bufFromFile path = 
    let content = System.IO.File.ReadAllText(path)
    Lexer.currentFileContent := content;
    Lexer.currentFile := path
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

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
//    let filter x =
//        let flg = 
//            if List.isEmpty !currentDefined 
//            then true
//            else (!currentDefined).All(fun (x,y) -> x)
//        flg

    let filtered =
        seq{
            for token in tokens do
                match token with
                | GrammarParser.SHARPLINE str ->
                    match str with
                    | IF d -> 
                        let x = Array.contains d userDefined
                        currentDefined := (x, x, !currentState)::!currentDefined
                        currentState := x && !currentState
                    | ELIF d ->
                        match !currentDefined with
                        | (_, prev, upper) :: tl -> 
                            let x = (Array.contains d userDefined) && (not prev)
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
    let tokensEnumerator = filtered.GetEnumerator()
    let getNextToken (lexbuf:Lexing.LexBuffer<_>) =
        tokensEnumerator.MoveNext() |> ignore
        let res = tokensEnumerator.Current
        res
    getNextToken
let ParseText (s:string) path =
    let buf = bufFromString s    
    let userDefs = [||]//
    GrammarParser.currentFilename := path
    Lexer.currentFile := path
    let posTo2D pos =
        let source = s
        source.ToCharArray(0, min (pos+1) (source.Length))
        |> Array.fold
            (fun (col,row) -> function
                | '\n' -> (col+1, 0)
                | '\r' -> (col, row)
                | _ -> (col, row+1)
            )
            (1,0)
    try
        GrammarParser.file (filterByDefs buf userDefs) <|Lexing.LexBuffer<_>.FromString "*this is stub*"
    with
    | Lexer.Lexical_error (msg, pos) ->
        let pos2D = posTo2D pos
        failwith <| sprintf "Lexical error in line %d position %d: %s" (fst pos2D) (snd pos2D) msg

let ParseFile (args:string) =
    let path,userDefs =
        let args = args.Trim().Split('%')
        let defs = 
            if args.Length = 2
            then args.[1].Split(';')
            else [||]
        args.[0], defs
        
    let buf = bufFromFile path
    GrammarParser.currentFilename := args
    let posTo2D pos =
        let source = System.IO.File.ReadAllText path
        source.ToCharArray(0, min (pos+1) (source.Length))
        |> Array.fold
            (fun (col,row) -> function
                | '\n' -> (col+1, 0)
                | '\r' -> (col, row)
                | _ -> (col, row+1)
            )
            (1,0)
    try
        GrammarParser.file (filterByDefs buf userDefs) <|Lexing.LexBuffer<_>.FromString "*this is stub*"
    with
    | Lexer.Lexical_error (msg, pos) ->
        let pos2D = posTo2D pos
        failwith <| sprintf "Lexical error in line %d position %d: %s" (fst pos2D) (snd pos2D) msg
    (*| GrammarParser.Parse_error msg ->
        let pos2D = posTo2D pos
        failwith <| sprintf "Lexical error in line %d position %d: %s" (fst pos2D) (snd pos2D) msg*)
    
let LexString string =
    Lexer.currentFileContent := string;
    let reader = new System.IO.StringReader(string)
    let buf = LexBuffer<_>.FromTextReader reader
    seq {
            while not buf.IsPastEndOfStream do
               yield Lexer.main buf  
        }
