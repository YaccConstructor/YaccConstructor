//this lexer is used for checking test101
module Lexer

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error



let ws = spaces // skips any whitespace
let ch c =   skipChar c >>. ws
let str s =  pstring s .>> ws


// next two functions - part of lexer for c grammar
let pLETTER: Parser<string, unit> = (many1Satisfy isLower .>> ws) <|> (many1Satisfy isUpper .>> ws) <|> str "_" <|> str "$" 


let pNUMBER: Parser<float, unit> =  pfloat .>>  ws 


let pPLUS: Parser<string, unit> = str "+"

let pMINUS: Parser<string, unit> = str "-"

let pMULT: Parser<string, unit> = str "*"

let pDIV: Parser<string, unit> = str "/"

let pPOW: Parser<string, unit> = str "**"

let pEQUAL: Parser<string, unit> = str ":="

let pLEFT: Parser<string, unit> = str "("

let (pRIGHT : Parser<string, unit>) = str ")"
let (pDOUBLE_COLON : Parser<string, unit>) = str ":"

    
let pSTRING_CONST : Parser<_, unit> = str "\"" .>> many (letter <|> digit <|> anyOf [' '; '\n'; '\r' ]) .>> str "\""

let pDEC_NUMBER : Parser<_, unit> = pfloat 

let pIDENT : Parser<_, unit> = attempt(letter .>> many (letter <|> digit) |>> string)
let pLOCALVAR : Parser<_, unit> = attempt(str "@" .>> pIDENT)
let pGLOBALVAR : Parser<_, unit> = str "@@" .>> pIDENT
let pSTOREDPROCEDURE : Parser<_, unit> = str "%" .>> pIDENT
let pWEIGHT : Parser<_, unit> = str "MIN" <|> str "MAX"

let literal s = pstring s .>> ws


let (pVAR : Parser<string, unit>)  =
    let idStr = many1Satisfy isLower .>> ws // [a-z]+


    //let expectedId = expectedError "id"

    fun state -> // we define our own "primitive" that checks that the parsed id is no keyword
        let reply = idStr state
        Reply(reply.Status, reply.Result, reply.Error)



