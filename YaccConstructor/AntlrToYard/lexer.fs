module Lexer

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

let (pCHAR : Parser<char, unit>) = anyChar //pLETTER


let literal s = pstring s .>> ws


let (pVAR : Parser<string, unit>)  =
    let idStr = many1Satisfy isLower .>> ws // [a-z]+


    let expectedId = expectedError "id"

    fun state -> // we define our own "primitive" that checks that the parsed id is no keyword
        let reply = idStr state
        if reply.Status = Ok then
            let id = reply.Result
            
            Reply(reply.Result, reply.State)

        else // reconstruct error
            Reply(reply.Status, reply.Error, reply.State)


