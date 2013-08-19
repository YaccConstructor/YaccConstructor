//this lexer is used for checking test101
module Lexer

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error



let ws = spaces // skips any whitespace
let ch c =   skipChar c >>. ws
let str s =  pstring s .>> ws


let mk =  fun s -> Yard.Core.IL.Source.t(s)


// next two functions - part of lexer for c grammar
let pLETTER: Parser<_, unit> = (many1Satisfy isLower .>> ws) <|> (many1Satisfy isUpper .>> ws) 
                                    <|> str "_" <|> str "$" |>> mk 


let pNUMBER: Parser<_, unit> =  (pfloat .>>  ws) |>> (string >> mk)


let pPLUS: Parser<_, unit> = str "+" |>> mk

let pMINUS: Parser<_, unit> = str "-" |>> mk

let pMULT: Parser<_, unit> = str "*" |>> mk

let pDIV: Parser<_, unit> = str "/" |>> mk

let pPOW: Parser<_, unit> = str "**" |>> mk

let pEQUAL: Parser<_, unit> = str ":=" |>> mk

let pLEFT: Parser<_, unit> = str "(" |>> mk

let (pRIGHT : Parser<_, unit>) = str ")" |>> mk
let (pDOUBLE_COLON : Parser<_, unit>) = str ":" |>> mk

let pMODULE: Parser<_, unit> = str "module" |>> mk
let pINCLUDE : Parser<_, unit> = str "include" |>> mk
let pPUBLIC : Parser<_, unit> = str "public" |>> mk
let pPRIVATE : Parser<_, unit> = str "private" |>> mk
let pOPEN : Parser<_, unit> = str "open" |>> mk

let pSTRING : Parser<_, unit> = str "\"" .>> many (letter <|> digit <|> anyOf [' '; '\n'; '\r' ]) .>> str "\"" |>> mk
let pSTRING_CONST : Parser<_, unit> = str "\"" .>> 
                                      many (letter <|> digit <|> anyOf [' '; '\n'; '\r' ]) .>> str "\"" |>> mk

let pDEC_NUMBER : Parser<_, unit> = pfloat |>> (string >> mk)

let pOPTIONS_START : Parser<_, unit> = str "options" .>> ws .>> str "{" |>> mk
let pBLOCK_END : Parser<_, unit> = str "}" |>> mk

let pUIDENT : Parser<_, unit> = many(asciiUpper) |>> (string >> mk)
let pLIDENT : Parser<_, unit> = many(asciiLower <|> pchar '_') |>> (string >> mk)
let pALL_PUBLIC : Parser<_, unit> = str "AllPublic" |>> mk

let pCOMMA : Parser<_, unit> = str "," |>> mk
let pSEMICOLON : Parser<_, unit> = str ";" |>> mk
let pCOLON : Parser<_, unit> = str ":" |>> mk
let pACTION : Parser<_, unit> = str "{" |>> mk
let pSTART_RULE_SIGN : Parser<_, unit> = str "[<Start>]" |>> mk
let pLESS : Parser<_, unit> = str "<" |>> mk
let pGREAT : Parser<_, unit> = str ">" |>> mk
let pLPAREN : Parser<_, unit> = str "(" |>> mk
let pRPAREN : Parser<_, unit> = str ")" |>> mk
let pPREDICATE : Parser<_, unit> = str "=>{" |>> mk
let pPARAM : Parser<_, unit> = str "<<" |>> mk
let pSQR_LBR : Parser<_, unit> = str "[" |>> mk
let pSQR_RBR : Parser<_, unit> = str "]" |>> mk
let pBAR : Parser<_, unit> = str "|" |>> mk
let pSTAR : Parser<_, unit> = str "*" |>> mk
let pLITERAL : Parser<_, unit> = str "'" |>> mk
let pQUESTION : Parser<_, unit> = str "?" |>> mk


let pDLABEL : Parser<_, unit> = str "@" .>> asciiLetter .>> many(asciiLetter <|> pchar '_' <|> pchar '-') |>> mk
let pSHARPLINE : Parser<_,unit> = str "#" .>> (str "if" <|> str "elif" <|> str "else"<|> str "endif") .>> 
                                  many(asciiLetter <|> pchar ' ') |>> mk

let pIDENT : Parser<_, unit> = attempt(letter .>> many (letter <|> digit) |>> string) |>> mk
let pLOCALVAR : Parser<_, unit> = attempt(str "@" .>> pIDENT) |>> mk
let pGLOBALVAR : Parser<_, unit> = str "@@" .>> pIDENT |>> mk
let pSTOREDPROCEDURE : Parser<_, unit> = str "%" .>> pIDENT |>> mk
let pWEIGHT : Parser<_, unit> = str "MIN" <|> str "MAX"|>> mk
let pTOKENS_BLOCK : Parser<_, unit>  = 
    str  "tokens" .>> ws .>> str "{" .>> many(noneOf ['}']) .>> str "}" 
    |>> mk


let literal s = pstring s .>> ws |>> mk


let (pVAR : Parser<_, unit>)  =
    let idStr = many1Satisfy isLower .>> ws |>> mk// [a-z]+


    //let expectedId = expectedError "id"

    fun state -> // we define our own "primitive" that checks that the parsed id is no keyword
        let reply = idStr state
        Reply(reply.Status, reply.Result, reply.Error)



