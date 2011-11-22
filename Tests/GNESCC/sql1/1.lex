{
(*
| IDENT         (* of string // Some, [Some] *)
| LOCALVAR      (* of string // @Some *)
| TEMPOBJ       (* of string // #Some *)
| GLOBALTEMPOBJ (* of string // ##Some *)
| GLOBALVAR     (* of string // @@Some  *)
*)

module xFormer.Translator.Lexer

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Reflection

let getKwToken = 
    let nameToUnionCtor (uci:UnionCaseInfo) = (uci.Name, FSharpValue.PreComputeUnionConstructor(uci))
    let ucis = FSharpType.GetUnionCases(typeof<token>) 
                    |> Array.map nameToUnionCtor                       
                    |> dict
    fun (name:string) startPos endPos ->
    let upperName = "KW_" + name.ToUpper()
    let (present, ctor) = ucis.TryGetValue(upperName) 
    if present then
        Some(ctor [| SourceText.ofTuple(name, (startPos, endPos)) |] :?>token)
    else
        None

let comment_depth = ref 0
let startPos = ref Position.Empty
let str_buf = new System.Text.StringBuilder()

let appendBuf (str:string) = str_buf.Append(str) |> ignore
let clearBuf () = str_buf.Clear() |> ignore
  
let makeIdent (name:string) (notKeyWord) (startPos, endPos) =
    let prefix = if name.Length>=2 then name.[0..1] else ""
    if prefix = "@@" then GLOBALVAR(SourceText.ofTuple(name,(startPos,endPos)))
    else if prefix = "##" then GLOBALTEMPOBJ(SourceText.ofTuple(name,(startPos,endPos)))
    else if name.[0] = '@' then LOCALVAR(SourceText.ofTuple(name,(startPos,endPos)))
    else if name.[0] = '#' then TEMPOBJ(SourceText.ofTuple(name,(startPos,endPos)))
    else if notKeyWord then IDENT(SourceText.ofTuple(name,(startPos,endPos)))
    else 
        match getKwToken name startPos endPos with
        | Some(kwToken) -> kwToken
        | None -> IDENT(SourceText.ofTuple(name,(startPos,endPos)))            
}

let eol = '\r' | '\n' | '\r' '\n' (* See script.sql position (4560,27) *)
let whitespaces = [' '  '\t']+
let ident_start_char  = ['A'-'Z' 'a'-'z' '_' '@' '#' 'а'-'я' 'А'-'Я' ] 
let ident_body_char  = ['A'-'Z' 'a'-'z' '_' '0'-'9' '@' '#' '$' 'а'-'я' 'А'-'Я' ] 
// Разобраться с идентификаторами cyrillic с,а (885574,_) (1004524)
let ident = ident_start_char ident_body_char*
let decnumber = ['0'-'9']+ 
let hexnumber = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let label = ident ':'


rule token = parse
	| label { LABEL(SourceText.ofTuple(LexBuffer<_>.LexemeString(lexbuf),(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | ident 
        { 
            (lexbuf.StartPos, lexbuf.EndPos)
            |> makeIdent (LexBuffer<_>.LexemeString(lexbuf)) (false)
            , lexbuf.StartPos
        }
    | '[' [^']' '\r' '\n']+ ']' 
	   { 
            let s=LexBuffer<_>.LexemeString(lexbuf)
            (lexbuf.StartPos, lexbuf.EndPos)
            |> makeIdent s.[1..(s.Length-2)] (true)
            , lexbuf.StartPos 
       }
    | '\'' | "N'" { clearBuf(); startPos := lexbuf.StartPos; literal lexbuf }
    | '"' { clearBuf(); startPos := lexbuf.StartPos; literal2 lexbuf }
    | "/*" { comment_depth := 1; startPos := lexbuf.StartPos; clearBuf(); multiline_comment lexbuf }
    | decnumber | hexnumber { (NUMBER(SourceText.ofTuple(LexBuffer<_>.LexemeString(lexbuf),(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos) }
    | "--" [^'\r' '\n']*  { (COMMENT(SourceText.ofTuple(LexBuffer<_>.LexemeString(lexbuf).Substring(2),(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos) }
    | whitespaces { token lexbuf }
    | eol { lexbuf.EndPos <- lexbuf.EndPos.NextLine; token lexbuf }
    | eof { EOF(SourceText.ofTuple("",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | "<>" { NOTEQUAL(SourceText.ofTuple("<>",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | "!=" { NOTEQUAL(SourceText.ofTuple("!=",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '.' { DOT(SourceText.ofTuple(".",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '+' { PLUS(SourceText.ofTuple("+",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '-' { MINUS(SourceText.ofTuple("-",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '*' { STAR(SourceText.ofTuple("*",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '/' { SLASH(SourceText.ofTuple("/",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '\\' { BACKSLASH(SourceText.ofTuple("\\",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '%' { PERCENT(SourceText.ofTuple("%",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '(' { LBRACE(SourceText.ofTuple("(",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | ')' { RBRACE(SourceText.ofTuple(")",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '>' { MORE(SourceText.ofTuple(">",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '<' { LESS(SourceText.ofTuple("<",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '=' { EQUAL(SourceText.ofTuple("=",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | ',' { COMMA(SourceText.ofTuple(",",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | ':' { COLON(SourceText.ofTuple(":",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | ';' { SEMICOLON(SourceText.ofTuple(";",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '&' { AMP(SourceText.ofTuple("&",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | '|' { VERTLINE(SourceText.ofTuple("|",(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
    | _ { UNRECOGNIZED(SourceText.ofTuple(LexBuffer<_>.LexemeString(lexbuf),(lexbuf.StartPos,lexbuf.EndPos))), lexbuf.StartPos }
and multiline_comment = parse
  | "/*" 
    { incr comment_depth; appendBuf(LexBuffer<_>.LexemeString(lexbuf)); multiline_comment lexbuf}
  | "*/"
    { decr comment_depth; 
      if !comment_depth = 0 then 
        (COMMENT(SourceText.ofTuple(str_buf.ToString(), (!startPos, lexbuf.EndPos))), lexbuf.StartPos) 
      else 
        appendBuf(LexBuffer<_>.LexemeString(lexbuf)); multiline_comment lexbuf 
    }
  | eol {lexbuf.EndPos <- lexbuf.EndPos.NextLine; appendBuf(LexBuffer<_>.LexemeString(lexbuf)); multiline_comment lexbuf }
  | eof { failwith "unclosed comment in the end of file" }
  | [^ '\r' '\n' '*' '/']+ { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); multiline_comment lexbuf } 
  | _ { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); multiline_comment lexbuf } 
and literal = parse
  | "''" { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal lexbuf }
  | '\'' { LITERAL(SourceText.ofTuple(str_buf.ToString(),(!startPos,lexbuf.EndPos))), lexbuf.StartPos }
  | eol { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); lexbuf.EndPos <- lexbuf.EndPos.NextLine; literal lexbuf }
  | eof { failwith "unclosed literal in the end of file" }
  | [^ '\r' '\n' '\'']+  { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal lexbuf }
  | _ { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal lexbuf }
and literal2 = parse
  | "\"\"" { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal2 lexbuf }
  | '"' { LITERAL2(SourceText.ofTuple(str_buf.ToString(),(!startPos,lexbuf.EndPos))), lexbuf.StartPos }
  | eol { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); lexbuf.EndPos <- lexbuf.EndPos.NextLine; literal2 lexbuf }
  | eof { failwith "unclosed literal2 in the end of file" }
  | [^ '\r' '\n' '"']+   { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal2 lexbuf }
  | _   { appendBuf(LexBuffer<_>.LexemeString(lexbuf)); literal2 lexbuf }