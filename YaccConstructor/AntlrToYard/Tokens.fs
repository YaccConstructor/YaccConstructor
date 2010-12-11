module AntlrToYard.Tokens

type Lexeme = string * (int * int) 

type token = 
  | MULTILINE_COMMENT of Lexeme
  | SINGLELINE_COMMENT of Lexeme
  | SCOPE_NAME of Lexeme
  | ACTION_NAME of Lexeme
  | ACTION_CODE of Lexeme
  | EOF
  | T_GRAMMAR
  | T_OPTIONS
  | IDENTIFIER of Lexeme
  | LITERAL of Lexeme
  | LPAREN 
  | RPAREN
  | BAR
  | EQUAL
  | STAR
  | PLUS
  | COLON
  | SEMICOLON
  | QUESTION
  | TILDE
  | DOUBLE_DOT