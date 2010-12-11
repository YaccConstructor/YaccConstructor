module AntlrToYard.Tokens

type token = 
  | MULTILINE_COMMENT
  | SINGLELINE_COMMENT
  | CHAR
  | EOF