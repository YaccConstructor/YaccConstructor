// Signature file for parser generated by fsyacc
#light "off"
module Yard.Frontends.YardFrontend.GrammarParser
open Yard.Core
type token = 
  | SHARPLINE of (string)
  | PATTERN of (IL.Source.t)
  | PARAM of (IL.Source.t)
  | PREDICATE of (IL.Source.t)
  | ACTION of (IL.Source.t)
  | STRING of (IL.Source.t)
  | LIDENT of (IL.Source.t)
  | UIDENT of (IL.Source.t)
  | VALUE of (string)
  | SET
  | INCLUDE
  | COMMUT
  | DLESS
  | DGREAT
  | RPAREN of (Range)
  | LPAREN of (Range)
  | EXCLAMATION
  | QUESTION
  | MINUS
  | PLUS
  | STAR
  | BAR
  | EQUAL
  | COMMA
  | SEMICOLON
  | COLON
  | EOF
type tokenId = 
    | TOKEN_SHARPLINE
    | TOKEN_PATTERN
    | TOKEN_PARAM
    | TOKEN_PREDICATE
    | TOKEN_ACTION
    | TOKEN_STRING
    | TOKEN_LIDENT
    | TOKEN_UIDENT
    | TOKEN_VALUE
    | TOKEN_SET
    | TOKEN_INCLUDE
    | TOKEN_COMMUT
    | TOKEN_DLESS
    | TOKEN_DGREAT
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_EXCLAMATION
    | TOKEN_QUESTION
    | TOKEN_MINUS
    | TOKEN_PLUS
    | TOKEN_STAR
    | TOKEN_BAR
    | TOKEN_EQUAL
    | TOKEN_COMMA
    | TOKEN_SEMICOLON
    | TOKEN_COLON
    | TOKEN_EOF
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startfile
    | NONTERM_file
    | NONTERM_action_opt
    | NONTERM_rule_nlist
    | NONTERM_rule
    | NONTERM_plus_opt
    | NONTERM_formal_meta_param_opt
    | NONTERM_formal_meta_list
    | NONTERM_param_opt
    | NONTERM_alts
    | NONTERM_bar_seq_nlist
    | NONTERM_seq
    | NONTERM_seq_elem_list
    | NONTERM_seq_elem
    | NONTERM_omit_opt
    | NONTERM_predicate_opt
    | NONTERM_bound
    | NONTERM_patt
    | NONTERM_prim
    | NONTERM_meta_param
    | NONTERM_meta_params
    | NONTERM_meta_param_opt
    | NONTERM_call
    | NONTERM_option_opt
    | NONTERM_option_params
    | NONTERM_option_param
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val file : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ((IL.Source.t, IL.Source.t)IL.Definition.t) 
