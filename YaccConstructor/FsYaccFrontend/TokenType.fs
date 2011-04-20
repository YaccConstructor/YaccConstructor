module Yard.Frontends.FsYaccFrontend.TokenType

type t =
    | HEAD of string
    | DOUBLE_PERC
    | TOKEN_KW
    | ASSOC_KW
    | START_KW
    | TYPE_KW
    | TYPE of string
    | UIDENT of string
    | LIDENT of string
    | COLON
    | BAR
    | ACTION_CODE of string
    | EOF