module ControlFlowGraph.Common

[<RequireQualifiedAccess>]
type Keyword = 
    | IF
    | THEN 
    | ELSE
    | ENDIF
    | EQ
    | SEMICOLON
    | WHILE

type BlockType = 
    | Assignment 
    | Condition
    | Declaration 
    | Definition
    | IfStatement 
    | ThenStatement 
    | ElseStatement 
    | ForStatement 
    | WhileStatement
    | Entry
    | NoneBlock 

    static member BlockTypeToString block = 
        match block with
        | Assignment -> "Assignment"
        | Condition -> "Condition"
        | Declaration -> "Declaration"
        | Definition -> "Definition"
        | IfStatement -> "IfStatement"
        | ThenStatement -> "ThenStatement"
        | ElseStatement -> "ElseStatement"
        | ForStatement -> "ForStatement"
        | WhileStatement -> "WhileStatement"
        | Entry -> "Entry"
        | NoneBlock -> "None"

    static member AreEquals (one : BlockType) (two : BlockType) = 
        match one, two with
        | Assignment, Assignment
        | Condition, Condition 
        | Declaration, Declaration 
        | Definition, Definition 
        | IfStatement, IfStatement 
        | ThenStatement, ThenStatement
        | ElseStatement, ElseStatement 
        | ForStatement, ForStatement
        | WhileStatement, WhileStatement
        | Entry, Entry
        | NoneBlock, NoneBlock -> true
        | _ -> false