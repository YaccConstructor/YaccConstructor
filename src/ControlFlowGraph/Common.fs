module ControlFlowGraph.Common

[<RequireQualifiedAccess>]
type Keyword = 
    | IF
    | THEN 
    | ELSE
    | ENDIF
    | EQ
    | ASSIGN
    | SEMICOLON
    | WHILE

type BlockType = 
    | Assignment 
    | Condition
    | Declaration 
    | Definition
    | Expression
    | IfStatement 
    | Identificator
    | ThenStatement 
    | ElseStatement 
    | ForStatement 
    | WhileStatement
    | Entry
    | NoneBlock 

    override this.ToString()= 
        match this with
        | Assignment -> "Assignment"
        | Condition -> "Condition"
        | Declaration -> "Declaration"
        | Definition -> "Definition"
        | Expression -> "Expression"
        | Identificator -> "Identificator"
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
        | Expression, Expression
        | Identificator, Identificator 
        | IfStatement, IfStatement 
        | ThenStatement, ThenStatement
        | ElseStatement, ElseStatement 
        | ForStatement, ForStatement
        | WhileStatement, WhileStatement
        | Entry, Entry
        | NoneBlock, NoneBlock -> true
        | _ -> false