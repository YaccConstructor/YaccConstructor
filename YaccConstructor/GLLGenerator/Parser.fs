// Parser.fs contains logic for maintaining parse states set

module Yard.Generators.GLL.Parser

type ParseFunction = unit -> bool

/// <summary>
/// Function representation for descriptor:
/// tag: function tag (index of this parse function in array)
/// ind: input buffer index at the moment of creating this descriptor
/// </summary>
type ParseFunctionDescr = int * int

/// <summary>
/// Elementary descriptor consists of the following:
/// L: continuation function tag (we implement goto as continuation tail call)
/// s: parse stack
/// j: position in the input array
/// </summary>
type ElementaryDescriptor = int * ParseFunctionDescr list * int

// states set R
let mutable parseStates : ElementaryDescriptor list = []
let mutable pos = 0
let mutable stack : ParseFunctionDescr list = []

let mutable _parseFunctions : ParseFunction[] = [||]
let mutable _tokens : int[] = [||]

let init tokens parseFunctions = 
    _tokens <- tokens
    _parseFunctions <- parseFunctions
    parseStates <- []
    pos <- 0
    stack <- [(0,0)]

// L_0 function, it has tag 0
let continueExecution () =
    if not parseStates.IsEmpty
    then
        let tag, newStack, newPos = parseStates.Head
        parseStates <- parseStates.Tail
        if tag = 0 && newStack.IsEmpty && newPos = _tokens.Length - 1
        then true
        else
            stack <- newStack
            pos <- newPos
            _parseFunctions.[tag] ()            
    else false

// adds new elementary descriptor to set R, if it is not present
let addDescriptor functionTag =
    let descriptor = (functionTag, stack, pos)
    if not <| List.exists  ((=) descriptor) parseStates
    then parseStates <- (descriptor::parseStates)

// pushes function descriptor to stack
let push functionDescr = stack <- functionDescr::stack

// pops top function descriptor from stack and adds resulting parse state to R
let pop () =
    let (stackTopTag, _) = stack.Head
    stack <- stack.Tail
    addDescriptor stackTopTag