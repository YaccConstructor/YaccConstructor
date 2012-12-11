module Yard.Utils.SourceText

type Trinity = {Id : int; Column : int; Line : int}

let private constId = 17
let private constColumn = 17
let private constLine = 64 - constId - constColumn

let Pack id column line = ((uint64 id) <<< constColumn + constLine) ||| 
                          ((uint64 column) <<< constLine) ||| 
                          (uint64 line)

let RePackId (trinity : uint64) = int (trinity >>> constColumn + constLine)
let RePackColumn (trinity : uint64) = int ((trinity <<< constId) >>> constId + constLine)
let RePackLine (trinity : uint64) = int ((trinity <<< constId + constColumn) >>> constId + constColumn)

let RePack (trinity : uint64) = 
    let id = RePackId trinity
    let column = RePackColumn trinity
    let line = RePackLine trinity
    let (res : Trinity) = {Id = id; Column = column; Line = line}
    res