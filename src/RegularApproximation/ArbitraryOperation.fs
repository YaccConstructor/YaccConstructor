/// Types representing functions and methods regular 
/// approximation algorithm can be applied to
module ArbitraryOperation

/// Represents any function or method
type ArbitraryOperationInfo<'Info> = {
    Name: string
    Info: 'Info }

/// Holds the language dependent data necessary to build the 
/// approximation of method or function
type ArbitraryOperation<'Info> = option<ArbitraryOperationInfo<'Info>>