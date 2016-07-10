module CYKMatrix

open Util

let recognize (options: Options.T) = 
    match options.algorithm with
    | Okhotin -> CYKMatrixOkhotin.recognize options
    | Modified -> CYKMatrixBFS.recognize options




