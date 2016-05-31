module Yard.Generators.RNGLR.ReadBack.Compressor

let pairToOne (first : int) (second : int) =
    let first, second = (int64 first) <<< 32, int64 second
    first ||| second

(*let oneToPair (one : int64) =
    let first, second = one >>> 32, one &&& ((1 <<< 32) - 1)
    first, second*)


