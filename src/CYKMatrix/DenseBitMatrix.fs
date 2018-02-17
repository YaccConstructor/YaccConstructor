module DenseBitMatrix

open MatrixKernels
open SparseGraphParsingImpl

let getIntRepresentation (x:System.Collections.BitArray) = 
    let fInfo = typeof<System.Collections.BitArray>.GetField("m_array", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    fInfo.GetValue(x) :?> int[] 

let countNNZ (a:System.Collections.BitArray) =
    let numberOfSetBits (i:int) =
         let i = i - ((i >>> 1) &&& 0x55555555)
         let i = (i &&& 0x33333333) + ((i >>> 2) &&& 0x33333333)
         (((i + (i >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24
    getIntRepresentation a
    |> Array.sumBy numberOfSetBits

let pos = [|0..512*512-1|]

let multiply (a:System.Collections.BitArray) (b:System.Collections.BitArray) =
    let n = a.Length |> float |> sqrt |> int
    
    let inversedB =
        let res = new System.Collections.BitArray(b.Length)
        for i in 0 .. b.Length - 1 do
            res.[i] <- b.[(i % n) * n + i / n]
        res
    let aInts = getIntRepresentation a
    let bInts = getIntRepresentation inversedB
    let res = new System.Collections.BitArray(b.Length)
    //for i in 0 .. n - 1 do
    pos
    |> Array.iter (fun k ->
        let i = k / 512
        let j = k % 512
        //for j in 0 .. n - 1 do
        //let mutable v = false
        //let mutable k = 0
        //while (not v) && k < n / 32 do 
            //printfn "%A %A" (aInts.[i * (n / 32) + k]) (bInts.[j * (n / 32) + k])
        let h = i * (n / 32)
        let g = j * (n / 32)
        res.[i*n + j] <- 
                (aInts.[h + 0] &&& bInts.[g + 0]) 
            ||| (aInts.[h + 1] &&& bInts.[g + 1]) 
            ||| (aInts.[h + 2] &&& bInts.[g + 2]) 
            ||| (aInts.[h + 3] &&& bInts.[g + 3]) 
            ||| (aInts.[h + 4] &&& bInts.[g + 4]) 
            ||| (aInts.[h + 5] &&& bInts.[g + 5]) 
            ||| (aInts.[h + 6] &&& bInts.[g + 6]) 
            ||| (aInts.[h + 7] &&& bInts.[g + 7]) 
            ||| (aInts.[h + 8] &&& bInts.[g + 8]) 
            ||| (aInts.[h + 9] &&& bInts.[g + 9]) 
            ||| (aInts.[h + 10] &&& bInts.[g + 10]) 
            ||| (aInts.[h + 11] &&& bInts.[g + 11]) 
            ||| (aInts.[h + 12] &&& bInts.[g + 12]) 
            ||| (aInts.[h + 13] &&& bInts.[g + 13]) 
            ||| (aInts.[h + 14] &&& bInts.[g + 14]) 
            ||| (aInts.[h + 15] &&& bInts.[g + 15]) <> 0
            //k <- k + 1
        //lock pos (fun () -> res.[i*n + j] <- v))
        //res.[i*n + j] <- v)
        )
    //printfn "NNZ A: %A B: %A C: %A" (countNNZ a) (countNNZ b) (countNNZ res)
    res

let add (a:System.Collections.BitArray) (b:System.Collections.BitArray) =
    let aInts = getIntRepresentation a
    getIntRepresentation b
    |> Array.iteri (fun i v -> aInts.[i] <- aInts.[i] ||| v)

type DenseBitHandler(_matrixSize:int) =       
    interface IMatrixHandler<System.Collections.BitArray, bool> with
        member this.matrixSize = _matrixSize
        member this.createEmptyMatrix size = new System.Collections.BitArray(_matrixSize*_matrixSize)
        member this.ParsingMatrixInitializator graph allRules nonterminals = 
            initParsingMatrix graph allRules nonterminals (fun n -> new System.Collections.BitArray(n*n)) (fun a i j v -> a.[i * _matrixSize + j] <-v) true
        member this.Multiply matrix1 matrix2 = multiply matrix1 matrix2
        member this.Add matrix1 matrix2 = 
            add matrix1 matrix2
            matrix1
        member this.getNonZerosCount a = countNNZ a

    