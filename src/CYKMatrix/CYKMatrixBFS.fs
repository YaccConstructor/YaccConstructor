module CYKMatrixBFS

    open System.Collections.Generic

    type NonTerminal = NonTerminal of string

    type ComplexRule = { 
        Head: NonTerminal;
        LeftTail: NonTerminal;
        RightTail: NonTerminal; 
        probability: double;
    }

    type SimpleRule = {
        Head: NonTerminal;
        Tail: char;
        probability: double;
    } 

    type EpsRule =  {
        Head: NonTerminal;
        probability: double;
    } 

    type RulesHolder(complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * double) list>,
                     simpleRules: Dictionary<char, (NonTerminal * double) list>,
                     epsilonRules: NonTerminal list)  = 
        member this.GetSimpleRule c = simpleRules.Item c



    // todo: сделать красиво
    module SubMatrix = 

        // todo: private?
        let dotWithShift initial rowShift colShift =
            let initRow, initCol = initial
            (initRow + rowShift, initCol + colShift)

        type T = {Top: int * int; Size: int} with
            member this.HalfSize = int(float(this.Size) / 2.)
            member this.RelativeDot = dotWithShift this.Top

            member this.Right  = this.RelativeDot this.Size  0
            member this.Left   = this.RelativeDot 0 -this.Size
            member this.Bottom = this.RelativeDot this.Size -this.Size        

        let matrixWithShift matrixSize (initial: T) rowShift colShift =
            let newTop = dotWithShift initial.Top rowShift colShift
            {Top = newTop; Size = matrixSize}

        type T with
//            member this.HalfSizeMatrixWithShift = matrixWithShift this.HalfSize this
            member this.RelativeMatrix = matrixWithShift this.Size this

            member this.TopMatrix    = matrixWithShift this.HalfSize this 0  0 
            member this.RightMatrix  = matrixWithShift this.HalfSize this this.HalfSize  0
            member this.LeftMatrix   = matrixWithShift this.HalfSize this 0 -this.HalfSize
            member this.BottomMatrix = matrixWithShift this.HalfSize this this.HalfSize -this.HalfSize 
    
        let create top size = {Top = top; Size = size}
        let print matrix = 
            let row, col = matrix.Top
            printf " (top: %d, %d; size: %d) " row col matrix.Size



    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}
    let printTask task = 
        printf "where: "
        SubMatrix.print task.where
        printf "from1: "
        SubMatrix.print task.from1
        printf "from2: "
        SubMatrix.print task.from2
        printfn ""         



    // only for str size == 2^k-1
    // maxSearchLength ignored
    let recognize (strToParse: string) 
                  (complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * double) list>)
                  (simpleRules: Dictionary<char, (NonTerminal * double) list>)
                  (epsilonRules: NonTerminal list) 
                  (nonterminals : NonTerminal [])
                  S 
                  maxSearchLength = 

        //todo:
        let allRules = new RulesHolder(complexRules, simpleRules, epsilonRules)


        let stringSize = String.length strToParse

        let matrixSizeExponent = (System.Math.Log (double stringSize)) / (System.Math.Log 2.) |> System.Math.Ceiling |> int
        let matrixSize = (1 <<< matrixSizeExponent)

    

        let emptyMatrixOfSize n = Array2D.init n n (fun x y -> 0.)
    
        // bottom-left triangle and diagonal of tMatrixes and pMatrixes are not used
        let tMatrix = new Map<NonTerminal, double [,]>
                            (
                                nonterminals 
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )

        let pMatrix = new Map<NonTerminal * NonTerminal, double [,]>
                            (
                                complexRules.Keys
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )                                                        

        let addProbToMatrix (matrix: Map<_, double [,]>) row column nontermProb = 
            let key, prob = nontermProb
            (matrix.Item key).[row, column] <- (matrix.Item key).[row, column] + prob
                  
        // todo: wrapper?      
        let updateTMatrixCell row column nontermProbs = 
            nontermProbs |> List.iter (addProbToMatrix tMatrix row column)

        let addToP nts (matrix: double [,]) (where: SubMatrix.T) =
            for i in [0 .. where.Size - 1] do
//                let rightBound = min m2 (stringSize + 1)
                for j in [0 .. where.Size - 1] do
                    addProbToMatrix pMatrix (i + fst where.Left) (j + snd where.Left) (nts, matrix.[i, j])

        let subMatrixMult nt1 nt2 (from1: SubMatrix.T) (from2: SubMatrix.T) =
            let calcCell i j =
                [0..from1.Size-1] |> List.fold (fun acc k -> acc + tMatrix.[nt1].[i + fst from1.Left, k + snd from1.Left] * 
                                                                   tMatrix.[nt2].[k + fst from2.Left, j + snd from2.Left]) 0. 
//            let bUpperBound = min bm2 (stringSize + 1)
            Array2D.init from1.Size from2.Size calcCell    

        let performMultiplication task = 
            let {where=where; from1=from1; from2=from2} = task
            let completeOnePair (nt1, nt2) =
                    addToP (nt1, nt2) (subMatrixMult nt1 nt2 from1 from2) where
            pMatrix |> Map.iter (fun nts _ -> completeOnePair nts)
        
        let rec completeSubLayer layer matricesSize = 
            if matricesSize = 1 then
                let headsFromTail (tail, tailProb) = 
                    if complexRules.ContainsKey tail then 
                        complexRules.Item tail |> List.map (fun (head, headProb) -> head, headProb * tailProb)
                    else 
                        []

                let tails row col = pMatrix |> Map.map (fun _ probs -> probs.[row, col-1]) |> Map.filter (fun _ prob -> prob > 0.)
                let heads (row, col) = tails row col |> Map.toList |> List.map headsFromTail |> List.concat

                layer 
                |> Array.map (fun (matrix: SubMatrix.T) -> matrix.Top)
                |> Array.map (fun top -> top, heads top)
                |> Array.iter (fun (top, heads) -> heads |> updateTMatrixCell (fst top) (snd top - 1))

            else
                let halfMatricesSize = int(matricesSize / 2)
                let zeroSubLayer = layer |> Array.map (fun (matrix: SubMatrix.T) -> matrix.BottomMatrix)        
                completeSubLayer zeroSubLayer halfMatricesSize
                completeVLayer layer matricesSize

        and completeVLayer layer matricesSize = 
            let halfMatricesSize = int(matricesSize / 2)

            let firstSubLayerLeft = 
                layer 
                |> Array.map (fun matrix -> matrix.LeftMatrix)
            let firstSubLayerRight = 
                layer 
                |> Array.map (fun matrix -> matrix.RightMatrix)

            let toMultLeft = 
                firstSubLayerLeft 
                |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -(snd matrix.Top - fst matrix.Top - matricesSize), 
                                            matrix.RelativeMatrix halfMatricesSize 0)
            let toMultRight = 
                firstSubLayerRight 
                |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -halfMatricesSize, 
                                            matrix.RelativeMatrix (snd matrix.Top - fst matrix.Top - matricesSize) 0)

            let firstSubLayer = Array.append firstSubLayerLeft firstSubLayerRight
            let toMult = Array.append toMultLeft toMultRight
                         
            let firstMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2})
                                            firstSubLayer 
                                            toMult
                                            
            firstMultTasks |> Array.iter performMultiplication
            completeSubLayer firstSubLayer halfMatricesSize
                      
            let secondSubLayer = 
                layer 
                |> Array.map (fun matrix -> matrix.TopMatrix)
            let secondToMult = 
                secondSubLayer 
                |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -(snd matrix.Top - fst matrix.Top - matricesSize), 
                                            matrix.RelativeMatrix halfMatricesSize 0) 
            let thirdToMult = 
                secondSubLayer 
                |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -halfMatricesSize, 
                                            matrix.RelativeMatrix (snd matrix.Top - fst matrix.Top - matricesSize) 0)
            let secondMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2}) secondSubLayer secondToMult 
            let thirdMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2}) secondSubLayer thirdToMult 
                        
            secondMultTasks |> Array.iter performMultiplication
            thirdMultTasks  |> Array.iter performMultiplication          
            completeSubLayer secondSubLayer halfMatricesSize


        and completeLayer layerNum = 
            let matricesSize = 1 <<< (layerNum - 1)
            let halfMatricesSize = int(matricesSize / 2)

            if matricesSize = 1 
            then
                let nonTermsForChars = strToParse |> List.ofSeq |> List.map allRules.GetSimpleRule
                nonTermsForChars |> List.iteri (fun i p -> updateTMatrixCell i (i + 1) p)
            else
                let matricesCount = (1 <<< (matrixSizeExponent - layerNum + 1)) - 1
                let firstInLayer = SubMatrix.create (0, 1 <<< layerNum) matricesSize
                let layer = Array.init matricesCount (fun i -> firstInLayer.RelativeMatrix (i * matricesSize) (i * matricesSize))
                completeVLayer layer matricesSize


        for i in 1..matrixSizeExponent do
            completeLayer i
            
        tMatrix.Item S



    [<EntryPoint>]
    let main args = 
        let A = NonTerminal "A"
        let B = NonTerminal "B"
        let BB = NonTerminal "BB"
        let S = NonTerminal "S"
        let nonterminals = [|A; B; S; BB|]


        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * double) list>()
        [(A, BB), [S, 1.]; (B, B), [BB, 1.; B, 0.2]; (A, A), [A, 0.8]] |> Seq.iter crl.Add
        let srl = new Dictionary<char, (NonTerminal * double) list>()
        ['a', [(A, 0.2)]; 'b', [B, 0.4]; 'c', [B, 0.4]] |> Seq.iter srl.Add
        let erl: NonTerminal list = []

    //    S -> A BB, 1.
    //
    //    BB -> B B, 1.
    //
    //    A -> A A, 0.8
    //    A -> 'a', 0.2
    //
    //    B -> B B, 0.2
    //    B -> 'b', 0.4
    //    B -> 'b', 0.4

        let printMatrix (matrix: double [,]) strLen searchLen =
            let rowLength = matrix.GetLength(0)
            let colLength = matrix.GetLength(1)

            for i in [0..rowLength-1] do
                for j in [0..colLength-1] do
                    if i <= strLen && j <= strLen && j > i && j-i <= searchLen then
                        printf "%.8f  " matrix.[i, j]
                    else
                        assert (matrix.[i, j] = 0.)
                        printf "----------  "
                printfn ""
            printfn ""

        let isAnswerValid (matrix: double [,]) strLen searchLen = 
            let rowLength = matrix.GetLength(0)
            let colLength = matrix.GetLength(1)
            if rowLength <> colLength || rowLength <> strLen + 1 then
                false
            else
                let notRealCell (i, j) =
                        i > strLen 
                        || j > strLen 
                        || j <= i 
                        || j-i > searchLen 

                [1..rowLength-1]
                |> List.map (fun i -> [1..colLength-1] |> List.map (fun j -> (i,j))) 
                |> List.concat
                |> List.filter notRealCell
                |> List.forall (fun (i, j) -> matrix.[i,j] = 0.)

        let check str = 
            let searchLen = String.length str
            let toCheck = recognize str crl srl erl nonterminals S searchLen
            assert (isAnswerValid toCheck (String.length str) searchLen)
            printMatrix toCheck (String.length str) searchLen
            
        check "abb"      
        check "aaabbcc"

        System.Console.ReadLine() |> ignore
        0