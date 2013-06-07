let mutable path = None
let commandLineSpecs =
    ["-i", ArgType.String (fun s ->
                                path <- Some s ), "Input text"         
    ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))

let run input =
    let  buf = Lexing.LexBuffer<_>.FromTextReader input
    let ts =  
        seq { 
                while not buf.IsPastEndOfStream do
                let t = Lexer.tokens buf
                yield t
            }
        |> Array.ofSeq

    let tokens =
        ts
        |> Yard.Generators.CYK.CodeTokenStream
        
    let cyk = new Yard.Generators.CYKGenerator.CYKCore()
    cyk.Recognize (Yard.Generators.CYK.rules, Yard.Generators.CYK.StartNTerm) tokens (fun x y z -> 0uy) Yard.Generators.CYK.lblName

let time () =    
    let start = System.DateTime.Now
    ArgParser.Parse commandLineSpecs
    match path with
    | Some v ->
        let res = run(new System.IO.StreamReader(v))
        printfn "%s" res
        printfn "%A" (System.DateTime.Now - start)
    | _ ->
        printfn "%s" "Не задано имя входного файла."
    
do 
    time ()    