let run inStr =
    let  buf = Lexing.LexBuffer<_>.FromString inStr 
    let tokens =
        seq { 
            while not buf.IsPastEndOfStream do
            let t = Lexer.token buf
            yield t
        }
        |> Yard.Generators.CYK.CodeTokenStream
        
    let cyk = new Yard.Generators.CYKGenerator.CYKCore()
    cyk.Recognize (new ResizeArray<_>(Yard.Generators.CYK.rules),Yard.Generators.CYK.StartNTerm) tokens (fun x y z -> 0uy)

let time () =    
    let start = System.DateTime.Now
    let res = 
        run( 
        "9+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2"
         + "+9+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2"
         + "+9+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2+2+8+9+2*2"
         )
    printfn "%s" res
    printfn "%A" (System.DateTime.Now - start)
    System.Console.ReadKey()
do 
    time ()