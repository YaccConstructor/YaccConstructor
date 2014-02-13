let series1 = [1;2;5;10]
let genAtom i = [for j in 1..i -> "x" + string j]
let genSeries m n = Array.init n (fun _ -> genAtom m)
let graphBody arr =
    arr 
    |> Array.mapi (fun i a -> [|for s in a -> sprintf "%A -> %A [label=%A]" (i+1) (i+2) s|])
    |> Array.concat
    |> String.concat ";\n"

let printGraph name m n gb =
    let s = 
      "digraph G {\n"
      + ([for i in 0..n+2 -> string i] |> String.concat ";")
      + ";\n"
      + sprintf "%A -> %A [label=\"'\"]" 0 1
      + ";\n"     
      + gb
      + ";\n"
      + sprintf "%A -> %A [label=\"'\"]" (n+1) (n+2)
      + ";\n}"

    System.IO.File.WriteAllText( name(), s)
(*
for m in series1 do
   for n in 1..100 do
     graphBody (genSeries (fun () -> sprintf "%A/%A.dot" m n) m n)
     |> printGraph m n
*)

let gen n =
    let pe l i j = sprintf "%i -> %i [label=%A]" i j l
    let branch x y = 
        [pe "x" x y
         pe "y" x y]
    let rec go n i  =
        if n = 0 
        then (i+1,branch i (i+1))
        else
             let e1 = pe "x" i (i+1)
             let j,es1 = go (n-1) (i+1)             
             let e2 = pe "y" i (j+1)
             let i,es2 = go (n-1) (j+1) 
             let e3 = pe "x" j (i+1)
             let e4 = pe "y" i (i+1)
             (i+1),([e1]@[e2]@[e3]@[e4]@es1@es2)
    let i,s = go n 1
    printGraph (fun () -> sprintf "2n/%A.dot" n) n (i-1) (String.concat ";\n" s)

for i in 0..10 do gen i     