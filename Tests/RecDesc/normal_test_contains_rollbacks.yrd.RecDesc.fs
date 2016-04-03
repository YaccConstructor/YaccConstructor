module RecursiveDescentParsers
open System.Collections.Generic

let rulesDict = new Dictionary<string, List<string * string> >()
let rollback = new List<string * int * int>()

let fst (x, _, _) = x
let snd (_, y, _) = y
let third (_, _, z) = z

let word = new List<string>()
let functions = new Dictionary<_, _>()
let terminals = Set.ofList ["EOF";"A";"B";"D";"C"]
let nonTerminals = Set.ofList ["a";"b";"c";"d";"e";"f";"g"]


rulesDict.Add("a", new List<string * string>())
rulesDict.["a"].Add(("b"), ("c"))
rulesDict.Add("b", new List<string * string>())
rulesDict.["b"].Add(("A"), ("$"))
rulesDict.["b"].Add(("d"), ("e"))
rulesDict.Add("d", new List<string * string>())
rulesDict.["d"].Add(("A"), ("$"))
rulesDict.Add("e", new List<string * string>())
rulesDict.["e"].Add(("B"), ("$"))
rulesDict.Add("c", new List<string * string>())
rulesDict.["c"].Add(("D"), ("$"))
rulesDict.["c"].Add(("f"), ("g"))
rulesDict.Add("f", new List<string * string>())
rulesDict.["f"].Add(("C"), ("$"))
rulesDict.Add("g", new List<string * string>())
rulesDict.["g"].Add(("D"), ("$"))


let a index = 
   let list = rulesDict.["a"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "a" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("a", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("a", !ind, i))
                           toBreak := true
   !ind
functions.["a"] <- a


let b index = 
   let list = rulesDict.["b"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "b" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("b", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("b", !ind, i))
                           toBreak := true
   !ind
functions.["b"] <- b


let c index = 
   let list = rulesDict.["c"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "c" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("c", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("c", !ind, i))
                           toBreak := true
   !ind
functions.["c"] <- c


let d index = 
   let list = rulesDict.["d"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "d" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("d", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("d", !ind, i))
                           toBreak := true
   !ind
functions.["d"] <- d


let e index = 
   let list = rulesDict.["e"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "e" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("e", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("e", !ind, i))
                           toBreak := true
   !ind
functions.["e"] <- e


let f index = 
   let list = rulesDict.["f"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "f" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("f", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("f", !ind, i))
                           toBreak := true
   !ind
functions.["f"] <- f


let g index = 
   let list = rulesDict.["g"]
   let toBreak = ref false
   let ruleNum = ref -1
   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = "g" then
       ruleNum := third(rollback.[rollback.Count - 1])
       index := snd(rollback.[rollback.Count - 1])
       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore
   let ind = ref 0
   ind := !index
   for i in (max !ruleNum 0) .. list.Count - 1 do
       let B, C = list.[i]
       if not (!toBreak) then
           ind := !index
           if C = "$" then
               if !ruleNum = -1 then
                   if !ind < word.Count && word.[!ind] = B then
                       if i <> list.Count - 1 then
                           rollback.Add(("g", !ind, i))
                       ind := !ind + 1
                       toBreak := true
                   else
                       ind := -1
           else
               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then
                   ind := snd(rollback.[rollback.Count - 1])
                   let fC = functions.[C]
                   let C_ind = ref(fC ind)
                   if !C_ind <> -1 then
                       toBreak := true
               if not (!toBreak) then
                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then
                       ind := snd(rollback.[rollback.Count - 1])
                   let fB = functions.[B]
                   let B_ind = ref(fB ind)
                   if !B_ind = -1 then
                       ind := -1
                   else
                       let fC = functions.[C]
                       let C_ind = ref(fC B_ind)
                       if !C_ind = -1 then
                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do
                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))
                               B_ind := fB new_ind
                               if !B_ind <> -1 then
                                   C_ind := fC B_ind
                       ind := !C_ind
                       if !C_ind <> -1 then
                           if i <> list.Count - 1 then
                               rollback.Add(("g", !ind, i))
                           toBreak := true
   !ind
functions.["g"] <- g


let start argv = 
   let i = ref 0
   word.AddRange(argv)
   let index = a i
   if index <> -1 then
       eprintfn "true"
   else
       eprintfn "false"
