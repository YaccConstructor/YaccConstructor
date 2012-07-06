//CYK for research :) 
// It is part of YaccConstructor.

//Правила контекстно-свободной грамматики в нормальной форме Хомского
type Rule = 
   |ToBranch of string*string*string*Option<string> //А->BC, A,B,C - нетерминалы
   |ToLeaf of string*char*Option<string> //A->a, а - терминал 

//Вывод правил
let printRules =
   (function
   | ToBranch(a,b,c,_),l -> System.Console.WriteLine(String.concat "" [a; "->"; b; c; "  "; (match l with | Some t -> t | None -> "")])
   | ToLeaf(a,b,_),l     -> System.Console.WriteLine(String.concat "" [a;"->";b.ToString();"  "; (match l with | Some t -> t | None -> "") ]))
   |> List.iter

//Последовательное применение правил rs, начиная с первого левого нетерминала start                             
let printOutput rs start =
   //последовательно строит сентенциальные формы вывода
   let rec sentForm current rules =
      match rules with
      |x::xs -> //первый левый нетерминал
                let leftNonTerminal = List.find (fun (y:string) -> (y = (y.ToUpper()))&&((y.ToUpper()) <> (y.ToLower()))) current
                //применение правила к левому нетерминалу - новая сентенциальнвя форма
                let rec newForm oldForm = 
                   match oldForm with
                   |l::ls -> if l = leftNonTerminal then //ищем первый нетерминал
                                match x with //замена l в соответствии с правилом x
                                |ToBranch(a,b,c,l) -> b::c::ls
                                |ToLeaf(a,b,l)     -> b.ToString()::ls                                
                             else l::(newForm ls)
                   |_     -> []
                System.Console.Write(String.concat "" ("->"::(newForm current)))                   
                sentForm (newForm current) xs
      |[]    -> ()          
   System.Console.Write("\n"+start)
   sentForm [start] rs
   System.Console.WriteLine("\n\n")
                                                                       
let recognitionTable (rules,_) (s:string) = 
           
   //let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length ([],(0,0),(0,0),None,[])
   let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length ([],(0,0),(0,0),[],[])
   //recTable(i,l), для l > 0

   let processRule rule i k l =
        match rule with
        |ToBranch(a,b,c,rule_lbl) ->
            let nonTerminals1,_,_,lbl1,_ = recTable.[i,k]
            let nonTerminals2,_,_,lbl2,_ = recTable.[k+i+1,l-k-1]
            let nonTerminals,_,_,lbl,rules = recTable.[i,l]
            if (List.exists ((=)b) nonTerminals1) && (List.exists ((=)c) nonTerminals2)
            then 
    (*            let new_lbl =
                    match lbl1,lbl2 with
                    | Some v1, Some v2 when v1<>v2 -> Some("conflict")
                    | None, None -> Some("None")
                    //| _ -> List.find (Option.isSome) [lbl1.Head;lbl2.Head]
                    | _ -> Some("one of") 
             
                recTable.[i,l] <- (a::nonTerminals),(i,k),(k+i+1,l-k-1),new_lbl,(rule::rules)
      *)      
                let new_lbl =
                    match lbl1.Head,lbl2.Head with
                    | Some v1, Some v2 when v1<>v2 -> Some("conflict")
                    | None, None -> rule_lbl
                    | _ -> List.find (Option.isSome) [lbl1.Head;lbl2.Head] 
             
                recTable.[i,l] <- (a::nonTerminals),(i,k),(k+i+1,l-k-1),(new_lbl::lbl),(rule::rules)
            
        |_               -> ()   

   let elem i l = rules |>  Array.iter (fun rule -> for k in 0..(l-1) do processRule rule i k l)

   //Заполнение RecognitionTable
   let rec fillTable i l =
        if l = s.Length-1
        then elem i l//последний элемент таблицы
        elif i+l <= s.Length-1
        then
             elem i l
             fillTable (i+1) l//продолжаем заполнять столбец
        else
             fillTable 0 (l+1)//переход на новый столбец

   //первый столбец таблицы для правил, выводящих терминал
   for rule in rules do
      for k in 0..(s.Length-1) do
         match rule with
         |ToLeaf(a,b,l) -> if b = s.[k] then
                            let nonTerminals,_,_,lbl,rules = recTable.[k,0]
                            //recTable.[k,0] <- ((a::nonTerminals),(-1,-1),(-1,-1),l,(rule::rules))
                            recTable.[k,0] <- ((a::nonTerminals),(-1,-1),(-1,-1),(l::lbl),(rule::rules))
         |_           -> ()
   //последующие столбцы
   fillTable 0 1
   recTable

//вывод строки s в грамматике g
let recognize ((_, start) as g) s =
   let recTable = recognitionTable g s
   //восстановление вывода по таблице начиная с ячейки recTable(i,l) нетерминала top
   let rec subRecognize i l top = 
      let nonTerminals,(leftI,leftL),(rightI,rightL),lbls,rs = recTable.[i,l]
      //правило вида А->_, где А = top
      let currentRuleIndex = List.findIndex (fun (ToBranch(st,_,_,l)|ToLeaf(st,_,l)) -> st = top) rs
      let currentRule = rs.[currentRuleIndex]
      let lbl = lbls.[currentRuleIndex]
      let leftNT,rightNT = 
         match currentRule with
         |ToBranch(_,left,right,l) -> left,right
         |ToLeaf(_,terminal,l)     -> terminal.ToString(),""  
      match i,l with
      | _,0 -> [(ToLeaf(top,leftNT.[0],None),lbl)]
      | _     -> (currentRule,lbl)::(subRecognize leftI leftL leftNT)@(subRecognize rightI rightL rightNT)
   let resultRules = if List.exists ((=)start) ((fun (a,_,_,_,l) -> a) recTable.[0, s.Length-1])
                     then subRecognize 0 (s.Length-1) start//если цепочка принадлежит языку L(g)
                     else []

   System.Console.WriteLine s
   System.Console.WriteLine "Rules:"
   printRules resultRules
   //printOutput resultRules start    


//грамматика, допускающая операции +, * и позволяющая группировать их с помощью скобок, преобразованная в НФХ
//let testGrammar1 = (Array.ofList <| (List.map ToBranch [("E","T","N1");("N1","N2","E");("E","F","N3");("N3","N4","T");("E","N5","N6");("N6","E","N7");("E1","N2","E");("T","F","N3");("T","N5","N6");("T1","N4","T");("F","N5","N6")]) @ (List.map ToLeaf [("N2",'+');("N4",'*');("E",'a');("N5",'(');("N7",')');("T",'a');("F",'a')]),"E")
//let testGrammar2 = ((List.map ToBranch [("S","A","B");("B","N1","N2");("N1","D1","D2")])@(List.map ToLeaf [("A",'a');("D2",'c');("B",'a');("D1",'b');("N2",'d')]),"S")
//System.Console.WriteLine("Test1:")
//recognize testGrammar1 "a*a+a"

let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")


let str = List.init 20 (fun i -> "(a+a)*a") |> String.concat "+"
System.Console.WriteLine("Test2:")
let start = System.DateTime.Now
//recognize testGrammar1 str
printfn "Time = %A" (System.DateTime.Now - start)

recognize testGrammar_lbl1 "a*a*a+"

//System.Console.WriteLine("Test3:")
//recognize testGrammar2 "abcd"
System.Console.ReadLine()   
