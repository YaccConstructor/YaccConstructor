//CYK for research :) 
// It is part of YaccConstructor.
namespace CYK

//Правила контекстно-свободной грамматики в нормальной форме Хомского
type Rule = 
   |ToBranch of string*string*string*Option<string>*Option<int> //А->BC<lbl>, A,B,C - нетерминалы
   |ToLeaf of string*char*Option<string> //A->a<lbl>, а - терминал 


type CYKParser()=
    //Вывод правил
    let printRules =
       (function
       | ToBranch(a,b,c,_,_),l -> [a; "->"; b; c; "  "; (match l with | Some t -> t | None -> "")]
       | ToLeaf(a,b,_),l     -> [a;"->";b.ToString();"  "; (match l with | Some t -> t | None -> "") ]
       >> String.concat "")
       |> List.map
       >> String.concat "\n"

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
                                    |ToBranch(a,b,c,l,_) -> b::c::ls
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
           
       let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length ([],(0,0),(0,0),[],[],[])
       //recTable(i,l), для l > 0

       let newLbl ruleLbl (lbls1:List<Option<string>>) (lbls2:List<Option<string>>) : Option<string> =
             match lbls1.Head,lbls2.Head with
             | Some v1, Some v2 when v1<>v2 -> Some("conflict")
             | None, None -> ruleLbl
             | _ -> List.find (Option.isSome) [lbls1.Head;lbls2.Head] 

       let newWeight ruleWeight (weights1:List<Option<int>>) (weights2:List<Option<int>>) : Option<int> = 
             match (weights1.IsEmpty,weights2.IsEmpty) with
             |true,true -> ruleWeight 
             |_ ->    
                 match weights1.Head,weights2.Head with
                 |Some w1, Some w2 -> Some(w1 + w2)
                 |None, None -> ruleWeight
                 |_ -> List.find (Option.isSome) [weights1.Head;weights2.Head] 

       let processRule rule i k l =
            match rule with
            |ToBranch(a,b,c,ruleLbl,ruleWeight) ->
                let nonTerminals1,_,_,lbls1,weights1,_ = recTable.[i,k]
                let nonTerminals2,_,_,lbls2,weights2,_ = recTable.[k+i+1,l-k-1]
                let nonTerminals,_,_,lbls,weights,rules = recTable.[i,l]
                if (List.exists ((=)b) nonTerminals1) && (List.exists ((=)c) nonTerminals2)
                then
                    recTable.[i,l] <- (a::nonTerminals),(i,k),(k+i+1,l-k-1),(newLbl ruleLbl lbls1 lbls2 ::lbls),(newWeight ruleWeight weights1 weights2 ::weights),(rule::rules)    
            |_               -> ()   

       let elem i l = rules |> Array.iter (fun rule -> for k in 0..(l-1) do processRule rule i k l)
    
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

       let printTableRules =
            (function
            | ToBranch(a,b,c,l,w) -> [a; "->"; b; c; "  "; (match l with | Some t -> t | None -> "--")]
            | ToLeaf(a,b,l)    -> [a;"->";b.ToString();"  "; (match l with | Some t -> t | None -> "--") ]
            >> String.concat "")
            |> List.map
            >> String.concat "; "

       let printTableLabels = 
            (function
            | Some v -> v
            | None -> "--")
            |> List.map
            >>String.concat "; "

//       let printTableWeights (weights:List<Option<int>>) : string = 
//            (function
//            | Some v -> "" + v.tostring()
//            | None -> "0"
//            >> String.concat "")
//            |> List.map
//            >> String.concat "; "
            

       let printElem i l =
            let _,_,_,lbls,weights,rules = recTable.[i,l]
            let outputElem = "" + i.ToString() + ") " + "lbls: " + printTableLabels lbls + ";; weights: " + weights.ToString() + ";; rules: " + printTableRules rules 
            System.Console.WriteLine (outputElem + "\n")

       let rec printTable i l =
            if l = s.Length-1
            then 
                 System.Console.WriteLine ("")
                 printElem i l//последний элемент таблицы
            elif i+l <= s.Length-1
            then
                 System.Console.WriteLine ("")
                 printElem i l
                 printTable (i+1) l//продолжаем заполнять столбец
            else 
                 System.Console.WriteLine ("row " + (l+1).ToString())
                 printTable 0 (l+1)//переход на новый столбец

       //первый столбец таблицы для правил, выводящих терминал
       for rule in rules do
          for k in 0..(s.Length-1) do
             match rule with
             |ToLeaf(a,b,l) -> if b = s.[k] then
                                let nonTerminals,_,_,lbl,weights,rules = recTable.[k,0]
                                recTable.[k,0] <- ((a::nonTerminals),(-1,-1),(-1,-1),(l::lbl),weights,(rule::rules))
             |_           -> ()
       //последующие столбцы
       fillTable 0 1

       printTable 0 0

       recTable

    //вывод строки s в грамматике g
    let recognize ((_, start) as g) s =
       let recTable = recognitionTable g s
       //восстановление вывода по таблице начиная с ячейки recTable(i,l) нетерминала top
       let rec subRecognize i l top = 
          let nonTerminals,(leftI,leftL),(rightI,rightL),lbls,ws,rs = recTable.[i,l]
          //правило вида А->_, где А = top
          let currentRuleIndex = List.findIndex (fun (ToBranch(st,_,_,l,_)|ToLeaf(st,_,l)) -> st = top) rs
          let currentRule = rs.[currentRuleIndex]
          let lbl = lbls.[currentRuleIndex]
          let leftNT,rightNT = 
             match currentRule with
             |ToBranch(_,left,right,l,w) -> left,right
             |ToLeaf(_,terminal,l)     -> terminal.ToString(),""  
          match i,l with
          | _,0 -> [(ToLeaf(top,leftNT.[0],None),lbl)]
          | _     -> (currentRule,lbl)::(subRecognize leftI leftL leftNT)@(subRecognize rightI rightL rightNT)
       let resultRules = if List.exists ((=)start) ((fun (a,_,_,_,l,w) -> a) recTable.[0, s.Length-1])
                         then subRecognize 0 (s.Length-1) start//если цепочка принадлежит языку L(g)
                         else []

       System.Console.WriteLine s
       System.Console.WriteLine "Rules:"
       printRules resultRules
       
       let _,_,_,_,ws,_ = recTable.[0,s.Length-1]

       let parseWeightCalc weights (n:int) = 
           for w in weights do
               match w with
               |Some v -> System.Console.Write (((float)v/(float)n).ToString())
               |None -> System.Console.Write ""
               System.Console.Write "; "

                        
       System.Console.Write "\nParse weights: "
       parseWeightCalc ws (s.Length - 1)
       //printOutput resultRules start    

    member this.Recognize grammar str = recognize grammar str

