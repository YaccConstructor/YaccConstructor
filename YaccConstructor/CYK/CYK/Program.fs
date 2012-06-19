#light
(*
Курсовая работа Лебедковой Татьяны, 361гр
на тему "Реализация алгоритма CYK для разбора языков, порожденных
контекстно-свободными грамматиками в нормальной форме Хомского"
по курсу "Программирование на F#"
*)

//Правила контекстно-свободной грамматики в нормальной форме Хомского
type Rule = 
   |ToBranch of string*string*string//А->BC, A,B,C - нетерминалы
   |ToLeaf of string*char//A->a, а - терминал

//Вывод правил
let rec printRules rs = 
   match rs with
   |(ToBranch(a,b,c))::xs -> (System.Console.WriteLine(String.concat "" [a;"->";b;c]))
                             printRules xs
   |(ToLeaf(a,b))::xs     -> (System.Console.WriteLine(String.concat "" [a;"->";b.ToString()]))
                             printRules xs                          
   |[]                    -> ()

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
                                |ToBranch(a,b,c) -> b::c::ls
                                |ToLeaf(a,b)     -> b.ToString()::ls
                             else
                                l::(newForm ls)
                   |_     -> []
                System.Console.Write(String.concat "" ("->"::(newForm current)))                   
                sentForm (newForm current) xs
      |[]    -> ()          
   System.Console.Write("\n"+start)
   sentForm [start] rs
   System.Console.WriteLine("\n\n")
                                                                       
let recognitionTable (rules,_) (s:string) = 
           
   let recTable = Microsoft.FSharp.Collections.Array2D.create s.Length s.Length ([],(0,0),(0,0),[]) 
   //recTable(i,l), для l > 0
   let elem i l = for rule in rules do
                     for k in 0..(l-1) do
                        match rule with
                        |ToBranch(a,b,c) -> let nonTerminals1,_,_,_ = recTable.[i,k]
                                            let nonTerminals2,_,_,_ = recTable.[k+i+1,l-k-1]
                                            let nonTerminals,_,_,rules = recTable.[i,l]
                                            if(List.exists ((=)b) nonTerminals1) && (List.exists ((=)c) nonTerminals2)then
                                               recTable.[i,l] <- ((a::nonTerminals),(i,k),(k+i+1,l-k-1),(rule::rules))
                        |_               -> ()                  
   //Заполнение RecognitionTable                                            
   let rec fillTable i l = if l = s.Length-1 then
                              elem i l//последний элемент таблицы
                           else if(i+l) <= (s.Length-1)then
                                   elem i l
                                   fillTable (i+1) l//продолжаем заполнять столбец
                                else
                                   fillTable 0 (l+1)//переход на новый столбец
   //первый столбец таблицы для правил, выводящих терминал
   for rule in rules do
      for k in 0..(s.Length-1) do
         match rule with
         |ToLeaf(a,b) -> if(b = s.[k])then
                            let nonTerminals,_,_,rules = recTable.[k,0]
                            recTable.[k,0] <- ((a::nonTerminals),(-1,-1),(-1,-1),(rule::rules))
         |_           -> ()                                            
   //последующие столбцы
   fillTable 0 1
   recTable                                           

//вывод строки s в грамматике g
let recognize ((rules, start)as g) s =
   let recTable = recognitionTable g s
   //восстановление вывода по таблице начиная с ячейки recTable(i,l) нетерминала top
   let rec subRecognize i l top = 
      let nonTerminals,(leftI,leftL),(rightI,rightL),rs = recTable.[i,l]
      //правило вида А->_, где А = top
      let currentRule = List.find (fun (ToBranch(st,_,_)|ToLeaf(st,_)) -> st = top) rs
      let leftNT,rightNT = 
         match currentRule with
         |ToBranch(_,left,right) -> left,right
         |ToLeaf(_,terminal)     -> terminal.ToString(),""  
      match (i,l) with 
      |(_,0) -> [ToLeaf(top,leftNT.[0])]
      |_     -> currentRule::(subRecognize leftI leftL leftNT)@(subRecognize rightI rightL rightNT)
   let resultRules = if List.exists ((=)start) ((fun (a,_,_,_) -> a) recTable.[0,s.Length-1]) then
                        subRecognize 0 (s.Length-1) start//если цепочка принадлежит языку L(g)
                     else
                        []
   System.Console.WriteLine(s)
   System.Console.WriteLine("Rules:")
   printRules resultRules
   printOutput resultRules start    



//грамматика, допускающая операции +, * и позволяющая группировать их с помощью скобок, преобразованная в НФХ
let testGrammar1 = ((List.map ToBranch [("E","T","N1");("N1","N2","E");("E","F","N3");("N3","N4","T");("E","N5","N6");("N6","E","N7");("E1","N2","E");("T","F","N3");("T","N5","N6");("T1","N4","T");("F","N5","N6")]) @ (List.map ToLeaf [("N2",'+');("N4",'*');("E",'a');("N5",'(');("N7",')');("T",'a');("F",'a')]),"E")
let testGrammar2 = ((List.map ToBranch [("S","A","B");("B","N1","N2");("N1","D1","D2")])@(List.map ToLeaf [("A",'a');("D2",'c');("B",'a');("D1",'b');("N2",'d')]),"S")
System.Console.WriteLine("Test1:")
recognize testGrammar1 "a*a+a"

System.Console.WriteLine("Test2:")
recognize testGrammar1 "a*a+a*(a+a)"

System.Console.WriteLine("Test3:")
recognize testGrammar2 "abcd"
System.Console.ReadLine()   
