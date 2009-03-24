#light

module Tree

type tree = 
     | Node  of (tree list)*string*(string list)*int
     | Leaf  of string*(string list)*int
     | RefTo of int
     
let rec dump_tree i item =
    let rec iter i = (function 0 -> "" | x -> ("    "+(iter (x-1))))i
    match item with
      Node (lst,name,a_ss,num) -> String.concat "" ([iter i;"<NODE name=\"";name;"\">\n"]@(List.map (dump_tree (i+1)) lst)@[iter i;"</NODE>\n"])
    | Leaf (name,a_ss,num)     -> String.concat "" [iter i;"<LEAF name=\"";name;"\" />\n"]    
    | RefTo (num)              -> String.concat "" [iter i;"<RefTo num=\"";num.ToString();"\"/>\n"]
    
let print_tree tree = System.Console.WriteLine (dump_tree 0 tree)     