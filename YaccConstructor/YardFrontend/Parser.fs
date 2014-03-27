
# 2 "Parser.fs"
module Yard.Frontends.YardFrontend.GrammarParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "Parser.fsy"

//  Copyright 2009 Jake Kirilenko
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#nowarn "62";; 
open Microsoft.FSharp.Text
open Yard.Core.IL
open Yard.Core
open Yard.Core.IL.Production
open Yard.Core.IL.Grammar
open Yard.Core.IL.Definition
open System.Text.RegularExpressions
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception ParseError of Source.t * string
let parseFile = ref Unchecked.defaultof<_>
let currentFilename = ref ""
let allPublic = ref false
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []
let fst_ (x,_,_) = x
let snd_ (_,x,_) = x
let trd_ (_,_,x) = x

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let makeNewSeq seq (lbl:Source.t) (w:Option<Source.t>) = 
    match seq,w with 
     | PSeq(els,ac,_),_ -> match w with
                           | None -> PSeq (els,ac,Some {label=lbl.text; weight=None})
                           | _ -> let wVal = w.Value
                                  try
                                      PSeq (els,ac,Some {label=lbl.text; weight = Some (float wVal.text)})
                                  with 
                                  | :? System.FormatException as ex ->
                                      failwithf "Parse error on position (%i,%i) on token %s: %s" wVal.startPos.line wVal.startPos.column wVal.text "illegal weight. Number expected."
     | x,_ -> x

let missing name = System.Console.WriteLine("Missing " + name)
let createSeqElem bnd omitted r check =
    { binding = bnd; omit = omitted; rule = r; checker = check }

let parseRules (filename:string) : Definition.t<Source.t, Source.t> =
    let oldFileName = !currentFilename
    currentFilename := filename
    let ext = filename.Substring(filename.LastIndexOf(".") + 1)    
    let userDefs =
        let args = oldFileName.Trim().Split('%') in
        if args.Length = 2
        then args.[1]
        else ""
    let sameDirFilename = System.IO.Path.Combine(System.IO.Path.GetDirectoryName oldFileName, filename) in
    let res = !parseFile (sameDirFilename + "%" + userDefs)
    currentFilename := oldFileName
    res

# 87 "Parser.fs"
type Token =
    | ACTION of (Source.t)
    | ALL_PUBLIC of (Source.t)
    | BAR of (Source.t)
    | BLOCK_END of (Source.t)
    | COLON of (Source.t)
    | COMMA of (Source.t)
    | DLABEL of (Source.t)
    | EOF of (Source.t)
    | EQUAL of (Source.t)
    | GREAT of (Source.t)
    | INCLUDE of (Source.t)
    | LESS of (Source.t)
    | LIDENT of (Source.t)
    | LITERAL of (Source.t)
    | LPAREN of (Source.t)
    | MINUS of (Source.t)
    | MODULE of (Source.t)
    | NUMBER of (Source.t)
    | OPEN of (Source.t)
    | OPTIONS_START of (Source.t)
    | PARAM of (Source.t)
    | PLUS of (Source.t)
    | PREDICATE of (Source.t)
    | PRIVATE of (Source.t)
    | PUBLIC of (Source.t)
    | QUESTION of (Source.t)
    | RNGLR_EOF of (Source.t)
    | RPAREN of (Source.t)
    | SEMICOLON of (Source.t)
    | SHARPLINE of (Source.t)
    | SQR_LBR of (Source.t)
    | SQR_RBR of (Source.t)
    | STAR of (Source.t)
    | START_RULE_SIGN of (Source.t)
    | STRING of (Source.t)
    | TOKENS_BLOCK of (Source.t)
    | UIDENT of (Source.t)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | ACTION x -> box x
    | ALL_PUBLIC x -> box x
    | BAR x -> box x
    | BLOCK_END x -> box x
    | COLON x -> box x
    | COMMA x -> box x
    | DLABEL x -> box x
    | EOF x -> box x
    | EQUAL x -> box x
    | GREAT x -> box x
    | INCLUDE x -> box x
    | LESS x -> box x
    | LIDENT x -> box x
    | LITERAL x -> box x
    | LPAREN x -> box x
    | MINUS x -> box x
    | MODULE x -> box x
    | NUMBER x -> box x
    | OPEN x -> box x
    | OPTIONS_START x -> box x
    | PARAM x -> box x
    | PLUS x -> box x
    | PREDICATE x -> box x
    | PRIVATE x -> box x
    | PUBLIC x -> box x
    | QUESTION x -> box x
    | RNGLR_EOF x -> box x
    | RPAREN x -> box x
    | SEMICOLON x -> box x
    | SHARPLINE x -> box x
    | SQR_LBR x -> box x
    | SQR_RBR x -> box x
    | STAR x -> box x
    | START_RULE_SIGN x -> box x
    | STRING x -> box x
    | TOKENS_BLOCK x -> box x
    | UIDENT x -> box x

let numToString = function
    | 0 -> "access_modifier_opt"
    | 1 -> "action_opt"
    | 2 -> "alts"
    | 3 -> "bar_seq_nlist"
    | 4 -> "bound"
    | 5 -> "call"
    | 6 -> "error"
    | 7 -> "file"
    | 8 -> "foot_opt"
    | 9 -> "formal_meta_list"
    | 10 -> "formal_meta_param_opt"
    | 11 -> "ident"
    | 12 -> "include_"
    | 13 -> "includes_or_options_or_tokens"
    | 14 -> "kw"
    | 15 -> "lbl_seq"
    | 16 -> "meta_param"
    | 17 -> "meta_param_opt"
    | 18 -> "meta_params"
    | 19 -> "module_"
    | 20 -> "module_header"
    | 21 -> "modules"
    | 22 -> "no_lbl_seq"
    | 23 -> "omit_opt"
    | 24 -> "open_list"
    | 25 -> "openings"
    | 26 -> "option"
    | 27 -> "option_block"
    | 28 -> "option_l_value"
    | 29 -> "option_value"
    | 30 -> "opts"
    | 31 -> "param_list"
    | 32 -> "param_opt"
    | 33 -> "patt"
    | 34 -> "predicate_opt"
    | 35 -> "prim"
    | 36 -> "rule"
    | 37 -> "rule_nlist"
    | 38 -> "semi_opt"
    | 39 -> "seq"
    | 40 -> "seq_elem"
    | 41 -> "seq_elem_list"
    | 42 -> "start_rule_sign_opt"
    | 43 -> "tada_rule"
    | 44 -> "tokens_block"
    | 45 -> "unnamed_module_opt"
    | 46 -> "weight_opt"
    | 47 -> "yard_start_rule"
    | 48 -> "ACTION"
    | 49 -> "ALL_PUBLIC"
    | 50 -> "BAR"
    | 51 -> "BLOCK_END"
    | 52 -> "COLON"
    | 53 -> "COMMA"
    | 54 -> "DLABEL"
    | 55 -> "EOF"
    | 56 -> "EQUAL"
    | 57 -> "GREAT"
    | 58 -> "INCLUDE"
    | 59 -> "LESS"
    | 60 -> "LIDENT"
    | 61 -> "LITERAL"
    | 62 -> "LPAREN"
    | 63 -> "MINUS"
    | 64 -> "MODULE"
    | 65 -> "NUMBER"
    | 66 -> "OPEN"
    | 67 -> "OPTIONS_START"
    | 68 -> "PARAM"
    | 69 -> "PLUS"
    | 70 -> "PREDICATE"
    | 71 -> "PRIVATE"
    | 72 -> "PUBLIC"
    | 73 -> "QUESTION"
    | 74 -> "RNGLR_EOF"
    | 75 -> "RPAREN"
    | 76 -> "SEMICOLON"
    | 77 -> "SHARPLINE"
    | 78 -> "SQR_LBR"
    | 79 -> "SQR_RBR"
    | 80 -> "STAR"
    | 81 -> "START_RULE_SIGN"
    | 82 -> "STRING"
    | 83 -> "TOKENS_BLOCK"
    | 84 -> "UIDENT"
    | _ -> ""

let tokenToNumber = function
    | ACTION _ -> 48
    | ALL_PUBLIC _ -> 49
    | BAR _ -> 50
    | BLOCK_END _ -> 51
    | COLON _ -> 52
    | COMMA _ -> 53
    | DLABEL _ -> 54
    | EOF _ -> 55
    | EQUAL _ -> 56
    | GREAT _ -> 57
    | INCLUDE _ -> 58
    | LESS _ -> 59
    | LIDENT _ -> 60
    | LITERAL _ -> 61
    | LPAREN _ -> 62
    | MINUS _ -> 63
    | MODULE _ -> 64
    | NUMBER _ -> 65
    | OPEN _ -> 66
    | OPTIONS_START _ -> 67
    | PARAM _ -> 68
    | PLUS _ -> 69
    | PREDICATE _ -> 70
    | PRIVATE _ -> 71
    | PUBLIC _ -> 72
    | QUESTION _ -> 73
    | RNGLR_EOF _ -> 74
    | RPAREN _ -> 75
    | SEMICOLON _ -> 76
    | SHARPLINE _ -> 77
    | SQR_LBR _ -> 78
    | SQR_RBR _ -> 79
    | STAR _ -> 80
    | START_RULE_SIGN _ -> 81
    | STRING _ -> 82
    | TOKENS_BLOCK _ -> 83
    | UIDENT _ -> 84

let isLiteral = function
    | ACTION _ -> false
    | ALL_PUBLIC _ -> false
    | BAR _ -> false
    | BLOCK_END _ -> false
    | COLON _ -> false
    | COMMA _ -> false
    | DLABEL _ -> false
    | EOF _ -> false
    | EQUAL _ -> false
    | GREAT _ -> false
    | INCLUDE _ -> false
    | LESS _ -> false
    | LIDENT _ -> false
    | LITERAL _ -> false
    | LPAREN _ -> false
    | MINUS _ -> false
    | MODULE _ -> false
    | NUMBER _ -> false
    | OPEN _ -> false
    | OPTIONS_START _ -> false
    | PARAM _ -> false
    | PLUS _ -> false
    | PREDICATE _ -> false
    | PRIVATE _ -> false
    | PUBLIC _ -> false
    | QUESTION _ -> false
    | RNGLR_EOF _ -> false
    | RPAREN _ -> false
    | SEMICOLON _ -> false
    | SHARPLINE _ -> false
    | SQR_LBR _ -> false
    | SQR_RBR _ -> false
    | STAR _ -> false
    | START_RULE_SIGN _ -> false
    | STRING _ -> false
    | TOKENS_BLOCK _ -> false
    | UIDENT _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|14; 14; 14; 14; 14; 7; 47; 13; 13; 13; 13; 44; 12; 27; 30; 30; 26; 29; 29; 29; 28; 28; 45; 21; 21; 19; 11; 11; 20; 20; 25; 25; 24; 24; 1; 1; 8; 8; 37; 37; 36; 42; 42; 0; 0; 0; 10; 10; 9; 9; 32; 32; 31; 31; 46; 46; 2; 2; 3; 3; 39; 39; 22; 22; 15; 41; 41; 40; 23; 23; 38; 38; 34; 34; 4; 4; 33; 33; 35; 35; 35; 35; 35; 35; 35; 35; 16; 18; 18; 17; 17; 5; 5; 43; 43|]
let private rules = [|71; 72; 66; 58; 64; 1; 13; 45; 21; 8; 55; 7; 44; 13; 27; 13; 12; 13; 83; 58; 82; 67; 30; 51; 26; 30; 28; 56; 29; 14; 82; 11; 14; 11; 37; 19; 21; 20; 11; 25; 37; 60; 84; 64; 49; 64; 66; 11; 24; 53; 11; 24; 48; 76; 48; 36; 38; 37; 42; 0; 60; 10; 31; 52; 2; 81; 71; 72; 59; 9; 57; 60; 9; 60; 68; 68; 31; 78; 65; 79; 39; 3; 39; 50; 39; 50; 39; 3; 15; 22; 48; 40; 41; 1; 54; 46; 62; 22; 75; 40; 41; 23; 4; 34; 63; 76; 70; 35; 33; 56; 35; 48; 60; 61; 15; 5; 62; 2; 75; 78; 2; 79; 35; 73; 35; 69; 35; 80; 35; 16; 18; 16; 59; 18; 57; 60; 17; 32; 84; 55; 77|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 11; 12; 14; 16; 18; 18; 19; 21; 24; 24; 26; 29; 30; 31; 32; 33; 34; 35; 35; 37; 41; 42; 43; 44; 46; 49; 49; 49; 52; 53; 53; 55; 55; 55; 58; 65; 66; 66; 66; 67; 68; 71; 71; 73; 74; 75; 75; 77; 77; 80; 80; 82; 83; 85; 88; 89; 90; 91; 94; 99; 101; 101; 104; 105; 105; 106; 106; 107; 107; 108; 111; 112; 113; 114; 115; 116; 119; 122; 124; 126; 128; 129; 131; 132; 135; 135; 138; 139; 140; 141|]
let startRule = 6

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 131; 86; 2; 31; 4; 6; 8; 10; 30; 3; 5; 7; 9; 11; 12; 13; 14; 28; 19; 20; 21; 22; 23; 24; 26; 27; 15; 16; 17; 18; 25; 29; 32; 109; 35; 110; 107; 33; 108; 34; 36; 105; 106; 37; 38; 100; 39; 98; 40; 41; 42; 43; 44; 75; 80; 87; 56; 83; 45; 48; 49; 50; 92; 97; 93; 71; 72; 88; 91; 46; 47; 51; 52; 61; 53; 54; 55; 57; 94; 58; 59; 60; 62; 65; 63; 64; 66; 69; 68; 67; 70; 73; 74; 76; 77; 78; 79; 81; 84; 82; 85; 89; 90; 95; 96; 99; 101; 103; 102; 104; 111; 112; 126; 123; 125; 122; 113; 114; 116; 115; 117; 118; 119; 120; 121; 124; 127; 129; 128; 130|]
let private small_gotos =
        [|3; 65536; 458753; 3145730; 65543; 786435; 851972; 1769477; 2883590; 3801095; 4390920; 5439497; 131079; 786435; 851978; 1769477; 2883590; 3801095; 4390920; 5439497; 262151; 786435; 851979; 1769477; 2883590; 3801095; 4390920; 5439497; 393223; 786435; 851980; 1769477; 2883590; 3801095; 4390920; 5439497; 524289; 5373965; 655372; 720910; 917519; 1703952; 1835025; 1966098; 3801107; 3932180; 4194325; 4325398; 4653079; 4718616; 5505049; 851980; 720910; 917519; 1703952; 1835025; 1966106; 3801107; 3932180; 4194325; 4325398; 4653079; 4718616; 5505049; 917505; 3670043; 983051; 720924; 917533; 1900574; 3801107; 3932180; 4194325; 4325398; 4653079; 4718616; 5373983; 5505049; 1835009; 3342368; 2031621; 2359329; 2424866; 2752547; 2949156; 5308453; 2097154; 2490406; 4980775; 2162692; 2359329; 2424872; 2752547; 5308453; 2293763; 41; 4653098; 4718635; 2359297; 3932204; 2424834; 655405; 3866670; 2490370; 2031663; 4456496; 2555905; 3407921; 2621449; 131122; 983091; 1441844; 1507381; 2555958; 2621495; 3145784; 3539001; 4128826; 2883596; 262203; 327740; 983101; 2162750; 2293823; 3145792; 3539001; 3932225; 3997762; 4063299; 5111876; 5505093; 2949122; 2228294; 4587591; 3276801; 3670088; 3342345; 327740; 983101; 2293833; 3539001; 3932234; 3997762; 4063299; 5111876; 5505093; 3407875; 4522059; 4784204; 5242957; 3670018; 3014734; 5111887; 3735553; 4063312; 3801093; 1441873; 1507381; 2621495; 3145784; 4128826; 3866625; 4915282; 3997698; 1114195; 3866708; 4063234; 2097237; 4456534; 4259851; 327740; 983101; 1048663; 1179736; 2293849; 3539001; 3932234; 3997762; 4063299; 5111876; 5505093; 4325387; 327740; 983101; 1048663; 1179738; 2293849; 3539001; 3932234; 3997762; 4063299; 5111876; 5505093; 4456451; 4522059; 4784204; 5242957; 4521985; 3735643; 4718601; 131164; 983091; 1441844; 1507381; 2555958; 2621495; 3145784; 3539001; 4128826; 4784129; 4915293; 4915202; 196702; 3276895; 5046280; 983091; 1441844; 1507381; 2556000; 2621495; 3145784; 3539001; 4128826; 5111810; 196705; 3276895; 5242884; 1507381; 2621538; 2687075; 4128826; 5308420; 1507381; 2621538; 2687076; 4128826; 5505026; 65637; 3145730; 5767177; 131174; 983091; 1441844; 1507381; 2555958; 2621495; 3145784; 3539001; 4128826; 5832705; 5177447; 6029315; 4522059; 4784204; 5242957; 6094850; 1114195; 3866708; 6160385; 4259944; 6225921; 5177449; 6422530; 2031722; 4456496; 6553602; 589931; 3932268; 6619137; 3735661; 6750210; 589934; 3932268; 7208965; 1245295; 1310832; 1376369; 3211378; 4194419; 7274501; 1245295; 1310832; 1376372; 3211378; 4194419; 7340035; 721013; 3932180; 5505049; 7405570; 1638518; 4325495; 7471108; 2359329; 2424952; 2752547; 5308453; 7602179; 721017; 3932180; 5505049; 7667714; 1572986; 3473531; 7798787; 721020; 3932180; 5505049; 7864322; 1572989; 3473531; 8060929; 4194430; 8257538; 524415; 4980864; 8323073; 3604609; 8454145; 3145858|]
let gotos = Array.zeroCreate 132
for i = 0 to 131 do
        gotos.[i] <- Array.zeroCreate 85
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|9,1|]; [|9,2|]; [|8,1|]; [|8,2|]; [|7,1|]; [|7,2|]; [|12,2|]; [|21,1|]; [|20,1|]; [|15,1|]; [|19,1|]; [|17,1|]; [|16,3|]; [|3,1|]; [|26,1|]; [|4,1|]; [|2,1|]; [|0,1|]; [|1,1|]; [|18,1|]; [|27,1|]; [|15,2|]; [|13,3|]; [|11,1|]; [|39,1|]; [|39,2|]; [|39,3|]; [|40,7|]; [|60,1|]; [|61,1|]; [|67,2|]; [|67,3|]; [|72,1|]; [|80,1|]; [|79,1|]; [|75,3|]; [|84,2|]; [|83,2|]; [|85,2|]; [|64,5|]; [|91,1|]; [|91,2|]; [|91,3|]; [|50,1|]; [|88,1|]; [|87,2|]; [|86,1|]; [|89,3|]; [|78,1|]; [|81,3|]; [|57,1|]; [|56,2|]; [|58,2|]; [|59,3|]; [|63,1|]; [|65,1|]; [|65,2|]; [|68,1|]; [|63,2|]; [|63,3|]; [|34,1|]; [|62,1|]; [|82,3|]; [|92,1|]; [|74,1|]; [|77,1|]; [|54,3|]; [|76,1|]; [|52,1|]; [|52,2|]; [|46,3|]; [|49,1|]; [|48,2|]; [|44,1|]; [|45,1|]; [|41,1|]; [|70,1|]; [|22,1|]; [|24,1|]; [|25,2|]; [|25,3|]; [|25,4|]; [|30,2|]; [|30,3|]; [|33,2|]; [|33,3|]; [|24,2|]; [|29,2|]; [|28,1|]; [|5,6|]; [|36,2|]|]
let private small_reduces =
        [|131080; 3211264; 3604480; 3932160; 4194304; 4653056; 4718592; 4980736; 5308416; 196616; 3211265; 3604481; 3932161; 4194305; 4653057; 4718593; 4980737; 5308417; 262152; 3211266; 3604482; 3932162; 4194306; 4653058; 4718594; 4980738; 5308418; 327688; 3211267; 3604483; 3932163; 4194307; 4653059; 4718595; 4980739; 5308419; 393224; 3211268; 3604484; 3932164; 4194308; 4653060; 4718596; 4980740; 5308420; 458760; 3211269; 3604485; 3932165; 4194309; 4653061; 4718597; 4980741; 5308421; 589835; 3211270; 3604486; 3801094; 3932166; 4194310; 4390918; 4653062; 4718598; 4980742; 5308422; 5439494; 720897; 3670023; 786433; 3670024; 851969; 3342345; 1048584; 3342346; 3801098; 3932170; 4194314; 4325386; 4653066; 4718602; 5505034; 1114120; 3342347; 3801099; 3932171; 4194315; 4325387; 4653067; 4718603; 5505035; 1179656; 3342348; 3801100; 3932172; 4194316; 4325388; 4653068; 4718604; 5505036; 1245193; 3342349; 3670029; 3801101; 3932173; 4194317; 4325389; 4653069; 4718605; 5505037; 1310734; 3211278; 3342350; 3473422; 3604494; 3670030; 3801102; 3932174; 4194318; 4325390; 4653070; 4718606; 4980750; 5308430; 5505038; 1376265; 3342351; 3670031; 3801103; 3932175; 4194319; 4325391; 4653071; 4718607; 5505039; 1441801; 3342352; 3670032; 3801104; 3932176; 4194320; 4325392; 4653072; 4718608; 5505040; 1507337; 3342353; 3670033; 3801105; 3932177; 4194321; 4325393; 4653073; 4718609; 5505041; 1572873; 3342354; 3670034; 3801106; 3932178; 4194322; 4325394; 4653074; 4718610; 5505042; 1638408; 3342355; 3801107; 3932179; 4194323; 4325395; 4653075; 4718611; 5505043; 1703950; 3211284; 3342356; 3473428; 3604500; 3670036; 3801108; 3932180; 4194324; 4325396; 4653076; 4718612; 4980756; 5308436; 5505044; 1769473; 3342357; 1900555; 3211286; 3604502; 3801110; 3932182; 4194326; 4390934; 4653078; 4718614; 4980758; 5308438; 5439510; 1966091; 3211287; 3604503; 3801111; 3932183; 4194327; 4390935; 4653079; 4718615; 4980759; 5308439; 5439511; 2097156; 3211288; 3604504; 4194328; 4980760; 2162692; 3211289; 3604505; 4194329; 4980761; 2228228; 3211290; 3604506; 4194330; 4980762; 2686984; 3211291; 3604507; 3932187; 4194331; 4653083; 4718619; 4980763; 5308443; 2752523; 3211292; 3276828; 3604508; 3932188; 4194332; 4653084; 4718620; 4915228; 4980764; 5177372; 5308444; 2818059; 3211293; 3276829; 3604509; 3932189; 4194333; 4653085; 4718621; 4915229; 4980765; 5177373; 5308445; 2949138; 3145758; 3211294; 3276830; 3538974; 3604510; 3932190; 3997726; 4063262; 4128798; 4194334; 4653086; 4718622; 4915230; 4980766; 5111838; 5177374; 5308446; 5505054; 3014674; 3145759; 3211295; 3276831; 3538975; 3604511; 3932191; 3997727; 4063263; 4128799; 4194335; 4653087; 4718623; 4915231; 4980767; 5111839; 5177375; 5308447; 5505055; 3080210; 3145760; 3211296; 3276832; 3538976; 3604512; 3932192; 3997728; 4063264; 4128800; 4194336; 4653088; 4718624; 4915232; 4980768; 5111840; 5177376; 5308448; 5505056; 3145751; 3145761; 3211297; 3276833; 3538977; 3604513; 3735585; 3932193; 3997729; 4063265; 4128801; 4194337; 4522017; 4587553; 4653089; 4718625; 4784161; 4915233; 4980769; 5111841; 5177377; 5242913; 5308449; 5505057; 3211287; 3145762; 3211298; 3276834; 3538978; 3604514; 3735586; 3932194; 3997730; 4063266; 4128802; 4194338; 4522018; 4587554; 4653090; 4718626; 4784162; 4915234; 4980770; 5111842; 5177378; 5242914; 5308450; 5505058; 3407891; 3145763; 3211299; 3276835; 3538979; 3604515; 3932195; 3997731; 4063267; 4128803; 4194339; 4587555; 4653091; 4718627; 4915235; 4980771; 5111843; 5177379; 5308451; 5505059; 3473431; 3145764; 3211300; 3276836; 3538980; 3604516; 3735588; 3932196; 3997732; 4063268; 4128804; 4194340; 4522020; 4587556; 4653092; 4718628; 4784164; 4915236; 4980772; 5111844; 5177380; 5242916; 5308452; 5505060; 3538967; 3145765; 3211301; 3276837; 3538981; 3604517; 3735589; 3932197; 3997733; 4063269; 4128805; 4194341; 4522021; 4587557; 4653093; 4718629; 4784165; 4915237; 4980773; 5111845; 5177381; 5242917; 5308453; 5505061; 3604503; 3145766; 3211302; 3276838; 3538982; 3604518; 3735590; 3932198; 3997734; 4063270; 4128806; 4194342; 4522022; 4587558; 4653094; 4718630; 4784166; 4915238; 4980774; 5111846; 5177382; 5242918; 5308454; 5505062; 3932183; 3145767; 3211303; 3276839; 3538983; 3604519; 3735591; 3932199; 3997735; 4063271; 4128807; 4194343; 4522023; 4587559; 4653095; 4718631; 4784167; 4915239; 4980775; 5111847; 5177383; 5242919; 5308455; 5505063; 3997719; 3145768; 3211304; 3276840; 3538984; 3604520; 3735592; 3932200; 3997736; 4063272; 4128808; 4194344; 4522024; 4587560; 4653096; 4718632; 4784168; 4915240; 4980776; 5111848; 5177384; 5242920; 5308456; 5505064; 4063255; 3145769; 3211305; 3276841; 3538985; 3604521; 3735593; 3932201; 3997737; 4063273; 4128809; 4194345; 4522025; 4587561; 4653097; 4718633; 4784169; 4915241; 4980777; 5111849; 5177385; 5242921; 5308457; 5505065; 4128791; 3145770; 3211306; 3276842; 3538986; 3604522; 3735594; 3932202; 3997738; 4063274; 4128810; 4194346; 4522026; 4587562; 4653098; 4718634; 4784170; 4915242; 4980778; 5111850; 5177386; 5242922; 5308458; 5505066; 4194327; 3145771; 3211307; 3276843; 3538987; 3604523; 3735595; 3932203; 3997739; 4063275; 4128811; 4194347; 4522027; 4587563; 4653099; 4718635; 4784171; 4915243; 4980779; 5111851; 5177387; 5242923; 5308459; 5505067; 4325377; 3735596; 4390913; 3735597; 4456455; 3538990; 3735598; 3932206; 3997742; 4063278; 5111854; 5505070; 4587544; 3145775; 3211311; 3276847; 3538991; 3604527; 3735599; 3932207; 3997743; 4063279; 4128815; 4194351; 4456495; 4522031; 4587567; 4653103; 4718639; 4784175; 4915247; 4980783; 5111855; 5177391; 5242927; 5308463; 5505071; 4653079; 3145776; 3211312; 3276848; 3538992; 3604528; 3735600; 3932208; 3997744; 4063280; 4128816; 4194352; 4522032; 4587568; 4653104; 4718640; 4784176; 4915248; 4980784; 5111856; 5177392; 5242928; 5308464; 5505072; 4849687; 3145777; 3211313; 3276849; 3538993; 3604529; 3735601; 3932209; 3997745; 4063281; 4128817; 4194353; 4522033; 4587569; 4653105; 4718641; 4784177; 4915249; 4980785; 5111857; 5177393; 5242929; 5308465; 5505073; 4915210; 3211314; 3604530; 3932210; 4194354; 4653106; 4718642; 4915250; 4980786; 5177394; 5308466; 4980746; 3211315; 3604531; 3932211; 4194355; 4653107; 4718643; 4915251; 4980787; 5177395; 5308467; 5111818; 3211316; 3604532; 3932212; 4194356; 4653108; 4718644; 4915252; 4980788; 5177396; 5308468; 5177354; 3211317; 3604533; 3932213; 4194357; 4653109; 4718645; 4915253; 4980789; 5177397; 5308469; 5242891; 3211318; 3276854; 3604534; 3932214; 4194358; 4653110; 4718646; 4915254; 4980790; 5177398; 5308470; 5308428; 3145783; 3211319; 3276855; 3604535; 3932215; 4194359; 4653111; 4718647; 4915255; 4980791; 5177399; 5308471; 5373964; 3145784; 3211320; 3276856; 3604536; 3932216; 4194360; 4653112; 4718648; 4915256; 4980792; 5177400; 5308472; 5439495; 3145785; 3539001; 3932217; 3997753; 4063289; 5111865; 5505081; 5505035; 3211322; 3276858; 3604538; 3932218; 4194362; 4653114; 4718650; 4915258; 4980794; 5177402; 5308474; 5570571; 3211323; 3276859; 3604539; 3932219; 4194363; 4653115; 4718651; 4915259; 4980795; 5177403; 5308475; 5636110; 3211324; 3276860; 3604540; 3801148; 3932220; 4194364; 4390972; 4653116; 4718652; 4915260; 4980796; 5177404; 5308476; 5439548; 5701643; 3211325; 3276861; 3604541; 3932221; 4194365; 4653117; 4718653; 4915261; 4980797; 5177405; 5308477; 5898263; 3145790; 3211326; 3276862; 3539006; 3604542; 3735614; 3932222; 3997758; 4063294; 4128830; 4194366; 4522046; 4587582; 4653118; 4718654; 4784190; 4915262; 4980798; 5111870; 5177406; 5242942; 5308478; 5505086; 5963799; 3145791; 3211327; 3276863; 3539007; 3604543; 3735615; 3932223; 3997759; 4063295; 4128831; 4194367; 4522047; 4587583; 4653119; 4718655; 4784191; 4915263; 4980799; 5111871; 5177407; 5242943; 5308479; 5505087; 6029331; 3145792; 3211328; 3276864; 3539008; 3604544; 3932224; 3997760; 4063296; 4128832; 4194368; 4587584; 4653120; 4718656; 4915264; 4980800; 5111872; 5177408; 5308480; 5505088; 6094871; 3145768; 3211304; 3276840; 3538984; 3604520; 3670081; 3932200; 3997736; 4063272; 4128808; 4194344; 4522024; 4587560; 4653096; 4718632; 4784168; 4915240; 4980776; 5111848; 5177384; 5242920; 5308456; 5505064; 6291457; 4063298; 6356993; 3670083; 6422529; 3407940; 6488065; 3407941; 6684674; 3407942; 4456518; 6750209; 3735623; 6815745; 3735624; 6881281; 3932233; 6946817; 3932234; 7012355; 3932235; 4653131; 4718667; 7077896; 3211340; 3604556; 3932236; 4194380; 4653132; 4718668; 4980812; 5308492; 7143428; 3211341; 3604557; 4194381; 4980813; 7274498; 3604558; 4980814; 7405572; 3211343; 3604559; 4194383; 4980815; 7471108; 3211344; 3604560; 4194384; 4980816; 7536644; 3211345; 3604561; 4194385; 4980817; 7667720; 3211346; 3604562; 3932242; 4194386; 4653138; 4718674; 4980818; 5308498; 7733256; 3211347; 3604563; 3932243; 4194387; 4653139; 4718675; 4980819; 5308499; 7864328; 3211348; 3604564; 3932244; 4194388; 4653140; 4718676; 4980820; 5308500; 7929864; 3211349; 3604565; 3932245; 4194389; 4653141; 4718677; 4980821; 5308501; 7995394; 3604566; 4980822; 8126466; 3932247; 5505111; 8192002; 3932248; 5505112; 8388609; 4849753; 8519681; 3604570|]
let reduces = Array.zeroCreate 132
for i = 0 to 131 do
        reduces.[i] <- Array.zeroCreate 85
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|35|]; [|10|]; [|14|]; [|38; 22|]; [|42|]; [|71|]; [|38|]; [|43|]; [|47|]; [|53|]; [|69|]; [|73|]; [|55|]; [|90|]; [|51|]; [|69; 66|]; [|66|]; [|23|]; [|31|]; [|32|]; [|37|]|]
let private small_zeroReduces =
        [|11; 3211264; 3604480; 3801088; 3932160; 4194304; 4390912; 4653056; 4718592; 4980736; 5308416; 5439488; 65544; 3211265; 3604481; 3932161; 4194305; 4653057; 4718593; 4980737; 5308417; 131080; 3211265; 3604481; 3932161; 4194305; 4653057; 4718593; 4980737; 5308417; 262152; 3211265; 3604481; 3932161; 4194305; 4653057; 4718593; 4980737; 5308417; 393224; 3211265; 3604481; 3932161; 4194305; 4653057; 4718593; 4980737; 5308417; 655361; 3342338; 851969; 3342338; 2031623; 3211267; 3604483; 3932164; 4194307; 4653060; 4718596; 4980739; 2097160; 3211269; 3604485; 3932165; 4194309; 4653061; 4718597; 4980741; 5308421; 2162695; 3211270; 3604486; 3932164; 4194310; 4653060; 4718596; 4980742; 2293761; 3932167; 2424834; 3407880; 4456456; 2490369; 3407881; 2621447; 3145738; 3538954; 3932170; 3997706; 4063242; 5111818; 5505034; 2949138; 3145739; 3211275; 3276811; 3538955; 3604491; 3932171; 3997707; 4063243; 4128779; 4194315; 4653067; 4718603; 4915211; 4980747; 5111819; 5177355; 5308427; 5505035; 3670017; 4063244; 3801095; 3145738; 3538954; 3932170; 3997706; 4063242; 5111818; 5505034; 3997720; 3145741; 3211277; 3276813; 3538957; 3604493; 3735565; 3932173; 3997709; 4063245; 4128781; 4194317; 4456461; 4521997; 4587533; 4653069; 4718605; 4784141; 4915213; 4980749; 5111821; 5177357; 5242893; 5308429; 5505037; 4063255; 3145742; 3211278; 3276814; 3538958; 3604494; 3735566; 3932174; 3997710; 4063246; 4128782; 4194318; 4521998; 4587534; 4653070; 4718606; 4784142; 4915214; 4980750; 5111822; 5177358; 5242894; 5308430; 5505038; 4718599; 3145738; 3538954; 3932170; 3997706; 4063242; 5111818; 5505034; 5046279; 3145738; 3538954; 3932170; 3997706; 4063242; 5111818; 5505034; 5242897; 3145743; 3211280; 3276816; 3538954; 3604496; 3932175; 3997706; 4063242; 4194320; 4653072; 4718608; 4915216; 4980752; 5111818; 5177360; 5308432; 5505034; 5308433; 3145743; 3211280; 3276816; 3538954; 3604496; 3932175; 3997706; 4063242; 4194320; 4653072; 4718608; 4915216; 4980752; 5111818; 5177360; 5308432; 5505034; 5505035; 3211264; 3276800; 3604480; 3932160; 4194304; 4653056; 4718592; 4915200; 4980736; 5177344; 5308416; 5767175; 3145738; 3538954; 3932170; 3997706; 4063242; 5111818; 5505034; 6094871; 3145741; 3211277; 3276813; 3538957; 3604493; 3932173; 3997709; 4063245; 4128781; 4194317; 4456461; 4521997; 4587533; 4653069; 4718605; 4784141; 4915213; 4980749; 5111821; 5177357; 5242893; 5308429; 5505037; 6422529; 3407881; 7208962; 3604497; 4980753; 7274498; 3604497; 4980753; 7405576; 3211282; 3604498; 3932178; 4194322; 4653074; 4718610; 4980754; 5308434; 7471111; 3211270; 3604486; 3932164; 4194310; 4653060; 4718596; 4980742; 7667720; 3211283; 3604499; 3932179; 4194323; 4653075; 4718611; 4980755; 5308435; 7864328; 3211283; 3604499; 3932179; 4194323; 4653075; 4718611; 4980755; 5308435; 8257537; 3604500|]
let zeroReduces = Array.zeroCreate 132
for i = 0 to 131 do
        zeroReduces.[i] <- Array.zeroCreate 85
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [131]
let private accStates = Array.zeroCreate 132
for i = 0 to 131 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 74
let errorIndex = 6
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(47, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(90, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(31, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(73, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(66, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(22, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(47, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(90, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(31, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(73, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(66, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(22, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_error * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes_or_options_or_tokens * '_rnglr_type_kw * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option * '_rnglr_type_option_block * '_rnglr_type_option_l_value * '_rnglr_type_option_value * '_rnglr_type_opts * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_tokens_block * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PRIVATE _rnglr_val -> [_rnglr_val] | a -> failwith "PRIVATE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 109 "Parser.fsy"
                                                                                     _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 109 "Parser.fsy"
               : '_rnglr_type_kw) 
# 430 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "PUBLIC expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 109 "Parser.fsy"
                                                                      _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 109 "Parser.fsy"
               : '_rnglr_type_kw) 
# 450 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPEN _rnglr_val -> [_rnglr_val] | a -> failwith "OPEN expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 109 "Parser.fsy"
                                                        _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 109 "Parser.fsy"
               : '_rnglr_type_kw) 
# 470 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with INCLUDE _rnglr_val -> [_rnglr_val] | a -> failwith "INCLUDE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 109 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 109 "Parser.fsy"
               : '_rnglr_type_kw) 
# 490 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 109 "Parser.fsy"
                             _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 109 "Parser.fsy"
               : '_rnglr_type_kw) 
# 510 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      (match ((unbox _rnglr_children.[5]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
                       |> List.iter (fun (_) -> 
                        _rnglr_cycle_res := (
                          
# 117 "Parser.fsy"
                                  
                                  {
                                      info = { fileName = !currentFilename }
                                      head = _S1
                                      grammar = fst_ _S2 @ _S3 @ _S4
                                      foot = _S5
                                      options = snd_ _S2
                                      tokens = trd_ _S2
                                  }
                                
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 111 "Parser.fsy"
               : '_rnglr_type_file) 
# 549 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 111 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 559 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_tokens_block) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 133 "Parser.fsy"
                                                                   fst_ _S2, snd_ _S2, joinMaps _S1 (trd_ _S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 581 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_block) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 132 "Parser.fsy"
                                                                     fst_ _S2, joinMaps _S1 (snd_ _S2), trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 603 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_include_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 131 "Parser.fsy"
                                                                     (_S1 @ fst_ _S2), snd_ _S2, trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 625 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 130 "Parser.fsy"
                     [],    Map.empty, Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 643 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with TOKENS_BLOCK _rnglr_val -> [_rnglr_val] | a -> failwith "TOKENS_BLOCK expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 136 "Parser.fsy"
                   
                    let block = _S1.text
                    let inner = block.[block.IndexOf '{' + 1 .. block.LastIndexOf '}' - 1]
                    inner.Split [|'|'|]
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter ((<>) "")
                    |> Array.map (fun s ->
                        let genError msg = raise <| ParseError (new Source.t(s, fst parserRange, snd parserRange, !currentFilename),
                                                                "Error in tokens block: " + msg)
                        if Regex.IsMatch(s, @"^(_|[A-Z][A-Za-z0-9_]*)$") then s, None
                        else
                            let m = Regex.Match(s, @"^(_|[A-Z][A-Za-z0-9_]*)\s*of\s*(.*)$")
                            if m.Success then
                                m.Groups.[1].Value, Some m.Groups.[2].Value
                            else
                                if not (System.Char.IsUpper s.[0]) && not (s.[0] = '_' && s.Length > 1 && System.Char.IsWhiteSpace s.[1])
                                then genError "Terminal must start from upper letter"
                                else genError "Token type description is incorrect"
                            
                    ) |> Map.ofArray
                  
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 135 "Parser.fsy"
               : '_rnglr_type_tokens_block) 
# 683 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with INCLUDE _rnglr_val -> [_rnglr_val] | a -> failwith "INCLUDE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 160 "Parser.fsy"
                       
                          let grammar = (parseRules _S2.text).grammar
                          if grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          grammar
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_include_) 
# 710 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPTIONS_START _rnglr_val -> [_rnglr_val] | a -> failwith "OPTIONS_START expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_opts) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with BLOCK_END _rnglr_val -> [_rnglr_val] | a -> failwith "BLOCK_END expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 167 "Parser.fsy"
                                                                 Map.ofList _S2 : Map<_,_>
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 167 "Parser.fsy"
               : '_rnglr_type_option_block) 
# 734 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 169 "Parser.fsy"
                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_opts) 
# 752 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_opts) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 169 "Parser.fsy"
                                      _S1::_S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_opts) 
# 774 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_l_value) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_option_value) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 171 "Parser.fsy"
                                                                (_S1 : Source.t).text, (_S3 : Source.t).text 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_option) 
# 798 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_kw) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                                                  _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 818 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 838 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_ident) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 858 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_kw) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 175 "Parser.fsy"
                                                    _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_option_l_value) 
# 878 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_ident) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 175 "Parser.fsy"
                                        _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_option_l_value) 
# 898 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_rule_nlist) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 178 "Parser.fsy"
                     
                        match _S1 with
                        | [] -> []
                        | x ->  defaultModules x
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 177 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 922 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 184 "Parser.fsy"
                                                         [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 184 "Parser.fsy"
               : '_rnglr_type_modules) 
# 940 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_module_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_modules) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 184 "Parser.fsy"
                                              _S1 :: _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 184 "Parser.fsy"
               : '_rnglr_type_modules) 
# 962 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_module_header) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_openings) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_rule_nlist) 
                   |> List.iter (fun (_S4) -> 
                    _rnglr_cycle_res := (
                      
# 187 "Parser.fsy"
                           
                              {
                                  allPublic = _S1
                                  name = Some _S2
                                  openings = _S3
                                  rules = _S4
                              }
                          
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 186 "Parser.fsy"
               : '_rnglr_type_module_) 
# 995 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 196 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_ident) 
# 1015 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with UIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "UIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 196 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_ident) 
# 1035 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 203 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 1055 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ALL_PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "ALL_PUBLIC expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 199 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 1080 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPEN _rnglr_val -> [_rnglr_val] | a -> failwith "OPEN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 205 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_openings) 
# 1104 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 205 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_openings) 
# 1122 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 207 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1140 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 207 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1164 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 209 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1184 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 209 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1202 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMICOLON _rnglr_val -> [_rnglr_val] | a -> failwith "SEMICOLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 211 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1224 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 211 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1242 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 215 "Parser.fsy"
                      [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 213 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1260 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_rule) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_semi_opt) 
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_rule_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 214 "Parser.fsy"
                            _S1::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 213 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1284 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_start_rule_sign_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_access_modifier_opt) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_formal_meta_param_opt) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_param_list) 
                     |> List.iter (fun (_S5) -> 
                      (match ((unbox _rnglr_children.[5]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
                       |> List.iter (fun (_) -> 
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_alts) 
                         |> List.iter (fun (_S7) -> 
                          _rnglr_cycle_res := (
                            
# 218 "Parser.fsy"
                                  
                                    {
                                        Rule.isStart = _S1
                                        Rule.isPublic = _S2
                                        Rule.name = _S3
                                        Rule.metaArgs = getList _S4
                                        Rule.body = _S7
                                        Rule.args = _S5
                                    }
                                
                              )::!_rnglr_cycle_res ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 217 "Parser.fsy"
               : '_rnglr_type_rule) 
# 1325 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with START_RULE_SIGN _rnglr_val -> [_rnglr_val] | a -> failwith "START_RULE_SIGN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 229 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1345 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 229 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1363 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 231 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1381 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PRIVATE _rnglr_val -> [_rnglr_val] | a -> failwith "PRIVATE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 231 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1401 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "PUBLIC expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 231 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1421 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LESS _rnglr_val -> [_rnglr_val] | a -> failwith "LESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with GREAT _rnglr_val -> [_rnglr_val] | a -> failwith "GREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 233 "Parser.fsy"
                                                                                   Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1445 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 233 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1463 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 236 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1485 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 235 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1505 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PARAM _rnglr_val -> [_rnglr_val] | a -> failwith "PARAM expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 238 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1525 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 238 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1543 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PARAM _rnglr_val -> [_rnglr_val] | a -> failwith "PARAM expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_param_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 240 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 240 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1565 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 240 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 240 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1583 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SQR_LBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_LBR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwith "NUMBER expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with SQR_RBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_RBR expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 242 "Parser.fsy"
                                                                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 242 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1607 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 242 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 242 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1625 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_bar_seq_nlist) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 244 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 244 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1647 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 244 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 244 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1667 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with BAR _rnglr_val -> [_rnglr_val] | a -> failwith "BAR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 247 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 246 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1689 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with BAR _rnglr_val -> [_rnglr_val] | a -> failwith "BAR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_bar_seq_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 246 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 246 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1713 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 249 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 249 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1733 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_no_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 249 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 249 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1753 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 252 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1773 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq_elem) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq_elem_list) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_action_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 251 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1797 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLABEL _rnglr_val -> [_rnglr_val] | a -> failwith "DLABEL expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_weight_opt) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_no_lbl_seq) 
                   |> List.iter (fun (_S4) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 254 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 254 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1825 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq_elem) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq_elem_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 256 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 256 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1847 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 256 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 256 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1865 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_omit_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_bound) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_predicate_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 258 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 258 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1889 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwith "MINUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 260 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1909 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 260 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1927 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMICOLON _rnglr_val -> [_rnglr_val] | a -> failwith "SEMICOLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 262 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1947 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 262 "Parser.fsy"
                           false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1965 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PREDICATE _rnglr_val -> [_rnglr_val] | a -> failwith "PREDICATE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 264 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1985 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 264 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 2003 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 267 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_bound) 
# 2023 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_patt) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_prim) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 266 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_bound) 
# 2047 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 269 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_patt) 
# 2067 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 269 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_patt) 
# 2087 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LITERAL _rnglr_val -> [_rnglr_val] | a -> failwith "LITERAL expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 278 "Parser.fsy"
                                            PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2107 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 277 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2127 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_call) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 276 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2147 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_alts) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 275 "Parser.fsy"
                                                _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2171 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SQR_LBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_LBR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_alts) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with SQR_RBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_RBR expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 274 "Parser.fsy"
                                                POpt _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2195 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with QUESTION _rnglr_val -> [_rnglr_val] | a -> failwith "QUESTION expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 273 "Parser.fsy"
                                              POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2217 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 272 "Parser.fsy"
                                              PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2239 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with STAR _rnglr_val -> [_rnglr_val] | a -> failwith "STAR expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 271 "Parser.fsy"
                                              PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 271 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2261 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 280 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 280 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 2281 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_meta_param) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 283 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 282 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2303 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_meta_param) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 282 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 282 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2323 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LESS _rnglr_val -> [_rnglr_val] | a -> failwith "LESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with GREAT _rnglr_val -> [_rnglr_val] | a -> failwith "GREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 285 "Parser.fsy"
                                                                       Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 285 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2347 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 285 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 285 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2365 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_param_opt) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_param_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 289 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 287 "Parser.fsy"
               : '_rnglr_type_call) 
# 2392 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with UIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "UIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 287 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 287 "Parser.fsy"
               : '_rnglr_type_call) 
# 2412 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 294 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 294 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2432 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SHARPLINE _rnglr_val -> [_rnglr_val] | a -> failwith "SHARPLINE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 294 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 294 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2452 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 2470 "Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_access_modifier_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_action_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_alts)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_bar_seq_nlist)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_bound)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_call)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_file)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_foot_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_formal_meta_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_formal_meta_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_ident)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_include_)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_includes_or_options_or_tokens)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_kw)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_param)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_params)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_module_)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_module_header)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_modules)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_no_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_omit_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_open_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_openings)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_block)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_l_value)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_value)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_opts)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_param_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_patt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_predicate_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_prim)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_rule)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_rule_nlist)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_semi_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq_elem)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq_elem_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_start_rule_sign_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_tada_rule)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_tokens_block)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_unnamed_module_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_weight_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
