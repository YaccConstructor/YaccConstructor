
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
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception Parse_error
let parseFile = ref Unchecked.defaultof<_>
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()  
let currentFilename = ref ""
let allPublic = ref false
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []

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

let concatModOpt a b = fst a :: fst b, joinMaps (snd a) (snd b)

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

# 86 "Parser.fs"
type Token =
    | ACTION of Source.t
    | ALL_PUBLIC of Source.t
    | BAR of Source.t
    | COLON of Source.t
    | COMMA of Source.t
    | DGREAT of Source.t
    | DLABEL of Source.t
    | DLESS of Source.t
    | EOF of Source.t
    | EQUAL of Source.t
    | INCLUDE of Source.t
    | LIDENT of Source.t
    | LPAREN of Source.t
    | MINUS of Source.t
    | MODULE of Source.t
    | OPEN of Source.t
    | PARAM of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | PRIVATE of Source.t
    | PUBLIC of Source.t
    | QUESTION of Source.t
    | RPAREN of Source.t
    | SEMICOLON of Source.t
    | SET of Source.t
    | SHARPLINE of Source.t
    | STAR of Source.t
    | START_RULE_SIGN of Source.t
    | STRING of Source.t
    | UIDENT of Source.t

let numToString = function
    | 0 -> "access_modifier_opt"
    | 1 -> "action_opt"
    | 2 -> "alts"
    | 3 -> "bar_seq_nlist"
    | 4 -> "bound"
    | 5 -> "call"
    | 6 -> "file"
    | 7 -> "foot_opt"
    | 8 -> "formal_meta_list"
    | 9 -> "formal_meta_param_opt"
    | 10 -> "ident"
    | 11 -> "include_"
    | 12 -> "includes"
    | 13 -> "lbl_seq"
    | 14 -> "meta_param"
    | 15 -> "meta_param_opt"
    | 16 -> "meta_params"
    | 17 -> "module_"
    | 18 -> "module_header"
    | 19 -> "modules"
    | 20 -> "no_lbl_seq"
    | 21 -> "omit_opt"
    | 22 -> "open_list"
    | 23 -> "openings"
    | 24 -> "option_opt"
    | 25 -> "option_param"
    | 26 -> "option_params"
    | 27 -> "param_list"
    | 28 -> "param_opt"
    | 29 -> "patt"
    | 30 -> "predicate_opt"
    | 31 -> "prim"
    | 32 -> "rule"
    | 33 -> "rule_nlist"
    | 34 -> "semi_opt"
    | 35 -> "seq"
    | 36 -> "seq_elem"
    | 37 -> "seq_elem_list"
    | 38 -> "start_rule_sign_opt"
    | 39 -> "tada_rule"
    | 40 -> "unnamed_module_opt"
    | 41 -> "weight_opt"
    | 42 -> "yard_start_rule"
    | 43 -> "ACTION"
    | 44 -> "ALL_PUBLIC"
    | 45 -> "BAR"
    | 46 -> "COLON"
    | 47 -> "COMMA"
    | 48 -> "DGREAT"
    | 49 -> "DLABEL"
    | 50 -> "DLESS"
    | 51 -> "EOF"
    | 52 -> "EQUAL"
    | 53 -> "INCLUDE"
    | 54 -> "LIDENT"
    | 55 -> "LPAREN"
    | 56 -> "MINUS"
    | 57 -> "MODULE"
    | 58 -> "OPEN"
    | 59 -> "PARAM"
    | 60 -> "PLUS"
    | 61 -> "PREDICATE"
    | 62 -> "PRIVATE"
    | 63 -> "PUBLIC"
    | 64 -> "QUESTION"
    | 65 -> "RPAREN"
    | 66 -> "SEMICOLON"
    | 67 -> "SET"
    | 68 -> "SHARPLINE"
    | 69 -> "STAR"
    | 70 -> "START_RULE_SIGN"
    | 71 -> "STRING"
    | 72 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 43
    | ALL_PUBLIC _ -> 44
    | BAR _ -> 45
    | COLON _ -> 46
    | COMMA _ -> 47
    | DGREAT _ -> 48
    | DLABEL _ -> 49
    | DLESS _ -> 50
    | EOF _ -> 51
    | EQUAL _ -> 52
    | INCLUDE _ -> 53
    | LIDENT _ -> 54
    | LPAREN _ -> 55
    | MINUS _ -> 56
    | MODULE _ -> 57
    | OPEN _ -> 58
    | PARAM _ -> 59
    | PLUS _ -> 60
    | PREDICATE _ -> 61
    | PRIVATE _ -> 62
    | PUBLIC _ -> 63
    | QUESTION _ -> 64
    | RPAREN _ -> 65
    | SEMICOLON _ -> 66
    | SET _ -> 67
    | SHARPLINE _ -> 68
    | STAR _ -> 69
    | START_RULE_SIGN _ -> 70
    | STRING _ -> 71
    | UIDENT _ -> 72

let mutable private cur = 0
let leftSide = [|6; 42; 40; 19; 19; 17; 10; 10; 18; 18; 23; 23; 22; 22; 1; 1; 7; 7; 12; 12; 11; 33; 33; 32; 38; 38; 0; 0; 0; 9; 9; 8; 8; 28; 28; 27; 27; 41; 41; 2; 2; 3; 3; 35; 35; 20; 20; 13; 37; 37; 36; 21; 21; 34; 34; 30; 30; 4; 4; 29; 29; 31; 31; 31; 31; 31; 31; 31; 14; 16; 16; 15; 15; 5; 5; 24; 24; 26; 26; 25; 39; 39|]
let private rules = [|1; 12; 40; 19; 7; 6; 33; 17; 19; 18; 10; 23; 33; 54; 72; 57; 44; 57; 58; 10; 22; 47; 10; 22; 43; 66; 43; 11; 12; 53; 71; 32; 34; 33; 38; 0; 54; 9; 27; 46; 24; 2; 70; 62; 63; 50; 8; 48; 54; 8; 54; 59; 59; 27; 59; 35; 3; 35; 45; 35; 45; 35; 3; 13; 20; 43; 36; 37; 1; 49; 41; 55; 20; 65; 36; 37; 21; 4; 30; 56; 66; 61; 31; 29; 52; 31; 43; 54; 71; 13; 5; 55; 2; 65; 31; 64; 31; 60; 31; 69; 31; 14; 16; 14; 50; 16; 48; 54; 15; 28; 72; 67; 26; 25; 47; 26; 25; 54; 52; 71; 51; 68|]
let private rulesStart = [|0; 5; 6; 7; 7; 9; 13; 14; 15; 16; 18; 21; 21; 21; 24; 25; 25; 27; 27; 27; 29; 31; 31; 34; 42; 43; 43; 43; 44; 45; 48; 48; 50; 51; 52; 52; 54; 54; 55; 55; 57; 58; 60; 63; 64; 65; 66; 69; 74; 76; 76; 79; 80; 80; 81; 81; 82; 82; 83; 86; 87; 88; 89; 90; 91; 94; 96; 98; 100; 101; 103; 104; 107; 107; 110; 111; 113; 113; 116; 117; 120; 121; 122|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 111; 61; 2; 6; 4; 3; 5; 7; 88; 10; 89; 86; 8; 87; 9; 11; 84; 85; 12; 13; 79; 14; 77; 15; 16; 69; 17; 18; 19; 20; 50; 55; 62; 32; 58; 21; 24; 25; 26; 65; 68; 66; 47; 63; 64; 22; 23; 27; 28; 37; 29; 30; 31; 33; 67; 34; 35; 36; 38; 41; 39; 40; 42; 45; 44; 43; 46; 48; 49; 51; 52; 53; 54; 56; 59; 57; 60; 70; 76; 73; 71; 72; 74; 75; 78; 80; 82; 81; 83; 90; 91; 107; 104; 106; 103; 92; 101; 102; 93; 95; 94; 96; 97; 98; 99; 100; 105; 108; 109; 110|]
let private small_gotos =
        [|3; 65536; 393217; 2818050; 65539; 720899; 786436; 3473413; 131075; 720899; 786438; 3473413; 262145; 4653063; 393221; 2097160; 2162697; 2490378; 2621451; 4587532; 458754; 2228237; 4325390; 524292; 2097160; 2162703; 2490378; 4587532; 655363; 16; 4063249; 4128786; 720897; 3538963; 786434; 589844; 3276821; 851970; 1769494; 3866647; 917505; 3014680; 983042; 1572889; 4390938; 1048585; 131099; 851996; 1310749; 1376286; 2293791; 2359328; 2818081; 3211298; 3670051; 1310731; 262180; 327717; 852006; 1900583; 2031656; 2818089; 3211298; 3538986; 3604523; 4653100; 4718637; 1376258; 1966126; 3997743; 1703937; 3407920; 1769480; 327717; 852006; 2031665; 3211298; 3538994; 3604523; 4653100; 4718637; 1835011; 3932211; 4194356; 4522037; 2097154; 2687030; 3866679; 2162689; 3604536; 2228229; 1310777; 1376286; 2359328; 2818081; 3670051; 2293761; 4259898; 2424834; 983099; 3276860; 2490370; 1835069; 3866686; 2686986; 327717; 852006; 917567; 1048640; 2031681; 3211298; 3538994; 3604523; 4653100; 4718637; 2752522; 327717; 852006; 917567; 1048642; 2031681; 3211298; 3538994; 3604523; 4653100; 4718637; 2883587; 3932211; 4194356; 4522037; 2949121; 3145795; 3080201; 131140; 851996; 1310749; 1376286; 2293791; 2359328; 2818081; 3211298; 3670051; 3145729; 4259909; 3276802; 196678; 2949191; 3407880; 851996; 1310749; 1376286; 2293832; 2359328; 2818081; 3211298; 3670051; 3473410; 196681; 2949191; 3604484; 1376286; 2359370; 2424907; 3670051; 3670020; 1376286; 2359370; 2424908; 3670051; 3866626; 65613; 2818050; 4259843; 3932211; 4194356; 4522037; 4325378; 983099; 3276860; 4521987; 1638478; 1704015; 3539024; 4587521; 3080273; 4653059; 1638478; 1704018; 3539024; 4784129; 3407955; 4849665; 4653140; 5046274; 1769557; 3866647; 5177346; 524374; 3539031; 5242881; 3145816; 5373954; 524377; 3539031; 5832709; 1114202; 1179739; 1245276; 2883677; 3735646; 5898245; 1114202; 1179739; 1245279; 2883677; 3735646; 5963779; 655456; 3539041; 4718690; 6029314; 1507427; 3801188; 6094852; 2097160; 2162789; 2490378; 4587532; 6225923; 655462; 3539041; 4718690; 6291458; 1441895; 3080296; 6422531; 655465; 3539041; 4718690; 6488066; 1441898; 3080296; 6815745; 3735659; 7012354; 458860; 4325485; 7143425; 2818158|]
let gotos = Array.zeroCreate 112
for i = 0 to 111 do
        gotos.[i] <- Array.zeroCreate 73
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
let private lists_reduces = [|[|0,1|]; [|19,1|]; [|19,2|]; [|20,2|]; [|0,2|]; [|22,1|]; [|22,2|]; [|22,3|]; [|23,8|]; [|43,1|]; [|44,1|]; [|50,2|]; [|50,3|]; [|55,1|]; [|63,1|]; [|62,1|]; [|58,3|]; [|66,2|]; [|65,2|]; [|67,2|]; [|47,5|]; [|73,1|]; [|73,2|]; [|73,3|]; [|33,1|]; [|70,1|]; [|69,2|]; [|68,1|]; [|71,3|]; [|64,3|]; [|40,1|]; [|39,2|]; [|41,2|]; [|42,3|]; [|46,1|]; [|48,1|]; [|48,2|]; [|51,1|]; [|46,2|]; [|46,3|]; [|14,1|]; [|45,1|]; [|61,1|]; [|74,1|]; [|57,1|]; [|60,1|]; [|37,1|]; [|59,1|]; [|78,1|]; [|77,3|]; [|79,3|]; [|75,2|]; [|35,1|]; [|35,2|]; [|29,3|]; [|32,1|]; [|31,2|]; [|27,1|]; [|28,1|]; [|24,1|]; [|53,1|]; [|2,1|]; [|0,3|]; [|4,1|]; [|5,2|]; [|5,3|]; [|5,4|]; [|10,2|]; [|10,3|]; [|13,2|]; [|13,3|]; [|6,1|]; [|7,1|]; [|4,2|]; [|9,2|]; [|8,1|]; [|0,4|]; [|0,5|]; [|16,2|]|]
let private small_reduces =
        [|65537; 3342336; 131080; 2883585; 3342337; 3538945; 3735553; 4063233; 4128769; 4325377; 4587521; 196616; 2883586; 3342338; 3538946; 3735554; 4063234; 4128770; 4325378; 4587522; 327689; 2883587; 3342339; 3473411; 3538947; 3735555; 4063235; 4128771; 4325379; 4587523; 393217; 3342340; 458756; 2883589; 3342341; 3735557; 4325381; 524292; 2883590; 3342342; 3735558; 4325382; 589828; 2883591; 3342343; 3735559; 4325383; 1114120; 2883592; 3342344; 3538952; 3735560; 4063240; 4128776; 4325384; 4587528; 1179658; 2883593; 2949129; 3342345; 3538953; 3735561; 4063241; 4128777; 4259849; 4325385; 4587529; 1245194; 2883594; 2949130; 3342346; 3538954; 3735562; 4063242; 4128778; 4259850; 4325386; 4587530; 1376272; 2818059; 2883595; 2949131; 3211275; 3342347; 3538955; 3604491; 3670027; 3735563; 4063243; 4128779; 4259851; 4325387; 4587531; 4653067; 4718603; 1441808; 2818060; 2883596; 2949132; 3211276; 3342348; 3538956; 3604492; 3670028; 3735564; 4063244; 4128780; 4259852; 4325388; 4587532; 4653068; 4718604; 1507344; 2818061; 2883597; 2949133; 3211277; 3342349; 3538957; 3604493; 3670029; 3735565; 4063245; 4128781; 4259853; 4325389; 4587533; 4653069; 4718605; 1572885; 2818062; 2883598; 2949134; 3145742; 3211278; 3342350; 3538958; 3604494; 3670030; 3735566; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4521998; 4587534; 4653070; 4718606; 1638421; 2818063; 2883599; 2949135; 3145743; 3211279; 3342351; 3538959; 3604495; 3670031; 3735567; 3932175; 3997711; 4063247; 4128783; 4194319; 4259855; 4325391; 4521999; 4587535; 4653071; 4718607; 1835025; 2818064; 2883600; 2949136; 3211280; 3342352; 3538960; 3604496; 3670032; 3735568; 3997712; 4063248; 4128784; 4259856; 4325392; 4587536; 4653072; 4718608; 1900565; 2818065; 2883601; 2949137; 3145745; 3211281; 3342353; 3538961; 3604497; 3670033; 3735569; 3932177; 3997713; 4063249; 4128785; 4194321; 4259857; 4325393; 4522001; 4587537; 4653073; 4718609; 1966101; 2818066; 2883602; 2949138; 3145746; 3211282; 3342354; 3538962; 3604498; 3670034; 3735570; 3932178; 3997714; 4063250; 4128786; 4194322; 4259858; 4325394; 4522002; 4587538; 4653074; 4718610; 2031637; 2818067; 2883603; 2949139; 3145747; 3211283; 3342355; 3538963; 3604499; 3670035; 3735571; 3932179; 3997715; 4063251; 4128787; 4194323; 4259859; 4325395; 4522003; 4587539; 4653075; 4718611; 2359317; 2818068; 2883604; 2949140; 3145748; 3211284; 3342356; 3538964; 3604500; 3670036; 3735572; 3932180; 3997716; 4063252; 4128788; 4194324; 4259860; 4325396; 4522004; 4587540; 4653076; 4718612; 2424853; 2818069; 2883605; 2949141; 3145749; 3211285; 3342357; 3538965; 3604501; 3670037; 3735573; 3932181; 3997717; 4063253; 4128789; 4194325; 4259861; 4325397; 4522005; 4587541; 4653077; 4718613; 2490389; 2818070; 2883606; 2949142; 3145750; 3211286; 3342358; 3538966; 3604502; 3670038; 3735574; 3932182; 3997718; 4063254; 4128790; 4194326; 4259862; 4325398; 4522006; 4587542; 4653078; 4718614; 2555925; 2818071; 2883607; 2949143; 3145751; 3211287; 3342359; 3538967; 3604503; 3670039; 3735575; 3932183; 3997719; 4063255; 4128791; 4194327; 4259863; 4325399; 4522007; 4587543; 4653079; 4718615; 2621461; 2818072; 2883608; 2949144; 3145752; 3211288; 3342360; 3538968; 3604504; 3670040; 3735576; 3932184; 3997720; 4063256; 4128792; 4194328; 4259864; 4325400; 4522008; 4587544; 4653080; 4718616; 2752513; 3145753; 2818049; 3145754; 2883590; 3145755; 3211291; 3538971; 3604507; 4653083; 4718619; 3014678; 2818076; 2883612; 2949148; 3145756; 3211292; 3342364; 3538972; 3604508; 3670044; 3735580; 3866652; 3932188; 3997724; 4063260; 4128796; 4194332; 4259868; 4325404; 4522012; 4587548; 4653084; 4718620; 3211285; 2818077; 2883613; 2949149; 3145757; 3211293; 3342365; 3538973; 3604509; 3670045; 3735581; 3932189; 3997725; 4063261; 4128797; 4194333; 4259869; 4325405; 4522013; 4587549; 4653085; 4718621; 3276809; 2883614; 3342366; 3538974; 3735582; 4063262; 4128798; 4259870; 4325406; 4587550; 3342345; 2883615; 3342367; 3538975; 3735583; 4063263; 4128799; 4259871; 4325407; 4587551; 3473417; 2883616; 3342368; 3538976; 3735584; 4063264; 4128800; 4259872; 4325408; 4587552; 3538953; 2883617; 3342369; 3538977; 3735585; 4063265; 4128801; 4259873; 4325409; 4587553; 3604490; 2883618; 2949154; 3342370; 3538978; 3735586; 4063266; 4128802; 4259874; 4325410; 4587554; 3670027; 2818083; 2883619; 2949155; 3342371; 3538979; 3735587; 4063267; 4128803; 4259875; 4325411; 4587555; 3735563; 2818084; 2883620; 2949156; 3342372; 3538980; 3735588; 4063268; 4128804; 4259876; 4325412; 4587556; 3801094; 2818085; 3211301; 3538981; 3604517; 4653093; 4718629; 3866634; 2883622; 2949158; 3342374; 3538982; 3735590; 4063270; 4128806; 4259878; 4325414; 4587558; 3932170; 2883623; 2949159; 3342375; 3538983; 3735591; 4063271; 4128807; 4259879; 4325415; 4587559; 3997707; 2883624; 2949160; 3342376; 3473448; 3538984; 3735592; 4063272; 4128808; 4259880; 4325416; 4587560; 4063242; 2883625; 2949161; 3342377; 3538985; 3735593; 4063273; 4128809; 4259881; 4325417; 4587561; 4128789; 2818090; 2883626; 2949162; 3145770; 3211306; 3342378; 3538986; 3604522; 3670058; 3735594; 3932202; 3997738; 4063274; 4128810; 4194346; 4259882; 4325418; 4522026; 4587562; 4653098; 4718634; 4194325; 2818091; 2883627; 2949163; 3145771; 3211307; 3342379; 3538987; 3604523; 3670059; 3735595; 3932203; 3997739; 4063275; 4128811; 4194347; 4259883; 4325419; 4522027; 4587563; 4653099; 4718635; 4259857; 2818092; 2883628; 2949164; 3211308; 3342380; 3538988; 3604524; 3670060; 3735596; 3997740; 4063276; 4128812; 4259884; 4325420; 4587564; 4653100; 4718636; 4325397; 2818069; 2883605; 2949141; 3211285; 3342357; 3407917; 3538965; 3604501; 3670037; 3735573; 3932181; 3997717; 4063253; 4128789; 4194325; 4259861; 4325397; 4522005; 4587541; 4653077; 4718613; 4390913; 3604526; 4456449; 3407919; 4587527; 2818096; 3211312; 3538992; 3604528; 3670064; 4653104; 4718640; 4718599; 2818097; 3211313; 3538993; 3604529; 3670065; 4653105; 4718641; 4915208; 2818098; 3080242; 3211314; 3538994; 3604530; 3670066; 4653106; 4718642; 4980743; 2818099; 3211315; 3538995; 3604531; 3670067; 4653107; 4718643; 5046273; 3014708; 5111809; 3014709; 5308418; 3014710; 3866678; 5373953; 3145783; 5439489; 3145784; 5505025; 3539001; 5570561; 3539002; 5636099; 3539003; 4063291; 4128827; 5701640; 2883644; 3342396; 3539004; 3735612; 4063292; 4128828; 4325436; 4587580; 5767172; 2883645; 3342397; 3735613; 4325437; 5832705; 3342398; 5898242; 3342399; 4325439; 6029316; 2883648; 3342400; 3735616; 4325440; 6094852; 2883649; 3342401; 3735617; 4325441; 6160388; 2883650; 3342402; 3735618; 4325442; 6291464; 2883651; 3342403; 3539011; 3735619; 4063299; 4128835; 4325443; 4587587; 6357000; 2883652; 3342404; 3539012; 3735620; 4063300; 4128836; 4325444; 4587588; 6488072; 2883653; 3342405; 3539013; 3735621; 4063301; 4128837; 4325445; 4587589; 6553608; 2883654; 3342406; 3539014; 3735622; 4063302; 4128838; 4325446; 4587590; 6619146; 2883655; 3080263; 3342407; 3539015; 3735623; 3801159; 4063303; 4128839; 4325447; 4587591; 6684682; 2883656; 3080264; 3342408; 3539016; 3735624; 3801160; 4063304; 4128840; 4325448; 4587592; 6750210; 3342409; 4325449; 6881282; 3539018; 4718666; 6946818; 3539019; 4718667; 7012353; 3342412; 7077889; 3342413; 7208961; 3342414|]
let reduces = Array.zeroCreate 112
for i = 0 to 111 do
        reduces.[i] <- Array.zeroCreate 73
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
let private lists_zeroReduces = [|[|15|]; [|15; 1; 0|]; [|18|]; [|21; 2|]; [|25|]; [|54|]; [|21|]; [|26|]; [|30|]; [|36|]; [|76|]; [|52|]; [|56|]; [|38|]; [|72|]; [|34|]; [|52; 49|]; [|49|]; [|3|]; [|11|]; [|12|]; [|17|]|]
let private small_zeroReduces =
        [|9; 2883584; 3342337; 3473408; 3538944; 3735552; 4063232; 4128768; 4325376; 4587520; 65544; 2883586; 3342338; 3538946; 3735554; 4063234; 4128770; 4325378; 4587522; 131080; 2883586; 3342338; 3538946; 3735554; 4063234; 4128770; 4325378; 4587522; 393223; 2883587; 3342339; 3538948; 3735555; 4063236; 4128772; 4325379; 458760; 2883589; 3342341; 3538949; 3735557; 4063237; 4128773; 4325381; 4587525; 524295; 2883590; 3342342; 3538948; 3735558; 4063236; 4128772; 4325382; 655361; 3538951; 786434; 3014664; 3866632; 851969; 3014665; 983047; 2818058; 3211274; 3538954; 3604490; 3670026; 4653066; 4718602; 1048582; 2818059; 3211275; 3538955; 3604491; 4653067; 4718603; 1376272; 2818060; 2883596; 2949132; 3211276; 3342348; 3538956; 3604492; 3670028; 3735564; 4063244; 4128780; 4259852; 4325388; 4587532; 4653068; 4718604; 2097153; 3604493; 2228230; 2818059; 3211275; 3538955; 3604491; 4653067; 4718603; 2424854; 2818062; 2883598; 2949134; 3145742; 3211278; 3342350; 3538958; 3604494; 3670030; 3735566; 3866638; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4521998; 4587534; 4653070; 4718606; 2490389; 2818063; 2883599; 2949135; 3145743; 3211279; 3342351; 3538959; 3604495; 3670031; 3735567; 3932175; 3997711; 4063247; 4128783; 4194319; 4259855; 4325391; 4521999; 4587535; 4653071; 4718607; 3080198; 2818059; 3211275; 3538955; 3604491; 4653067; 4718603; 3407878; 2818059; 3211275; 3538955; 3604491; 4653067; 4718603; 3604495; 2818064; 2883601; 2949137; 3211275; 3342353; 3538960; 3604491; 3735569; 4063249; 4128785; 4259857; 4325393; 4587537; 4653067; 4718603; 3670031; 2818064; 2883601; 2949137; 3211275; 3342353; 3538960; 3604491; 3735569; 4063249; 4128785; 4259857; 4325393; 4587537; 4653067; 4718603; 3866634; 2883584; 2949120; 3342336; 3538944; 3735552; 4063232; 4128768; 4259840; 4325376; 4587520; 4325397; 2818062; 2883598; 2949134; 3211278; 3342350; 3538958; 3604494; 3670030; 3735566; 3866638; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4521998; 4587534; 4653070; 4718606; 5046273; 3014665; 5832706; 3342354; 4325394; 5898242; 3342354; 4325394; 6029320; 2883603; 3342355; 3538963; 3735571; 4063251; 4128787; 4325395; 4587539; 6094855; 2883590; 3342342; 3538948; 3735558; 4063236; 4128772; 4325382; 6291464; 2883604; 3342356; 3538964; 3735572; 4063252; 4128788; 4325396; 4587540; 6488072; 2883604; 3342356; 3538964; 3735572; 4063252; 4128788; 4325396; 4587540; 7012353; 3342357|]
let zeroReduces = Array.zeroCreate 112
for i = 0 to 111 do
        zeroReduces.[i] <- Array.zeroCreate 73
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
let private small_acc = [111; 0]
let private accStates = Array.zeroCreate 112
for i = 0 to 111 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 51
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(76, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(76, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 113 "Parser.fsy"
                                
                                {
                                    info = { fileName = !currentFilename }
                                    head = _S1
                                    grammar = fst _S2 @ fst _S3 @ fst _S4
                                    foot = _S5
                                    options = (snd _S4) |> joinMaps (snd _S3) |> joinMaps (snd _S2)
                                }
                              
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 108 "Parser.fsy"
               : '_rnglr_type_file) 
# 334 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 108 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 344 "Parser.fs"
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
                
# 124 "Parser.fsy"
                     
                        match fst _S1 with
                        | [] -> [], Map.empty
                        | x ->  defaultModules x, snd _S1
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 123 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 368 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 131 "Parser.fsy"
                          [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 130 "Parser.fsy"
               : '_rnglr_type_modules) 
# 386 "Parser.fs"
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
                  
# 130 "Parser.fsy"
                                              concatModOpt _S1 _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 130 "Parser.fsy"
               : '_rnglr_type_modules) 
# 408 "Parser.fs"
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
                      
# 134 "Parser.fsy"
                           
                              {
                                  allPublic = _S1
                                  name = Some _S2
                                  openings = _S3
                                  rules = fst _S4
                              }
                              , snd _S4
                          
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 133 "Parser.fsy"
               : '_rnglr_type_module_) 
# 442 "Parser.fs"
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
                
# 144 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_ident) 
# 462 "Parser.fs"
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
                
# 144 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_ident) 
# 482 "Parser.fs"
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
                
# 151 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 502 "Parser.fs"
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
                  
# 147 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 527 "Parser.fs"
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
                    
# 153 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_openings) 
# 551 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 153 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_openings) 
# 569 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 155 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 587 "Parser.fs"
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
                    
# 155 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 611 "Parser.fs"
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
                
# 157 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 157 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 631 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 157 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 157 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 649 "Parser.fs"
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
                  
# 159 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 159 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 671 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 159 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 159 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 689 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 161 "Parser.fsy"
                                                                                               [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_includes) 
# 707 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_include_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 161 "Parser.fsy"
                                                 fst _S1 @ fst _S2, joinMaps (snd _S1) (snd _S2) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_includes) 
# 729 "Parser.fs"
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
                  
# 165 "Parser.fsy"
                       
                          let def = parseRules _S2.text in
                          if def.grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          def.grammar, def.options
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 163 "Parser.fsy"
               : '_rnglr_type_include_) 
# 756 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 175 "Parser.fsy"
                      [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 774 "Parser.fs"
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
                    
# 174 "Parser.fsy"
                            concatModOpt _S1 _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 798 "Parser.fs"
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
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_option_opt) 
                         |> List.iter (fun (_S7) -> 
                          ((unbox _rnglr_children.[7]) : '_rnglr_type_alts) 
                           |> List.iter (fun (_S8) -> 
                            _rnglr_cycle_res := (
                              
# 179 "Parser.fsy"
                                      
                                      let newRule =
                                          {
                                              Rule.isStart = _S1
                                              Rule.isPublic = _S2
                                              Rule.name = _S3
                                              Rule.metaArgs = getList _S4
                                              Rule.body = _S8
                                              Rule.args = _S5
                                          }
                                      in
                                      match _S7 with
                                      | Some m -> newRule, Map.empty.Add (newRule, m)
                                      | None -> newRule, Map.empty
                                     
                                )::!_rnglr_cycle_res ) ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 178 "Parser.fsy"
               : '_rnglr_type_rule) 
# 846 "Parser.fs"
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
                
# 195 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 866 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 195 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 884 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 197 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 902 "Parser.fs"
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
                
# 197 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 922 "Parser.fs"
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
                
# 197 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 942 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLESS _rnglr_val -> [_rnglr_val] | a -> failwith "DLESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with DGREAT _rnglr_val -> [_rnglr_val] | a -> failwith "DGREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 199 "Parser.fsy"
                                                                                     Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 966 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 199 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 984 "Parser.fs"
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
                  
# 202 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1006 "Parser.fs"
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
                
# 201 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1026 "Parser.fs"
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
                
# 204 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1046 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 204 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1064 "Parser.fs"
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
                  
# 206 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1086 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 206 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1104 "Parser.fs"
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
                
# 208 "Parser.fsy"
                                               Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1124 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 208 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1142 "Parser.fs"
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
                  
# 210 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1164 "Parser.fs"
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
                
# 210 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1184 "Parser.fs"
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
                  
# 213 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1206 "Parser.fs"
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
                    
# 212 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1230 "Parser.fs"
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
                
# 215 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 215 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1250 "Parser.fs"
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
                
# 215 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 215 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1270 "Parser.fs"
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
                
# 218 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 217 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1290 "Parser.fs"
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
                    
# 217 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 217 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1314 "Parser.fs"
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
                        
# 220 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 220 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1342 "Parser.fs"
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
                  
# 222 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 222 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1364 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 222 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 222 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1382 "Parser.fs"
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
                    
# 224 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 224 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1406 "Parser.fs"
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
                
# 226 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 226 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1426 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 226 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 226 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1444 "Parser.fs"
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
                
# 228 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 228 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1464 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 228 "Parser.fsy"
                           false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 228 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1482 "Parser.fs"
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
                
# 230 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1502 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 230 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1520 "Parser.fs"
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
                
# 233 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1540 "Parser.fs"
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
                    
# 232 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1564 "Parser.fs"
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
                
# 235 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1584 "Parser.fs"
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
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1604 "Parser.fs"
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
                
# 243 "Parser.fsy"
                                           PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1624 "Parser.fs"
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
                
# 242 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1644 "Parser.fs"
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
                
# 241 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1664 "Parser.fs"
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
                    
# 240 "Parser.fsy"
                                               _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1688 "Parser.fs"
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
                  
# 239 "Parser.fsy"
                                             POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1710 "Parser.fs"
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
                  
# 238 "Parser.fsy"
                                             PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1732 "Parser.fs"
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
                  
# 237 "Parser.fsy"
                                             PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1754 "Parser.fs"
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
                
# 245 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1774 "Parser.fs"
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
                  
# 248 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1796 "Parser.fs"
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
                
# 247 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1816 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLESS _rnglr_val -> [_rnglr_val] | a -> failwith "DLESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with DGREAT _rnglr_val -> [_rnglr_val] | a -> failwith "DGREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 250 "Parser.fsy"
                                                                         Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1840 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 250 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1858 "Parser.fs"
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
                    
# 254 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_call) 
# 1885 "Parser.fs"
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
                
# 252 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_call) 
# 1905 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SET _rnglr_val -> [_rnglr_val] | a -> failwith "SET expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_option_params) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 259 "Parser.fsy"
                                                             Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1927 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 259 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1945 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_param) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_option_params) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 262 "Parser.fsy"
                                                                     (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1969 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_param) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 261 "Parser.fsy"
                                              Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1989 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 264 "Parser.fsy"
                                                        Source.toString _S1, Source.toString _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 2013 "Parser.fs"
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
                
# 266 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2033 "Parser.fs"
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
                
# 266 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2053 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_includes)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_param)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_params)   ) |> List.concat));
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
