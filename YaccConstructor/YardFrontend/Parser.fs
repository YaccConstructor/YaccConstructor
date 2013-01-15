
# 2 "Parser.fs"
module Yard.Frontends.YardFrontend.GrammarParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 2 "Parser.fsy"

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
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()  
let currentFilename = ref ""
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let makeNewSeq seq (lbl:Source.t) = 
    match seq with 
    | PSeq(els,ac,_) ->PSeq (els,ac,Some {label=lbl.text; weight=None})
    | x -> x

let missing name = System.Console.WriteLine("Missing " + name)
let createSeqElem bnd omitted r check =
    { binding = bnd; omit = omitted; rule = r; checker = check }

let commonPublicModifier = ref false

let parseRules (filename:string) : Definition.t<Source.t, Source.t> =
    let ext = filename.Substring(filename.LastIndexOf(".") + 1)
    let frontend = FrontendsManager.GetByExtension ext
    let userDefs =
        let args = (!currentFilename).Trim().Split('%') in
        if args.Length = 2
        then args.[1]
        else ""
    let sameDirFilename = System.IO.Path.Combine(System.IO.Path.GetDirectoryName !currentFilename, filename) in

    match FrontendsManager.GetByExtension ext with
    | Some(frontend) -> 
        frontend.Value.ParseGrammar (sameDirFilename + "%" + userDefs)
    | None -> 
        failwith (sprintf "Not supported extension %s in file %s" ext filename )

# 79 "Parser.fs"
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
    | PATTERN of Source.t
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
    | 2 -> "allpublic_opt"
    | 3 -> "alts"
    | 4 -> "bar_seq_nlist"
    | 5 -> "bound"
    | 6 -> "call"
    | 7 -> "file"
    | 8 -> "foot_opt"
    | 9 -> "formal_meta_list"
    | 10 -> "formal_meta_param_opt"
    | 11 -> "include_"
    | 12 -> "includes"
    | 13 -> "lbl_seq"
    | 14 -> "meta_param"
    | 15 -> "meta_param_opt"
    | 16 -> "meta_params"
    | 17 -> "module_"
    | 18 -> "modules"
    | 19 -> "no_lbl_seq"
    | 20 -> "omit_opt"
    | 21 -> "open_list"
    | 22 -> "openings"
    | 23 -> "option_opt"
    | 24 -> "option_param"
    | 25 -> "option_params"
    | 26 -> "param_list"
    | 27 -> "param_opt"
    | 28 -> "patt"
    | 29 -> "predicate_opt"
    | 30 -> "prim"
    | 31 -> "rule"
    | 32 -> "rule_nlist"
    | 33 -> "semi_opt"
    | 34 -> "seq"
    | 35 -> "seq_elem"
    | 36 -> "seq_elem_list"
    | 37 -> "start_rule_sign_opt"
    | 38 -> "tada_rule"
    | 39 -> "unnamed_module_opt"
    | 40 -> "yard_start_rule"
    | 41 -> "ACTION"
    | 42 -> "ALL_PUBLIC"
    | 43 -> "BAR"
    | 44 -> "COLON"
    | 45 -> "COMMA"
    | 46 -> "DGREAT"
    | 47 -> "DLABEL"
    | 48 -> "DLESS"
    | 49 -> "EOF"
    | 50 -> "EQUAL"
    | 51 -> "INCLUDE"
    | 52 -> "LIDENT"
    | 53 -> "LPAREN"
    | 54 -> "MINUS"
    | 55 -> "MODULE"
    | 56 -> "OPEN"
    | 57 -> "PARAM"
    | 58 -> "PATTERN"
    | 59 -> "PLUS"
    | 60 -> "PREDICATE"
    | 61 -> "PRIVATE"
    | 62 -> "PUBLIC"
    | 63 -> "QUESTION"
    | 64 -> "RPAREN"
    | 65 -> "SEMICOLON"
    | 66 -> "SET"
    | 67 -> "SHARPLINE"
    | 68 -> "STAR"
    | 69 -> "START_RULE_SIGN"
    | 70 -> "STRING"
    | 71 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 41
    | ALL_PUBLIC _ -> 42
    | BAR _ -> 43
    | COLON _ -> 44
    | COMMA _ -> 45
    | DGREAT _ -> 46
    | DLABEL _ -> 47
    | DLESS _ -> 48
    | EOF _ -> 49
    | EQUAL _ -> 50
    | INCLUDE _ -> 51
    | LIDENT _ -> 52
    | LPAREN _ -> 53
    | MINUS _ -> 54
    | MODULE _ -> 55
    | OPEN _ -> 56
    | PARAM _ -> 57
    | PATTERN _ -> 58
    | PLUS _ -> 59
    | PREDICATE _ -> 60
    | PRIVATE _ -> 61
    | PUBLIC _ -> 62
    | QUESTION _ -> 63
    | RPAREN _ -> 64
    | SEMICOLON _ -> 65
    | SET _ -> 66
    | SHARPLINE _ -> 67
    | STAR _ -> 68
    | START_RULE_SIGN _ -> 69
    | STRING _ -> 70
    | UIDENT _ -> 71

let mutable private cur = 0
let leftSide = [|7; 40; 39; 18; 18; 17; 2; 2; 22; 22; 21; 21; 1; 1; 8; 8; 12; 12; 11; 32; 32; 31; 37; 37; 0; 0; 0; 10; 10; 9; 9; 27; 27; 26; 26; 3; 3; 4; 4; 34; 34; 19; 19; 13; 36; 36; 35; 20; 20; 33; 33; 29; 29; 5; 5; 28; 28; 30; 30; 30; 30; 30; 30; 30; 14; 16; 16; 15; 15; 6; 6; 23; 23; 25; 25; 24; 38; 38|]
let private rules = [|1; 12; 39; 18; 8; 7; 32; 17; 18; 2; 55; 52; 22; 32; 42; 56; 52; 21; 45; 52; 21; 41; 65; 41; 11; 12; 51; 70; 31; 33; 32; 37; 0; 52; 10; 26; 44; 23; 3; 69; 61; 62; 48; 9; 46; 52; 9; 52; 57; 57; 26; 34; 4; 34; 43; 34; 43; 34; 4; 13; 19; 41; 35; 36; 1; 47; 53; 19; 64; 35; 36; 20; 5; 29; 54; 65; 60; 30; 28; 50; 30; 58; 52; 70; 13; 6; 53; 3; 64; 30; 63; 30; 59; 30; 68; 30; 14; 16; 14; 48; 16; 46; 52; 15; 27; 71; 66; 25; 24; 45; 25; 24; 52; 50; 70; 49; 67|]
let private rulesStart = [|0; 5; 6; 7; 7; 9; 14; 14; 15; 18; 18; 18; 21; 22; 22; 24; 24; 24; 26; 28; 28; 31; 39; 40; 40; 40; 41; 42; 45; 45; 47; 48; 49; 49; 51; 51; 53; 54; 56; 59; 60; 61; 62; 65; 69; 71; 71; 74; 75; 75; 76; 76; 77; 77; 78; 81; 82; 83; 84; 85; 86; 89; 91; 93; 95; 96; 98; 99; 102; 102; 105; 106; 108; 108; 111; 112; 115; 116; 117|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 106; 60; 2; 6; 4; 3; 5; 7; 86; 10; 87; 84; 8; 85; 9; 11; 82; 83; 12; 13; 77; 14; 75; 15; 16; 67; 17; 18; 19; 20; 49; 54; 61; 32; 57; 21; 24; 25; 26; 64; 65; 46; 66; 62; 63; 22; 23; 27; 28; 36; 29; 30; 31; 33; 34; 35; 37; 40; 38; 39; 41; 44; 43; 42; 45; 47; 48; 50; 51; 52; 53; 55; 58; 56; 59; 68; 74; 71; 69; 70; 72; 73; 76; 78; 80; 79; 81; 88; 99; 102; 101; 89; 90; 91; 93; 92; 94; 95; 96; 97; 98; 100; 103; 104; 105|]
let private small_gotos =
        [|3; 65536; 458753; 2686978; 65539; 720899; 786436; 3342341; 131075; 720899; 786438; 3342341; 262145; 4587527; 393221; 2031624; 2097161; 2424842; 2555915; 4521996; 458754; 2162701; 4259854; 524292; 2031624; 2097167; 2424842; 4521996; 655363; 16; 3997713; 4063250; 720897; 3407891; 786434; 655380; 3145749; 851970; 1703958; 3735575; 917505; 2883608; 983042; 1507353; 4325402; 1048585; 196635; 851996; 1245213; 1310750; 2228255; 2293792; 2687009; 3080226; 3538979; 1310731; 327716; 393253; 852006; 1835047; 1966120; 3080226; 3407913; 3473450; 3801131; 4587564; 4653101; 1376258; 1900590; 3932207; 1703937; 3276848; 1769480; 393253; 852006; 1966129; 3080226; 3407922; 3473450; 4587564; 4653101; 1835011; 3866675; 4128820; 4456501; 2097153; 3473462; 2162693; 1245239; 1310750; 2293792; 2687009; 3538979; 2228225; 4194360; 2359298; 983097; 3145786; 2424834; 1769531; 3735612; 2621450; 393253; 852006; 917565; 1048638; 1966143; 3080226; 3407922; 3473450; 4587564; 4653101; 2686986; 393253; 852006; 917565; 1048640; 1966143; 3080226; 3407922; 3473450; 4587564; 4653101; 2818051; 3866675; 4128820; 4456501; 2883585; 3014721; 3014665; 196674; 851996; 1245213; 1310750; 2228255; 2293792; 2687009; 3080226; 3538979; 3080193; 4194371; 3211266; 262212; 2818117; 3342344; 851996; 1245213; 1310750; 2228294; 2293792; 2687009; 3080226; 3538979; 3407874; 262215; 2818117; 3538948; 1310750; 2293832; 2359369; 3538979; 3604484; 1310750; 2293832; 2359370; 3538979; 3801090; 65611; 2686978; 4194307; 3866675; 4128820; 4456501; 4259842; 983097; 3145786; 4390915; 1572940; 1638477; 3407950; 4456449; 2949199; 4521987; 1572940; 1638480; 3407950; 4653057; 3276881; 4718593; 4587602; 4915202; 1704019; 3735575; 5046274; 589908; 3407957; 5111809; 3014742; 5242882; 589911; 3407957; 5701636; 131160; 1114201; 1179738; 2752603; 5767169; 3604572; 5832705; 3407965; 5898242; 1441886; 3670111; 5963780; 2031624; 2097248; 2424842; 4521996; 6094849; 3407969; 6160386; 1376354; 2949219; 6291457; 3407972; 6356994; 1376357; 2949219; 6488068; 131160; 1114201; 1179750; 2752603; 6684674; 524391; 4259944; 6815745; 2687081|]
let gotos = Array.zeroCreate 107
for i = 0 to 106 do
        gotos.[i] <- Array.zeroCreate 72
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
let private lists_reduces = [|[|0,1|]; [|17,1|]; [|17,2|]; [|18,2|]; [|0,2|]; [|20,1|]; [|20,2|]; [|20,3|]; [|21,8|]; [|39,1|]; [|40,1|]; [|46,2|]; [|46,3|]; [|51,1|]; [|59,1|]; [|58,1|]; [|54,3|]; [|62,2|]; [|61,2|]; [|63,2|]; [|43,4|]; [|69,1|]; [|69,2|]; [|69,3|]; [|31,1|]; [|66,1|]; [|65,2|]; [|64,1|]; [|67,3|]; [|60,3|]; [|36,1|]; [|35,2|]; [|37,2|]; [|38,3|]; [|42,1|]; [|44,1|]; [|44,2|]; [|47,1|]; [|42,2|]; [|42,3|]; [|12,1|]; [|41,1|]; [|57,1|]; [|70,1|]; [|53,1|]; [|56,1|]; [|55,1|]; [|74,1|]; [|73,3|]; [|75,3|]; [|71,2|]; [|33,1|]; [|33,2|]; [|27,3|]; [|30,1|]; [|29,2|]; [|25,1|]; [|26,1|]; [|22,1|]; [|49,1|]; [|2,1|]; [|0,3|]; [|5,3|]; [|5,4|]; [|5,5|]; [|8,2|]; [|8,3|]; [|11,2|]; [|11,3|]; [|4,1|]; [|4,2|]; [|7,1|]; [|0,4|]; [|0,5|]; [|14,2|]|]
let private small_reduces =
        [|65537; 3211264; 131080; 2752513; 3211265; 3407873; 3604481; 3997697; 4063233; 4259841; 4521985; 196616; 2752514; 3211266; 3407874; 3604482; 3997698; 4063234; 4259842; 4521986; 327689; 2752515; 3211267; 3342339; 3407875; 3604483; 3997699; 4063235; 4259843; 4521987; 393217; 3211268; 458756; 2752517; 3211269; 3604485; 4259845; 524292; 2752518; 3211270; 3604486; 4259846; 589828; 2752519; 3211271; 3604487; 4259847; 1114120; 2752520; 3211272; 3407880; 3604488; 3997704; 4063240; 4259848; 4521992; 1179658; 2752521; 2818057; 3211273; 3407881; 3604489; 3997705; 4063241; 4194313; 4259849; 4521993; 1245194; 2752522; 2818058; 3211274; 3407882; 3604490; 3997706; 4063242; 4194314; 4259850; 4521994; 1376273; 2686987; 2752523; 2818059; 3080203; 3211275; 3407883; 3473419; 3538955; 3604491; 3801099; 3997707; 4063243; 4194315; 4259851; 4521995; 4587531; 4653067; 1441809; 2686988; 2752524; 2818060; 3080204; 3211276; 3407884; 3473420; 3538956; 3604492; 3801100; 3997708; 4063244; 4194316; 4259852; 4521996; 4587532; 4653068; 1507345; 2686989; 2752525; 2818061; 3080205; 3211277; 3407885; 3473421; 3538957; 3604493; 3801101; 3997709; 4063245; 4194317; 4259853; 4521997; 4587533; 4653069; 1572886; 2686990; 2752526; 2818062; 3014670; 3080206; 3211278; 3407886; 3473422; 3538958; 3604494; 3801102; 3866638; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4456462; 4521998; 4587534; 4653070; 1638422; 2686991; 2752527; 2818063; 3014671; 3080207; 3211279; 3407887; 3473423; 3538959; 3604495; 3801103; 3866639; 3932175; 3997711; 4063247; 4128783; 4194319; 4259855; 4456463; 4521999; 4587535; 4653071; 1835026; 2686992; 2752528; 2818064; 3080208; 3211280; 3407888; 3473424; 3538960; 3604496; 3801104; 3932176; 3997712; 4063248; 4194320; 4259856; 4522000; 4587536; 4653072; 1900566; 2686993; 2752529; 2818065; 3014673; 3080209; 3211281; 3407889; 3473425; 3538961; 3604497; 3801105; 3866641; 3932177; 3997713; 4063249; 4128785; 4194321; 4259857; 4456465; 4522001; 4587537; 4653073; 1966102; 2686994; 2752530; 2818066; 3014674; 3080210; 3211282; 3407890; 3473426; 3538962; 3604498; 3801106; 3866642; 3932178; 3997714; 4063250; 4128786; 4194322; 4259858; 4456466; 4522002; 4587538; 4653074; 2031638; 2686995; 2752531; 2818067; 3014675; 3080211; 3211283; 3407891; 3473427; 3538963; 3604499; 3801107; 3866643; 3932179; 3997715; 4063251; 4128787; 4194323; 4259859; 4456467; 4522003; 4587539; 4653075; 2293782; 2686996; 2752532; 2818068; 3014676; 3080212; 3211284; 3407892; 3473428; 3538964; 3604500; 3801108; 3866644; 3932180; 3997716; 4063252; 4128788; 4194324; 4259860; 4456468; 4522004; 4587540; 4653076; 2359318; 2686997; 2752533; 2818069; 3014677; 3080213; 3211285; 3407893; 3473429; 3538965; 3604501; 3801109; 3866645; 3932181; 3997717; 4063253; 4128789; 4194325; 4259861; 4456469; 4522005; 4587541; 4653077; 2424854; 2686998; 2752534; 2818070; 3014678; 3080214; 3211286; 3407894; 3473430; 3538966; 3604502; 3801110; 3866646; 3932182; 3997718; 4063254; 4128790; 4194326; 4259862; 4456470; 4522006; 4587542; 4653078; 2490390; 2686999; 2752535; 2818071; 3014679; 3080215; 3211287; 3407895; 3473431; 3538967; 3604503; 3801111; 3866647; 3932183; 3997719; 4063255; 4128791; 4194327; 4259863; 4456471; 4522007; 4587543; 4653079; 2555926; 2687000; 2752536; 2818072; 3014680; 3080216; 3211288; 3407896; 3473432; 3538968; 3604504; 3801112; 3866648; 3932184; 3997720; 4063256; 4128792; 4194328; 4259864; 4456472; 4522008; 4587544; 4653080; 2686977; 3014681; 2752513; 3014682; 2818054; 3014683; 3080219; 3407899; 3473435; 4587547; 4653083; 2949143; 2687004; 2752540; 2818076; 3014684; 3080220; 3211292; 3407900; 3473436; 3538972; 3604508; 3735580; 3801116; 3866652; 3932188; 3997724; 4063260; 4128796; 4194332; 4259868; 4456476; 4522012; 4587548; 4653084; 3145750; 2687005; 2752541; 2818077; 3014685; 3080221; 3211293; 3407901; 3473437; 3538973; 3604509; 3801117; 3866653; 3932189; 3997725; 4063261; 4128797; 4194333; 4259869; 4456477; 4522013; 4587549; 4653085; 3211273; 2752542; 3211294; 3407902; 3604510; 3997726; 4063262; 4194334; 4259870; 4522014; 3276809; 2752543; 3211295; 3407903; 3604511; 3997727; 4063263; 4194335; 4259871; 4522015; 3407881; 2752544; 3211296; 3407904; 3604512; 3997728; 4063264; 4194336; 4259872; 4522016; 3473417; 2752545; 3211297; 3407905; 3604513; 3997729; 4063265; 4194337; 4259873; 4522017; 3538954; 2752546; 2818082; 3211298; 3407906; 3604514; 3997730; 4063266; 4194338; 4259874; 4522018; 3604491; 2687011; 2752547; 2818083; 3211299; 3407907; 3604515; 3997731; 4063267; 4194339; 4259875; 4522019; 3670027; 2687012; 2752548; 2818084; 3211300; 3407908; 3604516; 3997732; 4063268; 4194340; 4259876; 4522020; 3735558; 3080229; 3407909; 3473445; 3801125; 4587557; 4653093; 3801098; 2752550; 2818086; 3211302; 3407910; 3604518; 3997734; 4063270; 4194342; 4259878; 4522022; 3866634; 2752551; 2818087; 3211303; 3407911; 3604519; 3997735; 4063271; 4194343; 4259879; 4522023; 3932171; 2752552; 2818088; 3211304; 3342376; 3407912; 3604520; 3997736; 4063272; 4194344; 4259880; 4522024; 3997706; 2752553; 2818089; 3211305; 3407913; 3604521; 3997737; 4063273; 4194345; 4259881; 4522025; 4063254; 2687018; 2752554; 2818090; 3014698; 3080234; 3211306; 3407914; 3473450; 3538986; 3604522; 3801130; 3866666; 3932202; 3997738; 4063274; 4128810; 4194346; 4259882; 4456490; 4522026; 4587562; 4653098; 4128790; 2687019; 2752555; 2818091; 3014699; 3080235; 3211307; 3407915; 3473451; 3538987; 3604523; 3801131; 3866667; 3932203; 3997739; 4063275; 4128811; 4194347; 4259883; 4456491; 4522027; 4587563; 4653099; 4194322; 2687020; 2752556; 2818092; 3080236; 3211308; 3407916; 3473452; 3538988; 3604524; 3801132; 3932204; 3997740; 4063276; 4194348; 4259884; 4522028; 4587564; 4653100; 4259862; 2686997; 2752533; 2818069; 3080213; 3211285; 3276845; 3407893; 3473429; 3538965; 3604501; 3801109; 3866645; 3932181; 3997717; 4063253; 4128789; 4194325; 4259861; 4456469; 4522005; 4587541; 4653077; 4325377; 3276846; 4456456; 2687023; 3080239; 3407919; 3473455; 3538991; 3801135; 4587567; 4653103; 4587528; 2687024; 3080240; 3407920; 3473456; 3538992; 3801136; 4587568; 4653104; 4784137; 2687025; 2949169; 3080241; 3407921; 3473457; 3538993; 3801137; 4587569; 4653105; 4849672; 2687026; 3080242; 3407922; 3473458; 3538994; 3801138; 4587570; 4653106; 4915201; 2883635; 4980737; 2883636; 5177346; 2883637; 3735605; 5242881; 3014710; 5308417; 3014711; 5373953; 3407928; 5439489; 3407929; 5505027; 3407930; 3997754; 4063290; 5570568; 2752571; 3211323; 3407931; 3604539; 3997755; 4063291; 4259899; 4522043; 5636100; 2752572; 3211324; 3604540; 4259900; 5701633; 3211325; 5898244; 2752574; 3211326; 3604542; 4259902; 5963780; 2752575; 3211327; 3604543; 4259903; 6029316; 2752576; 3211328; 3604544; 4259904; 6160392; 2752577; 3211329; 3407937; 3604545; 3997761; 4063297; 4259905; 4522049; 6225928; 2752578; 3211330; 3407938; 3604546; 3997762; 4063298; 4259906; 4522050; 6357000; 2752579; 3211331; 3407939; 3604547; 3997763; 4063299; 4259907; 4522051; 6422536; 2752580; 3211332; 3407940; 3604548; 3997764; 4063300; 4259908; 4522052; 6488066; 3211333; 4259909; 6553602; 3211334; 4259910; 6619137; 3604551; 6684673; 3211336; 6750209; 3211337; 6881281; 3211338|]
let reduces = Array.zeroCreate 107
for i = 0 to 106 do
        reduces.[i] <- Array.zeroCreate 72
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
let private lists_zeroReduces = [|[|13|]; [|13; 1; 0|]; [|16|]; [|19; 2|]; [|23|]; [|50|]; [|19|]; [|24|]; [|28|]; [|34|]; [|72|]; [|48|]; [|52|]; [|68|]; [|32|]; [|45|]; [|48; 45|]; [|3|]; [|6|]; [|9|]; [|10|]; [|15|]|]
let private small_zeroReduces =
        [|9; 2752512; 3211265; 3342336; 3407872; 3604480; 3997696; 4063232; 4259840; 4521984; 65544; 2752514; 3211266; 3407874; 3604482; 3997698; 4063234; 4259842; 4521986; 131080; 2752514; 3211266; 3407874; 3604482; 3997698; 4063234; 4259842; 4521986; 393223; 2752515; 3211267; 3407876; 3604483; 3997700; 4063236; 4259843; 458760; 2752517; 3211269; 3407877; 3604485; 3997701; 4063237; 4259845; 4521989; 524295; 2752518; 3211270; 3407876; 3604486; 3997700; 4063236; 4259846; 655361; 3407879; 786434; 2883592; 3735560; 851969; 2883593; 983048; 2686986; 3080202; 3407882; 3473418; 3538954; 3801098; 4587530; 4653066; 1048582; 3080203; 3407883; 3473419; 3801099; 4587531; 4653067; 1376273; 2686988; 2752524; 2818060; 3080204; 3211276; 3407884; 3473420; 3538956; 3604492; 3801100; 3997708; 4063244; 4194316; 4259852; 4521996; 4587532; 4653068; 2162694; 3080203; 3407883; 3473419; 3801099; 4587531; 4653067; 2359319; 2686989; 2752525; 2818061; 3014669; 3080205; 3211277; 3407885; 3473421; 3538957; 3604493; 3735565; 3801101; 3866637; 3932173; 3997709; 4063245; 4128781; 4194317; 4259853; 4456461; 4521997; 4587533; 4653069; 2424854; 2686990; 2752526; 2818062; 3014670; 3080206; 3211278; 3407886; 3473422; 3538958; 3604494; 3801102; 3866638; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4456462; 4521998; 4587534; 4653070; 3014662; 3080203; 3407883; 3473419; 3801099; 4587531; 4653067; 3342342; 3080203; 3407883; 3473419; 3801099; 4587531; 4653067; 3538960; 2686991; 2752527; 2818063; 3080203; 3211279; 3407888; 3473419; 3604495; 3801099; 3997711; 4063247; 4194319; 4259855; 4521999; 4587531; 4653067; 3604496; 2686991; 2752527; 2818063; 3080203; 3211279; 3407888; 3473419; 3604495; 3801099; 3997711; 4063247; 4194319; 4259855; 4521999; 4587531; 4653067; 3801098; 2752512; 2818048; 3211264; 3407872; 3604480; 3997696; 4063232; 4194304; 4259840; 4521984; 4259862; 2686989; 2752525; 2818061; 3080205; 3211277; 3407885; 3473421; 3538957; 3604493; 3735565; 3801101; 3866637; 3932173; 3997709; 4063245; 4128781; 4194317; 4259853; 4456461; 4521997; 4587533; 4653069; 4915201; 2883593; 5701635; 3211281; 3604498; 4259857; 5898248; 2752531; 3211283; 3407891; 3604499; 3997715; 4063251; 4259859; 4522003; 5963783; 2752518; 3211270; 3407876; 3604486; 3997700; 4063236; 4259846; 6160392; 2752532; 3211284; 3407892; 3604500; 3997716; 4063252; 4259860; 4522004; 6357000; 2752532; 3211284; 3407892; 3604500; 3997716; 4063252; 4259860; 4522004; 6488067; 3211281; 3604498; 4259857; 6684673; 3211285|]
let zeroReduces = Array.zeroCreate 107
for i = 0 to 106 do
        zeroReduces.[i] <- Array.zeroCreate 72
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
let private small_acc = [106; 0]
let private accStates = Array.zeroCreate 107
for i = 0 to 106 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 49
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(24, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(16, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(15, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(68, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(50, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(16, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(15, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(24, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(16, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(15, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(68, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(50, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(16, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(19, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(15, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_allpublic_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_include_ * '_rnglr_type_includes * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_unnamed_module_opt * '_rnglr_type_yard_start_rule>), 
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
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 108 "Parser.fsy"
                                
                                {
                                    info = { fileName = !currentFilename }
                                    head = _S1
                                    grammar = fst _S3 @ fst _S4
                                    foot = _S5
                                    options = joinMaps (snd _S3) (snd _S4)
                                }
                              
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 103 "Parser.fsy"
               : '_rnglr_type_file) 
# 328 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 103 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 338 "Parser.fs"
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
                
# 119 "Parser.fsy"
                     
                        match fst _S1 with
                        | [] -> [], Map.empty
                        | x ->  defaultModules x, snd _S1
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 118 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 362 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 126 "Parser.fsy"
                          [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 125 "Parser.fsy"
               : '_rnglr_type_modules) 
# 380 "Parser.fs"
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
                  
# 125 "Parser.fsy"
                                              fst _S1 :: fst _S2, joinMaps (snd _S1) (snd _S2) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 125 "Parser.fsy"
               : '_rnglr_type_modules) 
# 402 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_allpublic_opt) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_openings) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_rule_nlist) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 129 "Parser.fsy"
                             
                                {
                                    allPublic = _S1
                                    name = Some _S3
                                    openings = _S4
                                    rules = fst _S5
                                }
                                , snd _S5
                            
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_module_) 
# 438 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 139 "Parser.fsy"
                                                      false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 139 "Parser.fsy"
               : '_rnglr_type_allpublic_opt) 
# 456 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ALL_PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "ALL_PUBLIC expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 139 "Parser.fsy"
                                             true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 139 "Parser.fsy"
               : '_rnglr_type_allpublic_opt) 
# 476 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPEN _rnglr_val -> [_rnglr_val] | a -> failwith "OPEN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 141 "Parser.fsy"
                                                                 _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 141 "Parser.fsy"
               : '_rnglr_type_openings) 
# 500 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 141 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 141 "Parser.fsy"
               : '_rnglr_type_openings) 
# 518 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 143 "Parser.fsy"
                                                                [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 143 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 536 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 143 "Parser.fsy"
                                                         _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 143 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 560 "Parser.fs"
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
                
# 145 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 145 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 580 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 145 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 145 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 598 "Parser.fs"
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
                  
# 147 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 620 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 147 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 638 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 149 "Parser.fsy"
                                                          [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 149 "Parser.fsy"
               : '_rnglr_type_includes) 
# 656 "Parser.fs"
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
                  
# 149 "Parser.fsy"
                                                 _S1::_S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 149 "Parser.fsy"
               : '_rnglr_type_includes) 
# 678 "Parser.fs"
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
                  
# 153 "Parser.fsy"
                       
                          let def = parseRules _S2.text in
                          if def.grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintf "Error %s: Grammar in included files must be inside modules" _S2.text
                          def.grammar, def.options
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 151 "Parser.fsy"
               : '_rnglr_type_include_) 
# 705 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 163 "Parser.fsy"
                      [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 723 "Parser.fs"
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
                    
# 162 "Parser.fsy"
                              (fst _S1) :: (fst _S3), joinMaps (snd _S1) (snd _S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 747 "Parser.fs"
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
                              
# 167 "Parser.fsy"
                                      
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
# 166 "Parser.fsy"
               : '_rnglr_type_rule) 
# 795 "Parser.fs"
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
                
# 183 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 183 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 815 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 183 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 183 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 833 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 185 "Parser.fsy"
                                                                           !commonPublicModifier 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 185 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 851 "Parser.fs"
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
                
# 185 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 185 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 871 "Parser.fs"
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
                
# 185 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 185 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 891 "Parser.fs"
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
                    
# 187 "Parser.fsy"
                                                                                     Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 187 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 915 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 187 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 187 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 933 "Parser.fs"
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
                  
# 190 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 189 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 955 "Parser.fs"
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
                
# 189 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 189 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 975 "Parser.fs"
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
                
# 192 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 192 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 995 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 192 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 192 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1013 "Parser.fs"
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
                  
# 194 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1035 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 194 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1053 "Parser.fs"
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
                  
# 196 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1075 "Parser.fs"
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
                
# 196 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1095 "Parser.fs"
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
                  
# 199 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1117 "Parser.fs"
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
                    
# 198 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1141 "Parser.fs"
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
                
# 201 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1161 "Parser.fs"
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
                
# 201 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1181 "Parser.fs"
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
                
# 204 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1201 "Parser.fs"
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
                    
# 203 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1225 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLABEL _rnglr_val -> [_rnglr_val] | a -> failwith "DLABEL expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_no_lbl_seq) 
                 |> List.iter (fun (_S3) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 206 "Parser.fsy"
                                                                makeNewSeq _S3 _S1
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1251 "Parser.fs"
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
                  
# 208 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1273 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 208 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1291 "Parser.fs"
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
                    
# 210 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1315 "Parser.fs"
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
                
# 212 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1335 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 212 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1353 "Parser.fs"
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
                
# 214 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 214 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1373 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 214 "Parser.fsy"
                           false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 214 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1391 "Parser.fs"
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
                
# 216 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1411 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 216 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1429 "Parser.fs"
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
                
# 219 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 218 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1449 "Parser.fs"
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
                    
# 218 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 218 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1473 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PATTERN _rnglr_val -> [_rnglr_val] | a -> failwith "PATTERN expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 221 "Parser.fsy"
                                             _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 221 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1493 "Parser.fs"
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
                
# 221 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 221 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1513 "Parser.fs"
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
                
# 229 "Parser.fsy"
                                           PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1533 "Parser.fs"
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
                
# 228 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1553 "Parser.fs"
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
                
# 227 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1573 "Parser.fs"
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
                    
# 226 "Parser.fsy"
                                               _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1597 "Parser.fs"
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
                  
# 225 "Parser.fsy"
                                             POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1619 "Parser.fs"
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
                  
# 224 "Parser.fsy"
                                             PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1641 "Parser.fs"
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
                  
# 223 "Parser.fsy"
                                             PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1663 "Parser.fs"
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
                
# 231 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1683 "Parser.fs"
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
                  
# 234 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1705 "Parser.fs"
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
                
# 233 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1725 "Parser.fs"
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
                    
# 236 "Parser.fsy"
                                                                         Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1749 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 236 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1767 "Parser.fs"
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
                    
# 240 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_call) 
# 1794 "Parser.fs"
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
                
# 238 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_call) 
# 1814 "Parser.fs"
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
                  
# 245 "Parser.fsy"
                                                             Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1836 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 245 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1854 "Parser.fs"
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
                    
# 248 "Parser.fsy"
                                                                     (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1878 "Parser.fs"
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
                
# 247 "Parser.fsy"
                                              Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1898 "Parser.fs"
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
                    
# 250 "Parser.fsy"
                                                        Source.toString _S1, Source.toString _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 1922 "Parser.fs"
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
                
# 252 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1942 "Parser.fs"
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
                
# 252 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1962 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_allpublic_opt)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
