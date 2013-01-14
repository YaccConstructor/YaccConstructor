
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

# 75 "Parser.fs"
type Token =
    | ACTION of Source.t
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
    | PARAM of Source.t
    | PATTERN of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
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
    | 0 -> "action_opt"
    | 1 -> "alts"
    | 2 -> "bar_seq_nlist"
    | 3 -> "bound"
    | 4 -> "call"
    | 5 -> "file"
    | 6 -> "foot_opt"
    | 7 -> "formal_meta_list"
    | 8 -> "formal_meta_param_opt"
    | 9 -> "lbl_seq"
    | 10 -> "meta_param"
    | 11 -> "meta_param_opt"
    | 12 -> "meta_params"
    | 13 -> "no_lbl_seq"
    | 14 -> "omit_opt"
    | 15 -> "option_opt"
    | 16 -> "option_param"
    | 17 -> "option_params"
    | 18 -> "param_list"
    | 19 -> "param_opt"
    | 20 -> "patt"
    | 21 -> "predicate_opt"
    | 22 -> "prim"
    | 23 -> "rule"
    | 24 -> "rule_nlist"
    | 25 -> "semi_opt"
    | 26 -> "seq"
    | 27 -> "seq_elem"
    | 28 -> "seq_elem_list"
    | 29 -> "start_rule_sign_opt"
    | 30 -> "tada_rule"
    | 31 -> "yard_start_rule"
    | 32 -> "ACTION"
    | 33 -> "BAR"
    | 34 -> "COLON"
    | 35 -> "COMMA"
    | 36 -> "DGREAT"
    | 37 -> "DLABEL"
    | 38 -> "DLESS"
    | 39 -> "EOF"
    | 40 -> "EQUAL"
    | 41 -> "INCLUDE"
    | 42 -> "LIDENT"
    | 43 -> "LPAREN"
    | 44 -> "MINUS"
    | 45 -> "PARAM"
    | 46 -> "PATTERN"
    | 47 -> "PLUS"
    | 48 -> "PREDICATE"
    | 49 -> "QUESTION"
    | 50 -> "RPAREN"
    | 51 -> "SEMICOLON"
    | 52 -> "SET"
    | 53 -> "SHARPLINE"
    | 54 -> "STAR"
    | 55 -> "START_RULE_SIGN"
    | 56 -> "STRING"
    | 57 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 32
    | BAR _ -> 33
    | COLON _ -> 34
    | COMMA _ -> 35
    | DGREAT _ -> 36
    | DLABEL _ -> 37
    | DLESS _ -> 38
    | EOF _ -> 39
    | EQUAL _ -> 40
    | INCLUDE _ -> 41
    | LIDENT _ -> 42
    | LPAREN _ -> 43
    | MINUS _ -> 44
    | PARAM _ -> 45
    | PATTERN _ -> 46
    | PLUS _ -> 47
    | PREDICATE _ -> 48
    | QUESTION _ -> 49
    | RPAREN _ -> 50
    | SEMICOLON _ -> 51
    | SET _ -> 52
    | SHARPLINE _ -> 53
    | STAR _ -> 54
    | START_RULE_SIGN _ -> 55
    | STRING _ -> 56
    | UIDENT _ -> 57

let mutable private cur = 0
let leftSide = [|5; 31; 0; 0; 6; 6; 24; 24; 24; 23; 29; 29; 8; 8; 7; 7; 19; 19; 18; 18; 1; 1; 2; 2; 26; 26; 13; 13; 9; 28; 28; 27; 14; 14; 25; 25; 21; 21; 3; 3; 20; 20; 22; 22; 22; 22; 22; 22; 22; 10; 12; 12; 11; 11; 4; 4; 15; 15; 17; 17; 16; 30; 30|]
let private rules = [|0; 24; 6; 5; 32; 51; 32; 41; 56; 24; 23; 25; 24; 29; 42; 8; 18; 34; 15; 1; 55; 38; 7; 36; 42; 7; 42; 45; 45; 18; 26; 2; 26; 33; 26; 33; 26; 2; 9; 13; 32; 27; 28; 0; 37; 43; 13; 50; 27; 28; 14; 3; 21; 44; 51; 48; 22; 20; 40; 22; 46; 42; 56; 9; 4; 43; 1; 50; 22; 49; 22; 47; 22; 54; 22; 10; 12; 10; 38; 12; 36; 42; 11; 19; 57; 52; 17; 16; 35; 17; 16; 42; 40; 56; 39; 53|]
let private rulesStart = [|0; 3; 4; 5; 5; 7; 7; 7; 10; 13; 20; 21; 21; 24; 24; 26; 27; 28; 28; 30; 30; 32; 33; 35; 38; 39; 40; 41; 44; 48; 50; 50; 53; 54; 54; 55; 55; 56; 56; 57; 60; 61; 62; 63; 64; 65; 68; 70; 72; 74; 75; 77; 78; 81; 81; 84; 85; 87; 87; 90; 91; 94; 95; 96|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 85; 54; 2; 81; 5; 76; 79; 3; 80; 4; 6; 7; 71; 8; 69; 9; 10; 61; 11; 12; 13; 14; 43; 48; 55; 26; 51; 15; 18; 19; 20; 58; 59; 40; 60; 56; 57; 16; 17; 21; 22; 30; 23; 24; 25; 27; 28; 29; 31; 34; 32; 33; 35; 38; 37; 36; 39; 41; 42; 44; 45; 46; 47; 49; 52; 50; 53; 62; 68; 65; 63; 64; 66; 67; 70; 72; 74; 73; 75; 77; 78; 82; 83; 84|]
let private small_gotos =
        [|3; 0; 327681; 2097154; 65541; 1507331; 1572868; 1900549; 2686982; 3604487; 131074; 1638408; 3342345; 196613; 1507331; 1572874; 1900549; 2686982; 3604487; 327681; 2752523; 393218; 524300; 2490381; 458754; 1179662; 2949135; 524289; 2228240; 589826; 983057; 3407890; 655369; 65555; 589844; 851989; 917526; 1703959; 1769496; 2097177; 2424858; 2883611; 917515; 196636; 262173; 589854; 1310751; 1441824; 2424858; 2752545; 2818082; 3014691; 3670052; 3735589; 983042; 1376294; 3145767; 1310721; 2621480; 1376264; 262173; 589854; 1441833; 2424858; 2752554; 2818082; 3670052; 3735589; 1441795; 3080235; 3211308; 3538989; 1703937; 2818094; 1769477; 852015; 917526; 1769496; 2097177; 2883611; 1835009; 3276848; 1966082; 720945; 2490418; 2031618; 1245235; 2949172; 2228234; 262173; 589854; 655413; 786486; 1441847; 2424858; 2752554; 2818082; 3670052; 3735589; 2293770; 262173; 589854; 655413; 786488; 1441847; 2424858; 2752554; 2818082; 3670052; 3735589; 2424835; 3080235; 3211308; 3538989; 2490369; 2359353; 2621449; 65594; 589844; 851989; 917526; 1703959; 1769496; 2097177; 2424858; 2883611; 2686977; 3276859; 2818050; 131132; 2162749; 2949128; 589844; 851989; 917526; 1703998; 1769496; 2097177; 2424858; 2883611; 3014658; 131135; 2162749; 3145732; 917526; 1769536; 1835073; 2883611; 3211268; 917526; 1769536; 1835074; 2883611; 3407874; 67; 2097154; 3801091; 3080235; 3211308; 3538989; 3866626; 720945; 2490418; 3997699; 1048644; 1114181; 2752582; 4063233; 2293831; 4128771; 1048644; 1114184; 2752582; 4259841; 2621513; 4325377; 3670090; 4521986; 1179723; 2949135; 4653058; 458828; 2752589; 4718593; 2359374; 4849666; 458831; 2752589; 4980737; 3670096; 5046277; 1507331; 1572945; 1900549; 2686982; 3604487; 5308418; 393298; 3342419; 5439489; 2097236|]
let gotos = Array.zeroCreate 86
for i = 0 to 85 do
        gotos.[i] <- Array.zeroCreate 58
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
let private lists_reduces = [|[|0,1|]; [|8,1|]; [|8,2|]; [|8,3|]; [|9,7|]; [|24,1|]; [|25,1|]; [|31,2|]; [|31,3|]; [|36,1|]; [|44,1|]; [|43,1|]; [|39,3|]; [|47,2|]; [|46,2|]; [|48,2|]; [|28,4|]; [|54,1|]; [|54,2|]; [|54,3|]; [|16,1|]; [|51,1|]; [|50,2|]; [|49,1|]; [|52,3|]; [|45,3|]; [|21,1|]; [|20,2|]; [|22,2|]; [|23,3|]; [|27,1|]; [|29,1|]; [|29,2|]; [|32,1|]; [|27,2|]; [|27,3|]; [|2,1|]; [|26,1|]; [|42,1|]; [|55,1|]; [|38,1|]; [|41,1|]; [|40,1|]; [|59,1|]; [|58,3|]; [|60,3|]; [|56,2|]; [|18,1|]; [|18,2|]; [|12,3|]; [|15,1|]; [|14,2|]; [|7,2|]; [|7,3|]; [|10,1|]; [|34,1|]; [|0,2|]; [|0,3|]; [|4,2|]|]
let private small_reduces =
        [|65537; 2555904; 131074; 2555905; 3342337; 196610; 2555906; 3342338; 262146; 2555907; 3342339; 720901; 2555908; 2686980; 2752516; 3342340; 3604484; 786439; 2162693; 2555909; 2686981; 2752517; 3276805; 3342341; 3604485; 851975; 2162694; 2555910; 2686982; 2752518; 3276806; 3342342; 3604486; 983054; 2097159; 2162695; 2424839; 2555911; 2686983; 2752519; 2818055; 2883591; 3014663; 3276807; 3342343; 3604487; 3670023; 3735559; 1048590; 2097160; 2162696; 2424840; 2555912; 2686984; 2752520; 2818056; 2883592; 3014664; 3276808; 3342344; 3604488; 3670024; 3735560; 1114126; 2097161; 2162697; 2424841; 2555913; 2686985; 2752521; 2818057; 2883593; 3014665; 3276809; 3342345; 3604489; 3670025; 3735561; 1179667; 2097162; 2162698; 2359306; 2424842; 2555914; 2686986; 2752522; 2818058; 2883594; 3014666; 3080202; 3145738; 3211274; 3276810; 3342346; 3538954; 3604490; 3670026; 3735562; 1245203; 2097163; 2162699; 2359307; 2424843; 2555915; 2686987; 2752523; 2818059; 2883595; 3014667; 3080203; 3145739; 3211275; 3276811; 3342347; 3538955; 3604491; 3670027; 3735563; 1441807; 2097164; 2162700; 2424844; 2555916; 2686988; 2752524; 2818060; 2883596; 3014668; 3145740; 3276812; 3342348; 3604492; 3670028; 3735564; 1507347; 2097165; 2162701; 2359309; 2424845; 2555917; 2686989; 2752525; 2818061; 2883597; 3014669; 3080205; 3145741; 3211277; 3276813; 3342349; 3538957; 3604493; 3670029; 3735565; 1572883; 2097166; 2162702; 2359310; 2424846; 2555918; 2686990; 2752526; 2818062; 2883598; 3014670; 3080206; 3145742; 3211278; 3276814; 3342350; 3538958; 3604494; 3670030; 3735566; 1638419; 2097167; 2162703; 2359311; 2424847; 2555919; 2686991; 2752527; 2818063; 2883599; 3014671; 3080207; 3145743; 3211279; 3276815; 3342351; 3538959; 3604495; 3670031; 3735567; 1900563; 2097168; 2162704; 2359312; 2424848; 2555920; 2686992; 2752528; 2818064; 2883600; 3014672; 3080208; 3145744; 3211280; 3276816; 3342352; 3538960; 3604496; 3670032; 3735568; 1966099; 2097169; 2162705; 2359313; 2424849; 2555921; 2686993; 2752529; 2818065; 2883601; 3014673; 3080209; 3145745; 3211281; 3276817; 3342353; 3538961; 3604497; 3670033; 3735569; 2031635; 2097170; 2162706; 2359314; 2424850; 2555922; 2686994; 2752530; 2818066; 2883602; 3014674; 3080210; 3145746; 3211282; 3276818; 3342354; 3538962; 3604498; 3670034; 3735570; 2097171; 2097171; 2162707; 2359315; 2424851; 2555923; 2686995; 2752531; 2818067; 2883603; 3014675; 3080211; 3145747; 3211283; 3276819; 3342355; 3538963; 3604499; 3670035; 3735571; 2162707; 2097172; 2162708; 2359316; 2424852; 2555924; 2686996; 2752532; 2818068; 2883604; 3014676; 3080212; 3145748; 3211284; 3276820; 3342356; 3538964; 3604500; 3670036; 3735572; 2293761; 2359317; 2359297; 2359318; 2424838; 2359319; 2424855; 2752535; 2818071; 3670039; 3735575; 2555924; 2097176; 2162712; 2359320; 2424856; 2555928; 2687000; 2752536; 2818072; 2883608; 2949144; 3014680; 3080216; 3145752; 3211288; 3276824; 3342360; 3538968; 3604504; 3670040; 3735576; 2752531; 2097177; 2162713; 2359321; 2424857; 2555929; 2687001; 2752537; 2818073; 2883609; 3014681; 3080217; 3145753; 3211289; 3276825; 3342361; 3538969; 3604505; 3670041; 3735577; 2818054; 2555930; 2687002; 2752538; 3276826; 3342362; 3604506; 2883590; 2555931; 2687003; 2752539; 3276827; 3342363; 3604507; 3014662; 2555932; 2687004; 2752540; 3276828; 3342364; 3604508; 3080198; 2555933; 2687005; 2752541; 3276829; 3342365; 3604509; 3145735; 2162718; 2555934; 2687006; 2752542; 3276830; 3342366; 3604510; 3211272; 2097183; 2162719; 2555935; 2687007; 2752543; 3276831; 3342367; 3604511; 3276808; 2097184; 2162720; 2555936; 2687008; 2752544; 3276832; 3342368; 3604512; 3342342; 2424865; 2752545; 2818081; 3014689; 3670049; 3735585; 3407879; 2162722; 2555938; 2687010; 2752546; 3276834; 3342370; 3604514; 3473415; 2162723; 2555939; 2687011; 2752547; 3276835; 3342371; 3604515; 3538951; 2162724; 2555940; 2687012; 2752548; 3276836; 3342372; 3604516; 3604487; 2162725; 2555941; 2687013; 2752549; 3276837; 3342373; 3604517; 3670035; 2097190; 2162726; 2359334; 2424870; 2555942; 2687014; 2752550; 2818086; 2883622; 3014694; 3080230; 3145766; 3211302; 3276838; 3342374; 3538982; 3604518; 3670054; 3735590; 3735571; 2097191; 2162727; 2359335; 2424871; 2555943; 2687015; 2752551; 2818087; 2883623; 3014695; 3080231; 3145767; 3211303; 3276839; 3342375; 3538983; 3604519; 3670055; 3735591; 3801103; 2097192; 2162728; 2424872; 2555944; 2687016; 2752552; 2818088; 2883624; 3014696; 3145768; 3276840; 3342376; 3604520; 3670056; 3735592; 3866643; 2097169; 2162705; 2424849; 2555921; 2621481; 2686993; 2752529; 2818065; 2883601; 3014673; 3080209; 3145745; 3211281; 3276817; 3342353; 3538961; 3604497; 3670033; 3735569; 3932161; 2621482; 4063240; 2097195; 2424875; 2752555; 2818091; 2883627; 3014699; 3670059; 3735595; 4194312; 2097196; 2424876; 2752556; 2818092; 2883628; 3014700; 3670060; 3735596; 4390921; 2097197; 2293805; 2424877; 2752557; 2818093; 2883629; 3014701; 3670061; 3735597; 4456456; 2097198; 2424878; 2752558; 2818094; 2883630; 3014702; 3670062; 3735598; 4521985; 2228271; 4587521; 2228272; 4784130; 2228273; 2949169; 4849665; 2359346; 4915201; 2359347; 5046274; 2555956; 3342388; 5111810; 2555957; 3342389; 5177345; 2752566; 5242885; 2555959; 2687031; 2752567; 3342391; 3604535; 5308417; 2555960; 5373953; 2555961; 5505025; 2555962|]
let reduces = Array.zeroCreate 86
for i = 0 to 85 do
        reduces.[i] <- Array.zeroCreate 58
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
let private lists_zeroReduces = [|[|3; 1; 0|]; [|3|]; [|6|]; [|11|]; [|35|]; [|13|]; [|19|]; [|57|]; [|33|]; [|37|]; [|53|]; [|17|]; [|30|]; [|33; 30|]; [|5|]|]
let private small_zeroReduces =
        [|5; 2555904; 2686977; 2752513; 3342337; 3604481; 65539; 2555906; 2752515; 3342338; 131077; 2555908; 2686980; 2752516; 3342340; 3604484; 196611; 2555906; 2752515; 3342338; 393218; 2228229; 2949125; 458753; 2228230; 589832; 2097159; 2424839; 2752519; 2818055; 2883591; 3014663; 3670023; 3735559; 655366; 2424840; 2752520; 2818056; 3014664; 3670024; 3735560; 983054; 2097161; 2162697; 2424841; 2555913; 2686985; 2752521; 2818057; 2883593; 3014665; 3276809; 3342345; 3604489; 3670025; 3735561; 1769478; 2424840; 2752520; 2818056; 3014664; 3670024; 3735560; 1966100; 2097162; 2162698; 2359306; 2424842; 2555914; 2686986; 2752522; 2818058; 2883594; 2949130; 3014666; 3080202; 3145738; 3211274; 3276810; 3342346; 3538954; 3604490; 3670026; 3735562; 2031635; 2097163; 2162699; 2359307; 2424843; 2555915; 2686987; 2752523; 2818059; 2883595; 3014667; 3080203; 3145739; 3211275; 3276811; 3342347; 3538955; 3604491; 3670027; 3735563; 2621446; 2424840; 2752520; 2818056; 3014664; 3670024; 3735560; 2949126; 2424840; 2752520; 2818056; 3014664; 3670024; 3735560; 3145741; 2097164; 2162700; 2424840; 2555916; 2686988; 2752525; 2818056; 3014664; 3276812; 3342348; 3604492; 3670024; 3735560; 3211277; 2097164; 2162700; 2424840; 2555916; 2686988; 2752525; 2818056; 3014664; 3276812; 3342348; 3604492; 3670024; 3735560; 3407879; 2162689; 2555905; 2686977; 2752513; 3276801; 3342337; 3604481; 3866643; 2097162; 2162698; 2424842; 2555914; 2686986; 2752522; 2818058; 2883594; 2949130; 3014666; 3080202; 3145738; 3211274; 3276810; 3342346; 3538954; 3604490; 3670026; 3735562; 4521985; 2228230; 5046275; 2555906; 2752515; 3342338; 5308417; 2555918|]
let zeroReduces = Array.zeroCreate 86
for i = 0 to 85 do
        zeroReduces.[i] <- Array.zeroCreate 58
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
let private small_acc = [85; 0]
let private accStates = Array.zeroCreate 86
for i = 0 to 85 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 39
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(57, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(57, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_rule_nlist) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_foot_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    

                     
                            { Definition.empty with
                                Definition.info = { fileName = !currentFilename }
                                Definition.head = _S1
                                Definition.grammar = defaultModules (fst _S2)
                                Definition.foot = _S3
                                Definition.options = (snd _S2)
                            }
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 99 "Parser.fsy"
               : '_rnglr_type_file) 
# 296 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 99 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 306 "Parser.fs"
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
                

                 Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 112 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 326 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 112 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 344 "Parser.fs"
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
                  

                   Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 114 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 366 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 114 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 384 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 116 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 402 "Parser.fs"
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
                ((unbox _rnglr_children.[2]) : '_rnglr_type_rule_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    

                     let def = parseRules _S2.text in
                              (def.grammar.Head.rules @ (fst _S3), joinMaps def.options (snd _S3)) 
                            
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 116 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 428 "Parser.fs"
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
                    

                     (fst _S1) :: (fst _S3), joinMaps (snd _S1) (snd _S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 116 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 452 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_start_rule_sign_opt) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_formal_meta_param_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_param_list) 
                   |> List.iter (fun (_S4) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      ((unbox _rnglr_children.[5]) : '_rnglr_type_option_opt) 
                       |> List.iter (fun (_S6) -> 
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_alts) 
                         |> List.iter (fun (_S7) -> 
                          _rnglr_cycle_res := (
                            

                             
                                    let newRule =
                                        {
                                            Rule.isStart = _S1
                                            Rule.isPublic = false
                                            Rule.name = _S2
                                            Rule.metaArgs = getList _S3
                                            Rule.body = _S7
                                            Rule.args = _S4
                                        }
                                    in
                                    match _S6 with
                                    | Some m -> newRule, Map.empty.Add (newRule, m)
                                    | None -> newRule, Map.empty
                                   
                              )::!_rnglr_cycle_res ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 125 "Parser.fsy"
               : '_rnglr_type_rule) 
# 498 "Parser.fs"
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
                

                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 142 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 518 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 142 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 536 "Parser.fs"
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
                    

                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 560 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 578 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 146 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 600 "Parser.fs"
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
                

                [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 146 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 620 "Parser.fs"
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
                

                Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 149 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 640 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 149 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 658 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 151 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 680 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 151 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 698 "Parser.fs"
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
                  

                  PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_alts) 
# 720 "Parser.fs"
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
                

                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_alts) 
# 740 "Parser.fs"
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
                  

                  _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 762 "Parser.fs"
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
                    

                    PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 786 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_seq) 
# 806 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_seq) 
# 826 "Parser.fs"
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
                

                 PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 160 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 846 "Parser.fs"
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
                    

                     PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 160 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 870 "Parser.fs"
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
                      

                      makeNewSeq _S3 _S1
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 163 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 896 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 165 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 918 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 165 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 936 "Parser.fs"
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
                    

                    {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 167 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 960 "Parser.fs"
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
                

                 true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 980 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 998 "Parser.fs"
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
                

                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1018 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1036 "Parser.fs"
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
                

                 Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1056 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1074 "Parser.fs"
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
                

                 createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1094 "Parser.fs"
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
                    

                     createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1118 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 178 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1138 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 178 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1158 "Parser.fs"
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
                

                PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1178 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1198 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1218 "Parser.fs"
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
                    

                    _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1242 "Parser.fs"
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
                  

                  POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1264 "Parser.fs"
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
                  

                  PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1286 "Parser.fs"
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
                  

                  PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 180 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1308 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 188 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1328 "Parser.fs"
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
                  

                  _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 190 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1350 "Parser.fs"
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
                

                [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 190 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1370 "Parser.fs"
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
                    

                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 193 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1394 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 193 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1412 "Parser.fs"
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
                    

                     match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_call) 
# 1439 "Parser.fs"
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
                

                PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_call) 
# 1459 "Parser.fs"
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
                  

                   Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 202 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1481 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 202 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1499 "Parser.fs"
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
                    

                    (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1523 "Parser.fs"
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
                

                 Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1543 "Parser.fs"
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
                    

                     (Source.toString _S1, Source.toString _S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 1567 "Parser.fs"
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
                

                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1587 "Parser.fs"
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
                

                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1607 "Parser.fs"
      );
  |] , [|
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_no_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_omit_opt)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
