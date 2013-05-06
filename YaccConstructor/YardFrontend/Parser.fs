
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
    | PATTERN of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | PRIVATE of Source.t
    | PUBLIC of Source.t
    | QUESTION of Source.t
    | RNGLR_EOF of Source.t
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
    | 6 -> "error"
    | 7 -> "file"
    | 8 -> "foot_opt"
    | 9 -> "formal_meta_list"
    | 10 -> "formal_meta_param_opt"
    | 11 -> "ident"
    | 12 -> "include_"
    | 13 -> "includes"
    | 14 -> "lbl_seq"
    | 15 -> "meta_param"
    | 16 -> "meta_param_opt"
    | 17 -> "meta_params"
    | 18 -> "module_"
    | 19 -> "module_header"
    | 20 -> "modules"
    | 21 -> "no_lbl_seq"
    | 22 -> "omit_opt"
    | 23 -> "open_list"
    | 24 -> "openings"
    | 25 -> "option_opt"
    | 26 -> "option_param"
    | 27 -> "option_params"
    | 28 -> "param_list"
    | 29 -> "param_opt"
    | 30 -> "patt"
    | 31 -> "predicate_opt"
    | 32 -> "prim"
    | 33 -> "rule"
    | 34 -> "rule_nlist"
    | 35 -> "semi_opt"
    | 36 -> "seq"
    | 37 -> "seq_elem"
    | 38 -> "seq_elem_list"
    | 39 -> "start_rule_sign_opt"
    | 40 -> "tada_rule"
    | 41 -> "unnamed_module_opt"
    | 42 -> "weight_opt"
    | 43 -> "yard_start_rule"
    | 44 -> "ACTION"
    | 45 -> "ALL_PUBLIC"
    | 46 -> "BAR"
    | 47 -> "COLON"
    | 48 -> "COMMA"
    | 49 -> "DGREAT"
    | 50 -> "DLABEL"
    | 51 -> "DLESS"
    | 52 -> "EOF"
    | 53 -> "EQUAL"
    | 54 -> "INCLUDE"
    | 55 -> "LIDENT"
    | 56 -> "LPAREN"
    | 57 -> "MINUS"
    | 58 -> "MODULE"
    | 59 -> "OPEN"
    | 60 -> "PARAM"
    | 61 -> "PATTERN"
    | 62 -> "PLUS"
    | 63 -> "PREDICATE"
    | 64 -> "PRIVATE"
    | 65 -> "PUBLIC"
    | 66 -> "QUESTION"
    | 67 -> "RNGLR_EOF"
    | 68 -> "RPAREN"
    | 69 -> "SEMICOLON"
    | 70 -> "SET"
    | 71 -> "SHARPLINE"
    | 72 -> "STAR"
    | 73 -> "START_RULE_SIGN"
    | 74 -> "STRING"
    | 75 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 44
    | ALL_PUBLIC _ -> 45
    | BAR _ -> 46
    | COLON _ -> 47
    | COMMA _ -> 48
    | DGREAT _ -> 49
    | DLABEL _ -> 50
    | DLESS _ -> 51
    | EOF _ -> 52
    | EQUAL _ -> 53
    | INCLUDE _ -> 54
    | LIDENT _ -> 55
    | LPAREN _ -> 56
    | MINUS _ -> 57
    | MODULE _ -> 58
    | OPEN _ -> 59
    | PARAM _ -> 60
    | PATTERN _ -> 61
    | PLUS _ -> 62
    | PREDICATE _ -> 63
    | PRIVATE _ -> 64
    | PUBLIC _ -> 65
    | QUESTION _ -> 66
    | RNGLR_EOF _ -> 67
    | RPAREN _ -> 68
    | SEMICOLON _ -> 69
    | SET _ -> 70
    | SHARPLINE _ -> 71
    | STAR _ -> 72
    | START_RULE_SIGN _ -> 73
    | STRING _ -> 74
    | UIDENT _ -> 75

let mutable private cur = 0
let leftSide = [|7; 43; 41; 20; 20; 18; 11; 11; 19; 19; 24; 24; 23; 23; 1; 1; 8; 8; 13; 13; 12; 34; 34; 33; 39; 39; 0; 0; 0; 10; 10; 9; 9; 29; 29; 28; 28; 42; 42; 2; 2; 3; 3; 36; 36; 21; 21; 14; 38; 38; 37; 22; 22; 35; 35; 31; 31; 4; 4; 30; 30; 32; 32; 32; 32; 32; 32; 32; 15; 17; 17; 16; 16; 5; 5; 25; 25; 27; 27; 26; 40; 40|]
let private rules = [|1; 13; 41; 20; 8; 52; 7; 34; 18; 20; 19; 11; 24; 34; 55; 75; 58; 45; 58; 59; 11; 23; 48; 11; 23; 44; 69; 44; 12; 13; 54; 74; 33; 35; 34; 39; 0; 55; 10; 28; 47; 25; 2; 73; 64; 65; 51; 9; 49; 55; 9; 55; 60; 60; 28; 60; 36; 3; 36; 46; 36; 46; 36; 3; 14; 21; 44; 37; 38; 1; 50; 42; 56; 21; 68; 37; 38; 22; 4; 31; 57; 69; 63; 32; 30; 53; 32; 61; 55; 74; 14; 5; 56; 2; 68; 32; 66; 32; 62; 32; 72; 32; 15; 17; 15; 51; 17; 49; 55; 16; 29; 75; 70; 27; 26; 48; 27; 26; 55; 53; 74; 52; 71|]
let private rulesStart = [|0; 6; 7; 8; 8; 10; 14; 15; 16; 17; 19; 22; 22; 22; 25; 26; 26; 28; 28; 28; 30; 32; 32; 35; 43; 44; 44; 44; 45; 46; 49; 49; 51; 52; 53; 53; 55; 55; 56; 56; 58; 59; 61; 64; 65; 66; 67; 70; 75; 77; 77; 80; 81; 81; 82; 82; 83; 83; 84; 87; 88; 89; 90; 91; 92; 95; 97; 99; 101; 102; 104; 105; 108; 108; 111; 112; 114; 114; 117; 118; 121; 122; 123|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 112; 61; 2; 6; 4; 3; 5; 7; 88; 10; 89; 86; 8; 87; 9; 11; 84; 85; 12; 13; 79; 14; 77; 15; 16; 69; 17; 18; 19; 20; 50; 55; 62; 32; 58; 21; 24; 25; 26; 65; 66; 47; 68; 63; 64; 22; 23; 27; 28; 37; 29; 30; 31; 33; 67; 34; 35; 36; 38; 41; 39; 40; 42; 45; 44; 43; 46; 48; 49; 51; 52; 53; 54; 56; 59; 57; 60; 70; 76; 73; 71; 72; 74; 75; 78; 80; 82; 81; 83; 90; 91; 107; 104; 106; 103; 92; 101; 102; 93; 95; 94; 96; 97; 98; 99; 100; 105; 108; 110; 109; 111|]
let private small_gotos =
        [|3; 65536; 458753; 2883586; 65539; 786435; 851972; 3538949; 131075; 786435; 851974; 3538949; 262145; 4849671; 393221; 2162696; 2228233; 2555914; 2686987; 4784140; 458754; 2293773; 4521998; 524292; 2162696; 2228239; 2555914; 4784140; 655363; 16; 4194321; 4259858; 720897; 3604499; 786434; 655380; 3342357; 851970; 1835030; 3932183; 917505; 3080216; 983042; 1638425; 4587546; 1048585; 131099; 917532; 1376285; 1441822; 2359327; 2424864; 2883617; 3276834; 3735587; 1310731; 262180; 327717; 917542; 1966119; 2097192; 3276834; 3604521; 3670058; 3997739; 4849708; 4915245; 1376258; 2031662; 4128815; 1703937; 3473456; 1769480; 327717; 917542; 2097201; 3276834; 3604530; 3670058; 4849708; 4915245; 1835011; 4063283; 4325428; 4718645; 2097154; 2752566; 3932215; 2162689; 3670072; 2228229; 1376313; 1441822; 2424864; 2883617; 3735587; 2293761; 4456506; 2424834; 1048635; 3342396; 2490370; 1900605; 3932222; 2686986; 327717; 917542; 983103; 1114176; 2097217; 3276834; 3604530; 3670058; 4849708; 4915245; 2752522; 327717; 917542; 983103; 1114178; 2097217; 3276834; 3604530; 3670058; 4849708; 4915245; 2883587; 4063283; 4325428; 4718645; 2949121; 3211331; 3080201; 131140; 917532; 1376285; 1441822; 2359327; 2424864; 2883617; 3276834; 3735587; 3145729; 4456517; 3276802; 196678; 3014727; 3407880; 917532; 1376285; 1441822; 2359368; 2424864; 2883617; 3276834; 3735587; 3473410; 196681; 3014727; 3604484; 1441822; 2424906; 2490443; 3735587; 3670020; 1441822; 2424906; 2490444; 3735587; 3866626; 65613; 2883586; 4259843; 4063283; 4325428; 4718645; 4325378; 1048635; 3342396; 4521987; 1704014; 1769551; 3604560; 4587521; 3145809; 4653059; 1704014; 1769554; 3604560; 4784129; 3473491; 4849665; 4849748; 5046274; 1835093; 3932183; 5177346; 589910; 3604567; 5242881; 3211352; 5373954; 589913; 3604567; 5832709; 1179738; 1245275; 1310812; 2949213; 3801182; 5898245; 1179738; 1245275; 1310815; 2949213; 3801182; 5963779; 720992; 3604577; 4915298; 6029314; 1572963; 3866724; 6094852; 2162696; 2228325; 2555914; 4784140; 6225923; 720998; 3604577; 4915298; 6291458; 1507431; 3145832; 6422531; 721001; 3604577; 4915298; 6488066; 1507434; 3145832; 6815745; 3801195; 7012354; 524396; 4522093; 7077889; 3407982; 7208961; 2883695|]
let gotos = Array.zeroCreate 113
for i = 0 to 112 do
        gotos.[i] <- Array.zeroCreate 76
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
let private lists_reduces = [|[|19,1|]; [|19,2|]; [|20,2|]; [|22,1|]; [|22,2|]; [|22,3|]; [|23,8|]; [|43,1|]; [|44,1|]; [|50,2|]; [|50,3|]; [|55,1|]; [|63,1|]; [|62,1|]; [|58,3|]; [|66,2|]; [|65,2|]; [|67,2|]; [|47,5|]; [|73,1|]; [|73,2|]; [|73,3|]; [|33,1|]; [|70,1|]; [|69,2|]; [|68,1|]; [|71,3|]; [|64,3|]; [|40,1|]; [|39,2|]; [|41,2|]; [|42,3|]; [|46,1|]; [|48,1|]; [|48,2|]; [|51,1|]; [|46,2|]; [|46,3|]; [|14,1|]; [|45,1|]; [|61,1|]; [|74,1|]; [|57,1|]; [|60,1|]; [|37,1|]; [|59,1|]; [|78,1|]; [|77,3|]; [|79,3|]; [|75,2|]; [|35,1|]; [|35,2|]; [|29,3|]; [|32,1|]; [|31,2|]; [|27,1|]; [|28,1|]; [|24,1|]; [|53,1|]; [|2,1|]; [|4,1|]; [|5,2|]; [|5,3|]; [|5,4|]; [|10,2|]; [|10,3|]; [|13,2|]; [|13,3|]; [|6,1|]; [|7,1|]; [|4,2|]; [|9,2|]; [|8,1|]; [|0,6|]; [|16,2|]|]
let private small_reduces =
        [|131080; 2949120; 3407872; 3604480; 3801088; 4194304; 4259840; 4521984; 4784128; 196616; 2949121; 3407873; 3604481; 3801089; 4194305; 4259841; 4521985; 4784129; 327689; 2949122; 3407874; 3538946; 3604482; 3801090; 4194306; 4259842; 4521986; 4784130; 458756; 2949123; 3407875; 3801091; 4521987; 524292; 2949124; 3407876; 3801092; 4521988; 589828; 2949125; 3407877; 3801093; 4521989; 1114120; 2949126; 3407878; 3604486; 3801094; 4194310; 4259846; 4521990; 4784134; 1179658; 2949127; 3014663; 3407879; 3604487; 3801095; 4194311; 4259847; 4456455; 4521991; 4784135; 1245194; 2949128; 3014664; 3407880; 3604488; 3801096; 4194312; 4259848; 4456456; 4521992; 4784136; 1376273; 2883593; 2949129; 3014665; 3276809; 3407881; 3604489; 3670025; 3735561; 3801097; 3997705; 4194313; 4259849; 4456457; 4521993; 4784137; 4849673; 4915209; 1441809; 2883594; 2949130; 3014666; 3276810; 3407882; 3604490; 3670026; 3735562; 3801098; 3997706; 4194314; 4259850; 4456458; 4521994; 4784138; 4849674; 4915210; 1507345; 2883595; 2949131; 3014667; 3276811; 3407883; 3604491; 3670027; 3735563; 3801099; 3997707; 4194315; 4259851; 4456459; 4521995; 4784139; 4849675; 4915211; 1572886; 2883596; 2949132; 3014668; 3211276; 3276812; 3407884; 3604492; 3670028; 3735564; 3801100; 3997708; 4063244; 4128780; 4194316; 4259852; 4325388; 4456460; 4521996; 4718604; 4784140; 4849676; 4915212; 1638422; 2883597; 2949133; 3014669; 3211277; 3276813; 3407885; 3604493; 3670029; 3735565; 3801101; 3997709; 4063245; 4128781; 4194317; 4259853; 4325389; 4456461; 4521997; 4718605; 4784141; 4849677; 4915213; 1835026; 2883598; 2949134; 3014670; 3276814; 3407886; 3604494; 3670030; 3735566; 3801102; 3997710; 4128782; 4194318; 4259854; 4456462; 4521998; 4784142; 4849678; 4915214; 1900566; 2883599; 2949135; 3014671; 3211279; 3276815; 3407887; 3604495; 3670031; 3735567; 3801103; 3997711; 4063247; 4128783; 4194319; 4259855; 4325391; 4456463; 4521999; 4718607; 4784143; 4849679; 4915215; 1966102; 2883600; 2949136; 3014672; 3211280; 3276816; 3407888; 3604496; 3670032; 3735568; 3801104; 3997712; 4063248; 4128784; 4194320; 4259856; 4325392; 4456464; 4522000; 4718608; 4784144; 4849680; 4915216; 2031638; 2883601; 2949137; 3014673; 3211281; 3276817; 3407889; 3604497; 3670033; 3735569; 3801105; 3997713; 4063249; 4128785; 4194321; 4259857; 4325393; 4456465; 4522001; 4718609; 4784145; 4849681; 4915217; 2359318; 2883602; 2949138; 3014674; 3211282; 3276818; 3407890; 3604498; 3670034; 3735570; 3801106; 3997714; 4063250; 4128786; 4194322; 4259858; 4325394; 4456466; 4522002; 4718610; 4784146; 4849682; 4915218; 2424854; 2883603; 2949139; 3014675; 3211283; 3276819; 3407891; 3604499; 3670035; 3735571; 3801107; 3997715; 4063251; 4128787; 4194323; 4259859; 4325395; 4456467; 4522003; 4718611; 4784147; 4849683; 4915219; 2490390; 2883604; 2949140; 3014676; 3211284; 3276820; 3407892; 3604500; 3670036; 3735572; 3801108; 3997716; 4063252; 4128788; 4194324; 4259860; 4325396; 4456468; 4522004; 4718612; 4784148; 4849684; 4915220; 2555926; 2883605; 2949141; 3014677; 3211285; 3276821; 3407893; 3604501; 3670037; 3735573; 3801109; 3997717; 4063253; 4128789; 4194325; 4259861; 4325397; 4456469; 4522005; 4718613; 4784149; 4849685; 4915221; 2621462; 2883606; 2949142; 3014678; 3211286; 3276822; 3407894; 3604502; 3670038; 3735574; 3801110; 3997718; 4063254; 4128790; 4194326; 4259862; 4325398; 4456470; 4522006; 4718614; 4784150; 4849686; 4915222; 2752513; 3211287; 2818049; 3211288; 2883590; 3211289; 3276825; 3604505; 3670041; 4849689; 4915225; 3014679; 2883610; 2949146; 3014682; 3211290; 3276826; 3407898; 3604506; 3670042; 3735578; 3801114; 3932186; 3997722; 4063258; 4128794; 4194330; 4259866; 4325402; 4456474; 4522010; 4718618; 4784154; 4849690; 4915226; 3211286; 2883611; 2949147; 3014683; 3211291; 3276827; 3407899; 3604507; 3670043; 3735579; 3801115; 3997723; 4063259; 4128795; 4194331; 4259867; 4325403; 4456475; 4522011; 4718619; 4784155; 4849691; 4915227; 3276809; 2949148; 3407900; 3604508; 3801116; 4194332; 4259868; 4456476; 4522012; 4784156; 3342345; 2949149; 3407901; 3604509; 3801117; 4194333; 4259869; 4456477; 4522013; 4784157; 3473417; 2949150; 3407902; 3604510; 3801118; 4194334; 4259870; 4456478; 4522014; 4784158; 3538953; 2949151; 3407903; 3604511; 3801119; 4194335; 4259871; 4456479; 4522015; 4784159; 3604490; 2949152; 3014688; 3407904; 3604512; 3801120; 4194336; 4259872; 4456480; 4522016; 4784160; 3670027; 2883617; 2949153; 3014689; 3407905; 3604513; 3801121; 4194337; 4259873; 4456481; 4522017; 4784161; 3735563; 2883618; 2949154; 3014690; 3407906; 3604514; 3801122; 4194338; 4259874; 4456482; 4522018; 4784162; 3801094; 3276835; 3604515; 3670051; 3997731; 4849699; 4915235; 3866634; 2949156; 3014692; 3407908; 3604516; 3801124; 4194340; 4259876; 4456484; 4522020; 4784164; 3932170; 2949157; 3014693; 3407909; 3604517; 3801125; 4194341; 4259877; 4456485; 4522021; 4784165; 3997707; 2949158; 3014694; 3407910; 3538982; 3604518; 3801126; 4194342; 4259878; 4456486; 4522022; 4784166; 4063242; 2949159; 3014695; 3407911; 3604519; 3801127; 4194343; 4259879; 4456487; 4522023; 4784167; 4128790; 2883624; 2949160; 3014696; 3211304; 3276840; 3407912; 3604520; 3670056; 3735592; 3801128; 3997736; 4063272; 4128808; 4194344; 4259880; 4325416; 4456488; 4522024; 4718632; 4784168; 4849704; 4915240; 4194326; 2883625; 2949161; 3014697; 3211305; 3276841; 3407913; 3604521; 3670057; 3735593; 3801129; 3997737; 4063273; 4128809; 4194345; 4259881; 4325417; 4456489; 4522025; 4718633; 4784169; 4849705; 4915241; 4259858; 2883626; 2949162; 3014698; 3276842; 3407914; 3604522; 3670058; 3735594; 3801130; 3997738; 4128810; 4194346; 4259882; 4456490; 4522026; 4784170; 4849706; 4915242; 4325398; 2883603; 2949139; 3014675; 3276819; 3407891; 3473451; 3604499; 3670035; 3735571; 3801107; 3997715; 4063251; 4128787; 4194323; 4259859; 4325395; 4456467; 4522003; 4718611; 4784147; 4849683; 4915219; 4390913; 3670060; 4456449; 3473453; 4587528; 2883630; 3276846; 3604526; 3670062; 3735598; 3997742; 4849710; 4915246; 4718600; 2883631; 3276847; 3604527; 3670063; 3735599; 3997743; 4849711; 4915247; 4915209; 2883632; 3145776; 3276848; 3604528; 3670064; 3735600; 3997744; 4849712; 4915248; 4980744; 2883633; 3276849; 3604529; 3670065; 3735601; 3997745; 4849713; 4915249; 5046273; 3080242; 5111809; 3080243; 5308418; 3080244; 3932212; 5373953; 3211317; 5439489; 3211318; 5505025; 3604535; 5570561; 3604536; 5636099; 3604537; 4194361; 4259897; 5701640; 2949178; 3407930; 3604538; 3801146; 4194362; 4259898; 4522042; 4784186; 5767172; 2949179; 3407931; 3801147; 4522043; 5898242; 3407932; 4522044; 6029316; 2949181; 3407933; 3801149; 4522045; 6094852; 2949182; 3407934; 3801150; 4522046; 6160388; 2949183; 3407935; 3801151; 4522047; 6291464; 2949184; 3407936; 3604544; 3801152; 4194368; 4259904; 4522048; 4784192; 6357000; 2949185; 3407937; 3604545; 3801153; 4194369; 4259905; 4522049; 4784193; 6488072; 2949186; 3407938; 3604546; 3801154; 4194370; 4259906; 4522050; 4784194; 6553608; 2949187; 3407939; 3604547; 3801155; 4194371; 4259907; 4522051; 4784195; 6619146; 2949188; 3145796; 3407940; 3604548; 3801156; 3866692; 4194372; 4259908; 4522052; 4784196; 6684682; 2949189; 3145797; 3407941; 3604549; 3801157; 3866693; 4194373; 4259909; 4522053; 4784197; 6750210; 3407942; 4522054; 6881282; 3604551; 4915271; 6946818; 3604552; 4915272; 7143425; 4390985; 7274497; 3407946|]
let reduces = Array.zeroCreate 113
for i = 0 to 112 do
        reduces.[i] <- Array.zeroCreate 76
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
let private lists_zeroReduces = [|[|15|]; [|18|]; [|21; 2|]; [|25|]; [|54|]; [|21|]; [|26|]; [|30|]; [|36|]; [|76|]; [|52|]; [|56|]; [|38|]; [|72|]; [|34|]; [|49|]; [|52; 49|]; [|3|]; [|11|]; [|12|]; [|17|]|]
let private small_zeroReduces =
        [|9; 2949120; 3407872; 3538944; 3604480; 3801088; 4194304; 4259840; 4521984; 4784128; 65544; 2949121; 3407873; 3604481; 3801089; 4194305; 4259841; 4521985; 4784129; 131080; 2949121; 3407873; 3604481; 3801089; 4194305; 4259841; 4521985; 4784129; 393223; 2949122; 3407874; 3604483; 3801090; 4194307; 4259843; 4521986; 458760; 2949124; 3407876; 3604484; 3801092; 4194308; 4259844; 4521988; 4784132; 524295; 2949125; 3407877; 3604483; 3801093; 4194307; 4259843; 4521989; 655361; 3604486; 786434; 3080199; 3932167; 851969; 3080200; 983048; 2883593; 3276809; 3604489; 3670025; 3735561; 3997705; 4849673; 4915209; 1048582; 3276810; 3604490; 3670026; 3997706; 4849674; 4915210; 1376273; 2883595; 2949131; 3014667; 3276811; 3407883; 3604491; 3670027; 3735563; 3801099; 3997707; 4194315; 4259851; 4456459; 4521995; 4784139; 4849675; 4915211; 2097153; 3670028; 2228230; 3276810; 3604490; 3670026; 3997706; 4849674; 4915210; 2424855; 2883597; 2949133; 3014669; 3211277; 3276813; 3407885; 3604493; 3670029; 3735565; 3801101; 3932173; 3997709; 4063245; 4128781; 4194317; 4259853; 4325389; 4456461; 4521997; 4718605; 4784141; 4849677; 4915213; 2490390; 2883598; 2949134; 3014670; 3211278; 3276814; 3407886; 3604494; 3670030; 3735566; 3801102; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4456462; 4521998; 4718606; 4784142; 4849678; 4915214; 3080198; 3276810; 3604490; 3670026; 3997706; 4849674; 4915210; 3407878; 3276810; 3604490; 3670026; 3997706; 4849674; 4915210; 3604496; 2883599; 2949135; 3014671; 3276810; 3407887; 3604496; 3670026; 3801103; 3997706; 4194319; 4259855; 4456463; 4521999; 4784143; 4849674; 4915210; 3670032; 2883599; 2949135; 3014671; 3276810; 3407887; 3604496; 3670026; 3801103; 3997706; 4194319; 4259855; 4456463; 4521999; 4784143; 4849674; 4915210; 3866634; 2949120; 3014656; 3407872; 3604480; 3801088; 4194304; 4259840; 4456448; 4521984; 4784128; 4325398; 2883597; 2949133; 3014669; 3276813; 3407885; 3604493; 3670029; 3735565; 3801101; 3932173; 3997709; 4063245; 4128781; 4194317; 4259853; 4325389; 4456461; 4521997; 4718605; 4784141; 4849677; 4915213; 5046273; 3080200; 5832706; 3407889; 4522001; 5898242; 3407889; 4522001; 6029320; 2949138; 3407890; 3604498; 3801106; 4194322; 4259858; 4522002; 4784146; 6094855; 2949125; 3407877; 3604483; 3801093; 4194307; 4259843; 4521989; 6291464; 2949139; 3407891; 3604499; 3801107; 4194323; 4259859; 4522003; 4784147; 6488072; 2949139; 3407891; 3604499; 3801107; 4194323; 4259859; 4522003; 4784147; 7012353; 3407892|]
let zeroReduces = Array.zeroCreate 113
for i = 0 to 112 do
        zeroReduces.[i] <- Array.zeroCreate 76
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
let private small_acc = [112]
let private accStates = Array.zeroCreate 113
for i = 0 to 112 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 67
let errorNIndex = 6
let errorTIndex = -1
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorNIndex, errorTIndex)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(76, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(72, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(76, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_error * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
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
                      (match ((unbox _rnglr_children.[5]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
                       |> List.iter (fun (_) -> 
                        _rnglr_cycle_res := (
                          
# 114 "Parser.fsy"
                                  
                                  {
                                      info = { fileName = !currentFilename }
                                      head = _S1
                                      grammar = fst _S2 @ fst _S3 @ fst _S4
                                      foot = _S5
                                      options = (snd _S4) |> joinMaps (snd _S3) |> joinMaps (snd _S2)
                                  }
                                
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 108 "Parser.fsy"
               : '_rnglr_type_file) 
# 345 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 108 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 355 "Parser.fs"
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
                
# 125 "Parser.fsy"
                     
                        match fst _S1 with
                        | [] -> [], Map.empty
                        | x ->  defaultModules x, snd _S1
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 124 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 379 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 132 "Parser.fsy"
                          [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 131 "Parser.fsy"
               : '_rnglr_type_modules) 
# 397 "Parser.fs"
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
                  
# 131 "Parser.fsy"
                                              concatModOpt _S1 _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 131 "Parser.fsy"
               : '_rnglr_type_modules) 
# 419 "Parser.fs"
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
                      
# 135 "Parser.fsy"
                           
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
# 134 "Parser.fsy"
               : '_rnglr_type_module_) 
# 453 "Parser.fs"
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
                
# 145 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 145 "Parser.fsy"
               : '_rnglr_type_ident) 
# 473 "Parser.fs"
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
                
# 145 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 145 "Parser.fsy"
               : '_rnglr_type_ident) 
# 493 "Parser.fs"
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
                
# 152 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 148 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 513 "Parser.fs"
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
                  
# 148 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 148 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 538 "Parser.fs"
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
                    
# 154 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 154 "Parser.fsy"
               : '_rnglr_type_openings) 
# 562 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 154 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 154 "Parser.fsy"
               : '_rnglr_type_openings) 
# 580 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 156 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 156 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 598 "Parser.fs"
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
                    
# 156 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 156 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 622 "Parser.fs"
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
                
# 158 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 642 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 158 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 660 "Parser.fs"
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
                  
# 160 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 160 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 682 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 160 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 160 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 700 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 162 "Parser.fsy"
                                                                                               [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 162 "Parser.fsy"
               : '_rnglr_type_includes) 
# 718 "Parser.fs"
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
                  
# 162 "Parser.fsy"
                                                 fst _S1 @ fst _S2, joinMaps (snd _S1) (snd _S2) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 162 "Parser.fsy"
               : '_rnglr_type_includes) 
# 740 "Parser.fs"
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
                  
# 166 "Parser.fsy"
                       
                          let def = parseRules _S2.text in
                          if def.grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          def.grammar, def.options
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 164 "Parser.fsy"
               : '_rnglr_type_include_) 
# 767 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 176 "Parser.fsy"
                      [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 174 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 785 "Parser.fs"
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
                    
# 175 "Parser.fsy"
                            concatModOpt _S1 _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 174 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 809 "Parser.fs"
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
                              
# 180 "Parser.fsy"
                                      
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
# 179 "Parser.fsy"
               : '_rnglr_type_rule) 
# 857 "Parser.fs"
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
                
# 196 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 877 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 196 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 895 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 198 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 913 "Parser.fs"
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
                
# 198 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 933 "Parser.fs"
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
                
# 198 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 953 "Parser.fs"
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
                    
# 200 "Parser.fsy"
                                                                                     Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 200 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 977 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 200 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 200 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
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
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 203 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 202 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1017 "Parser.fs"
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
                
# 202 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 202 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1037 "Parser.fs"
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
                
# 205 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1057 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 205 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1075 "Parser.fs"
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
                  
# 207 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1097 "Parser.fs"
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
               : '_rnglr_type_param_list) 
# 1115 "Parser.fs"
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
                
# 209 "Parser.fsy"
                                               Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1135 "Parser.fs"
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
               : '_rnglr_type_weight_opt) 
# 1153 "Parser.fs"
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
                  
# 211 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1175 "Parser.fs"
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
                
# 211 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1195 "Parser.fs"
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
                  
# 214 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 213 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1217 "Parser.fs"
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
                    
# 213 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 213 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1241 "Parser.fs"
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
                
# 216 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1261 "Parser.fs"
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
                
# 216 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1281 "Parser.fs"
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
                
# 219 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 218 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1301 "Parser.fs"
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
                    
# 218 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 218 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1325 "Parser.fs"
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
                        
# 221 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 221 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1353 "Parser.fs"
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
                  
# 223 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1375 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 223 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1393 "Parser.fs"
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
                    
# 225 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 225 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1417 "Parser.fs"
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
                
# 227 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 227 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1437 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 227 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 227 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1455 "Parser.fs"
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
                
# 229 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1475 "Parser.fs"
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
               : '_rnglr_type_semi_opt) 
# 1493 "Parser.fs"
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
                
# 231 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1513 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 231 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1531 "Parser.fs"
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
                
# 234 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1551 "Parser.fs"
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
                    
# 233 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1575 "Parser.fs"
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
                
# 236 "Parser.fsy"
                                             _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1595 "Parser.fs"
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
                
# 236 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1615 "Parser.fs"
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
                
# 244 "Parser.fsy"
                                           PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1635 "Parser.fs"
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
                
# 243 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1655 "Parser.fs"
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
                
# 242 "Parser.fsy"
                                           _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1675 "Parser.fs"
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
                    
# 241 "Parser.fsy"
                                               _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1699 "Parser.fs"
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
                  
# 240 "Parser.fsy"
                                             POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1721 "Parser.fs"
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
                  
# 239 "Parser.fsy"
                                             PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1743 "Parser.fs"
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
                  
# 238 "Parser.fsy"
                                             PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1765 "Parser.fs"
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
                
# 246 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 246 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1785 "Parser.fs"
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
                  
# 249 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1807 "Parser.fs"
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
                
# 248 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1827 "Parser.fs"
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
                    
# 251 "Parser.fsy"
                                                                         Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1851 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 251 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1869 "Parser.fs"
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
                    
# 255 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 253 "Parser.fsy"
               : '_rnglr_type_call) 
# 1896 "Parser.fs"
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
                
# 253 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 253 "Parser.fsy"
               : '_rnglr_type_call) 
# 1916 "Parser.fs"
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
                  
# 260 "Parser.fsy"
                                                             Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1938 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 260 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1956 "Parser.fs"
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
                    
# 263 "Parser.fsy"
                                                                     (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1980 "Parser.fs"
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
                
# 262 "Parser.fsy"
                                              Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 2000 "Parser.fs"
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
                    
# 265 "Parser.fsy"
                                                        Source.toString _S1, Source.toString _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 265 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 2024 "Parser.fs"
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
                
# 267 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 267 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2044 "Parser.fs"
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
                
# 267 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 267 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2064 "Parser.fs"
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
# 2082 "Parser.fs"
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
