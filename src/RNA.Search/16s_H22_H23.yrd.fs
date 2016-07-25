
# 2 "16s_H22_H23.yrd.fs"
module GLL.r16s.H22_H23
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
type Token =
    | A of (int)
    | C of (int)
    | G of (int)
    | RNGLR_EOF of (int)
    | U of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | C x -> box x
    | G x -> box x
    | RNGLR_EOF x -> box x
    | U x -> box x

let numToString = function
    | 0 -> "any"
    | 1 -> "any_1_2"
    | 2 -> "any_1_3"
    | 3 -> "any_2_3"
    | 4 -> "any_2_4"
    | 5 -> "any_3_4"
    | 6 -> "any_3_5"
    | 7 -> "any_4"
    | 8 -> "any_4_6"
    | 9 -> "any_5_7"
    | 10 -> "any_6_8"
    | 11 -> "any_9_11"
    | 12 -> "error"
    | 13 -> "folded"
    | 14 -> "full"
    | 15 -> "h10"
    | 16 -> "h11"
    | 17 -> "h12"
    | 18 -> "h13"
    | 19 -> "h14"
    | 20 -> "h15"
    | 21 -> "h16"
    | 22 -> "h17"
    | 23 -> "h18"
    | 24 -> "h19"
    | 25 -> "h21"
    | 26 -> "h22"
    | 27 -> "h23"
    | 28 -> "h24"
    | 29 -> "h25"
    | 30 -> "h26"
    | 31 -> "h27"
    | 32 -> "h3"
    | 33 -> "h4"
    | 34 -> "h5"
    | 35 -> "h6"
    | 36 -> "h7"
    | 37 -> "h8"
    | 38 -> "h9"
    | 39 -> "root"
    | 40 -> "root2"
    | 41 -> "s1"
    | 42 -> "s2"
    | 43 -> "s4"
    | 44 -> "s5"
    | 45 -> "s6"
    | 46 -> "s7"
    | 47 -> "s8"
    | 48 -> "yard_exp_brackets_140"
    | 49 -> "yard_exp_brackets_141"
    | 50 -> "yard_exp_brackets_142"
    | 51 -> "yard_exp_brackets_143"
    | 52 -> "yard_exp_brackets_144"
    | 53 -> "yard_exp_brackets_145"
    | 54 -> "yard_exp_brackets_146"
    | 55 -> "yard_exp_brackets_147"
    | 56 -> "yard_exp_brackets_148"
    | 57 -> "yard_exp_brackets_149"
    | 58 -> "yard_exp_brackets_150"
    | 59 -> "yard_exp_brackets_151"
    | 60 -> "yard_exp_brackets_152"
    | 61 -> "yard_exp_brackets_153"
    | 62 -> "yard_exp_brackets_154"
    | 63 -> "yard_exp_brackets_155"
    | 64 -> "yard_exp_brackets_156"
    | 65 -> "yard_exp_brackets_157"
    | 66 -> "yard_exp_brackets_158"
    | 67 -> "yard_exp_brackets_159"
    | 68 -> "yard_exp_brackets_160"
    | 69 -> "yard_exp_brackets_161"
    | 70 -> "yard_exp_brackets_162"
    | 71 -> "yard_exp_brackets_163"
    | 72 -> "yard_exp_brackets_164"
    | 73 -> "yard_exp_brackets_165"
    | 74 -> "yard_exp_brackets_166"
    | 75 -> "yard_exp_brackets_167"
    | 76 -> "yard_exp_brackets_168"
    | 77 -> "yard_exp_brackets_169"
    | 78 -> "yard_exp_brackets_170"
    | 79 -> "yard_exp_brackets_171"
    | 80 -> "yard_exp_brackets_172"
    | 81 -> "yard_exp_brackets_173"
    | 82 -> "yard_exp_brackets_174"
    | 83 -> "yard_exp_brackets_175"
    | 84 -> "yard_exp_brackets_176"
    | 85 -> "yard_exp_brackets_177"
    | 86 -> "yard_exp_brackets_178"
    | 87 -> "yard_exp_brackets_179"
    | 88 -> "yard_exp_brackets_180"
    | 89 -> "yard_exp_brackets_181"
    | 90 -> "yard_exp_brackets_182"
    | 91 -> "yard_exp_brackets_183"
    | 92 -> "yard_exp_brackets_184"
    | 93 -> "yard_exp_brackets_185"
    | 94 -> "yard_exp_brackets_186"
    | 95 -> "yard_exp_brackets_187"
    | 96 -> "yard_exp_brackets_188"
    | 97 -> "yard_exp_brackets_189"
    | 98 -> "yard_exp_brackets_190"
    | 99 -> "yard_exp_brackets_191"
    | 100 -> "yard_exp_brackets_192"
    | 101 -> "yard_exp_brackets_193"
    | 102 -> "yard_exp_brackets_194"
    | 103 -> "yard_exp_brackets_195"
    | 104 -> "yard_exp_brackets_196"
    | 105 -> "yard_exp_brackets_197"
    | 106 -> "yard_exp_brackets_198"
    | 107 -> "yard_exp_brackets_199"
    | 108 -> "yard_exp_brackets_200"
    | 109 -> "yard_exp_brackets_201"
    | 110 -> "yard_exp_brackets_202"
    | 111 -> "yard_exp_brackets_203"
    | 112 -> "yard_exp_brackets_204"
    | 113 -> "yard_exp_brackets_205"
    | 114 -> "yard_exp_brackets_206"
    | 115 -> "yard_opt_1"
    | 116 -> "yard_opt_10"
    | 117 -> "yard_opt_11"
    | 118 -> "yard_opt_12"
    | 119 -> "yard_opt_13"
    | 120 -> "yard_opt_14"
    | 121 -> "yard_opt_15"
    | 122 -> "yard_opt_16"
    | 123 -> "yard_opt_17"
    | 124 -> "yard_opt_18"
    | 125 -> "yard_opt_6"
    | 126 -> "yard_opt_7"
    | 127 -> "yard_opt_8"
    | 128 -> "yard_opt_9"
    | 129 -> "yard_rule_gstem_17"
    | 130 -> "yard_rule_stem_101"
    | 131 -> "yard_rule_stem_104"
    | 132 -> "yard_rule_stem_106"
    | 133 -> "yard_rule_stem_107"
    | 134 -> "yard_rule_stem_108"
    | 135 -> "yard_rule_stem_109"
    | 136 -> "yard_rule_stem_110"
    | 137 -> "yard_rule_stem_111"
    | 138 -> "yard_rule_stem_114"
    | 139 -> "yard_rule_stem_116"
    | 140 -> "yard_rule_stem_117"
    | 141 -> "yard_rule_stem_122"
    | 142 -> "yard_rule_stem_124"
    | 143 -> "yard_rule_stem_126"
    | 144 -> "yard_rule_stem_127"
    | 145 -> "yard_rule_stem_128"
    | 146 -> "yard_rule_stem_129"
    | 147 -> "yard_rule_stem_13"
    | 148 -> "yard_rule_stem_132"
    | 149 -> "yard_rule_stem_134"
    | 150 -> "yard_rule_stem_137"
    | 151 -> "yard_rule_stem_139"
    | 152 -> "yard_rule_stem_14"
    | 153 -> "yard_rule_stem_15"
    | 154 -> "yard_rule_stem_16"
    | 155 -> "yard_rule_stem_18"
    | 156 -> "yard_rule_stem_19"
    | 157 -> "yard_rule_stem_20"
    | 158 -> "yard_rule_stem_21"
    | 159 -> "yard_rule_stem_22"
    | 160 -> "yard_rule_stem_23"
    | 161 -> "yard_rule_stem_24"
    | 162 -> "yard_rule_stem_29"
    | 163 -> "yard_rule_stem_31"
    | 164 -> "yard_rule_stem_33"
    | 165 -> "yard_rule_stem_34"
    | 166 -> "yard_rule_stem_35"
    | 167 -> "yard_rule_stem_36"
    | 168 -> "yard_rule_stem_37"
    | 169 -> "yard_rule_stem_38"
    | 170 -> "yard_rule_stem_39"
    | 171 -> "yard_rule_stem_44"
    | 172 -> "yard_rule_stem_46"
    | 173 -> "yard_rule_stem_48"
    | 174 -> "yard_rule_stem_49"
    | 175 -> "yard_rule_stem_50"
    | 176 -> "yard_rule_stem_51"
    | 177 -> "yard_rule_stem_57"
    | 178 -> "yard_rule_stem_59"
    | 179 -> "yard_rule_stem_64"
    | 180 -> "yard_rule_stem_66"
    | 181 -> "yard_rule_stem_68"
    | 182 -> "yard_rule_stem_71"
    | 183 -> "yard_rule_stem_73"
    | 184 -> "yard_rule_stem_78"
    | 185 -> "yard_rule_stem_80"
    | 186 -> "yard_rule_stem_82"
    | 187 -> "yard_rule_stem_83"
    | 188 -> "yard_rule_stem_88"
    | 189 -> "yard_rule_stem_90"
    | 190 -> "yard_rule_stem_92"
    | 191 -> "yard_rule_stem_97"
    | 192 -> "yard_rule_stem_99"
    | 193 -> "yard_rule_stem_e1_102"
    | 194 -> "yard_rule_stem_e1_112"
    | 195 -> "yard_rule_stem_e1_120"
    | 196 -> "yard_rule_stem_e1_130"
    | 197 -> "yard_rule_stem_e1_135"
    | 198 -> "yard_rule_stem_e1_27"
    | 199 -> "yard_rule_stem_e1_42"
    | 200 -> "yard_rule_stem_e1_54"
    | 201 -> "yard_rule_stem_e1_62"
    | 202 -> "yard_rule_stem_e1_69"
    | 203 -> "yard_rule_stem_e1_76"
    | 204 -> "yard_rule_stem_e1_86"
    | 205 -> "yard_rule_stem_e1_95"
    | 206 -> "yard_rule_stem_e2_118"
    | 207 -> "yard_rule_stem_e2_25"
    | 208 -> "yard_rule_stem_e2_40"
    | 209 -> "yard_rule_stem_e2_52"
    | 210 -> "yard_rule_stem_e2_60"
    | 211 -> "yard_rule_stem_e2_74"
    | 212 -> "yard_rule_stem_e2_84"
    | 213 -> "yard_rule_stem_e2_93"
    | 214 -> "yard_rule_yard_opt_2_103"
    | 215 -> "yard_rule_yard_opt_2_113"
    | 216 -> "yard_rule_yard_opt_2_121"
    | 217 -> "yard_rule_yard_opt_2_131"
    | 218 -> "yard_rule_yard_opt_2_136"
    | 219 -> "yard_rule_yard_opt_2_28"
    | 220 -> "yard_rule_yard_opt_2_43"
    | 221 -> "yard_rule_yard_opt_2_55"
    | 222 -> "yard_rule_yard_opt_2_63"
    | 223 -> "yard_rule_yard_opt_2_70"
    | 224 -> "yard_rule_yard_opt_2_77"
    | 225 -> "yard_rule_yard_opt_2_87"
    | 226 -> "yard_rule_yard_opt_2_96"
    | 227 -> "yard_rule_yard_opt_3_105"
    | 228 -> "yard_rule_yard_opt_3_115"
    | 229 -> "yard_rule_yard_opt_3_123"
    | 230 -> "yard_rule_yard_opt_3_133"
    | 231 -> "yard_rule_yard_opt_3_138"
    | 232 -> "yard_rule_yard_opt_3_30"
    | 233 -> "yard_rule_yard_opt_3_45"
    | 234 -> "yard_rule_yard_opt_3_56"
    | 235 -> "yard_rule_yard_opt_3_65"
    | 236 -> "yard_rule_yard_opt_3_72"
    | 237 -> "yard_rule_yard_opt_3_79"
    | 238 -> "yard_rule_yard_opt_3_89"
    | 239 -> "yard_rule_yard_opt_3_98"
    | 240 -> "yard_rule_yard_opt_4_119"
    | 241 -> "yard_rule_yard_opt_4_26"
    | 242 -> "yard_rule_yard_opt_4_41"
    | 243 -> "yard_rule_yard_opt_4_53"
    | 244 -> "yard_rule_yard_opt_4_61"
    | 245 -> "yard_rule_yard_opt_4_75"
    | 246 -> "yard_rule_yard_opt_4_85"
    | 247 -> "yard_rule_yard_opt_4_94"
    | 248 -> "yard_rule_yard_opt_5_100"
    | 249 -> "yard_rule_yard_opt_5_125"
    | 250 -> "yard_rule_yard_opt_5_32"
    | 251 -> "yard_rule_yard_opt_5_47"
    | 252 -> "yard_rule_yard_opt_5_58"
    | 253 -> "yard_rule_yard_opt_5_67"
    | 254 -> "yard_rule_yard_opt_5_81"
    | 255 -> "yard_rule_yard_opt_5_91"
    | 256 -> "yard_start_rule"
    | 257 -> "A"
    | 258 -> "C"
    | 259 -> "G"
    | 260 -> "RNGLR_EOF"
    | 261 -> "T"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 257
    | C _ -> 258
    | G _ -> 259
    | RNGLR_EOF _ -> 260
    | U _ -> 261

let isLiteral = function
    | A _ -> false
    | C _ -> false
    | G _ -> false
    | RNGLR_EOF _ -> false
    | U _ -> false

let isTerminal = function
    | A _ -> true
    | C _ -> true
    | G _ -> true
    | RNGLR_EOF _ -> true
    | U _ -> true

let numIsTerminal = function
    | 257 -> true
    | 258 -> true
    | 259 -> true
    | 260 -> true
    | 261 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | 8 -> true
    | 9 -> true
    | 10 -> true
    | 11 -> true
    | 12 -> true
    | 13 -> true
    | 14 -> true
    | 15 -> true
    | 16 -> true
    | 17 -> true
    | 18 -> true
    | 19 -> true
    | 20 -> true
    | 21 -> true
    | 22 -> true
    | 23 -> true
    | 24 -> true
    | 25 -> true
    | 26 -> true
    | 27 -> true
    | 28 -> true
    | 29 -> true
    | 30 -> true
    | 31 -> true
    | 32 -> true
    | 33 -> true
    | 34 -> true
    | 35 -> true
    | 36 -> true
    | 37 -> true
    | 38 -> true
    | 39 -> true
    | 40 -> true
    | 41 -> true
    | 42 -> true
    | 43 -> true
    | 44 -> true
    | 45 -> true
    | 46 -> true
    | 47 -> true
    | 48 -> true
    | 49 -> true
    | 50 -> true
    | 51 -> true
    | 52 -> true
    | 53 -> true
    | 54 -> true
    | 55 -> true
    | 56 -> true
    | 57 -> true
    | 58 -> true
    | 59 -> true
    | 60 -> true
    | 61 -> true
    | 62 -> true
    | 63 -> true
    | 64 -> true
    | 65 -> true
    | 66 -> true
    | 67 -> true
    | 68 -> true
    | 69 -> true
    | 70 -> true
    | 71 -> true
    | 72 -> true
    | 73 -> true
    | 74 -> true
    | 75 -> true
    | 76 -> true
    | 77 -> true
    | 78 -> true
    | 79 -> true
    | 80 -> true
    | 81 -> true
    | 82 -> true
    | 83 -> true
    | 84 -> true
    | 85 -> true
    | 86 -> true
    | 87 -> true
    | 88 -> true
    | 89 -> true
    | 90 -> true
    | 91 -> true
    | 92 -> true
    | 93 -> true
    | 94 -> true
    | 95 -> true
    | 96 -> true
    | 97 -> true
    | 98 -> true
    | 99 -> true
    | 100 -> true
    | 101 -> true
    | 102 -> true
    | 103 -> true
    | 104 -> true
    | 105 -> true
    | 106 -> true
    | 107 -> true
    | 108 -> true
    | 109 -> true
    | 110 -> true
    | 111 -> true
    | 112 -> true
    | 113 -> true
    | 114 -> true
    | 115 -> true
    | 116 -> true
    | 117 -> true
    | 118 -> true
    | 119 -> true
    | 120 -> true
    | 121 -> true
    | 122 -> true
    | 123 -> true
    | 124 -> true
    | 125 -> true
    | 126 -> true
    | 127 -> true
    | 128 -> true
    | 129 -> true
    | 130 -> true
    | 131 -> true
    | 132 -> true
    | 133 -> true
    | 134 -> true
    | 135 -> true
    | 136 -> true
    | 137 -> true
    | 138 -> true
    | 139 -> true
    | 140 -> true
    | 141 -> true
    | 142 -> true
    | 143 -> true
    | 144 -> true
    | 145 -> true
    | 146 -> true
    | 147 -> true
    | 148 -> true
    | 149 -> true
    | 150 -> true
    | 151 -> true
    | 152 -> true
    | 153 -> true
    | 154 -> true
    | 155 -> true
    | 156 -> true
    | 157 -> true
    | 158 -> true
    | 159 -> true
    | 160 -> true
    | 161 -> true
    | 162 -> true
    | 163 -> true
    | 164 -> true
    | 165 -> true
    | 166 -> true
    | 167 -> true
    | 168 -> true
    | 169 -> true
    | 170 -> true
    | 171 -> true
    | 172 -> true
    | 173 -> true
    | 174 -> true
    | 175 -> true
    | 176 -> true
    | 177 -> true
    | 178 -> true
    | 179 -> true
    | 180 -> true
    | 181 -> true
    | 182 -> true
    | 183 -> true
    | 184 -> true
    | 185 -> true
    | 186 -> true
    | 187 -> true
    | 188 -> true
    | 189 -> true
    | 190 -> true
    | 191 -> true
    | 192 -> true
    | 193 -> true
    | 194 -> true
    | 195 -> true
    | 196 -> true
    | 197 -> true
    | 198 -> true
    | 199 -> true
    | 200 -> true
    | 201 -> true
    | 202 -> true
    | 203 -> true
    | 204 -> true
    | 205 -> true
    | 206 -> true
    | 207 -> true
    | 208 -> true
    | 209 -> true
    | 210 -> true
    | 211 -> true
    | 212 -> true
    | 213 -> true
    | 214 -> true
    | 215 -> true
    | 216 -> true
    | 217 -> true
    | 218 -> true
    | 219 -> true
    | 220 -> true
    | 221 -> true
    | 222 -> true
    | 223 -> true
    | 224 -> true
    | 225 -> true
    | 226 -> true
    | 227 -> true
    | 228 -> true
    | 229 -> true
    | 230 -> true
    | 231 -> true
    | 232 -> true
    | 233 -> true
    | 234 -> true
    | 235 -> true
    | 236 -> true
    | 237 -> true
    | 238 -> true
    | 239 -> true
    | 240 -> true
    | 241 -> true
    | 242 -> true
    | 243 -> true
    | 244 -> true
    | 245 -> true
    | 246 -> true
    | 247 -> true
    | 248 -> true
    | 249 -> true
    | 250 -> true
    | 251 -> true
    | 252 -> true
    | 253 -> true
    | 254 -> true
    | 255 -> true
    | 256 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|0; 0; 0; 0; 14; 256; 115; 115; 13; 27; 147; 147; 147; 147; 147; 147; 147; 147; 147; 41; 152; 152; 152; 152; 152; 152; 152; 152; 152; 42; 153; 153; 153; 153; 153; 153; 153; 153; 153; 28; 129; 129; 129; 129; 129; 129; 129; 129; 129; 129; 155; 155; 155; 155; 155; 155; 155; 155; 155; 154; 154; 154; 154; 154; 154; 154; 154; 154; 26; 156; 156; 156; 156; 156; 156; 156; 156; 156; 43; 157; 157; 157; 157; 157; 157; 157; 157; 157; 44; 158; 158; 158; 158; 158; 158; 158; 158; 158; 1; 2; 3; 4; 5; 6; 9; 8; 10; 11; 25; 159; 159; 159; 159; 159; 159; 159; 159; 159; 125; 125; 126; 126; 45; 160; 160; 160; 160; 160; 160; 160; 160; 160; 46; 161; 161; 161; 161; 161; 161; 161; 161; 161; 47; 207; 164; 164; 164; 164; 164; 164; 164; 164; 164; 250; 250; 198; 163; 163; 163; 163; 163; 163; 163; 163; 163; 232; 232; 162; 162; 162; 162; 162; 162; 162; 162; 162; 219; 219; 241; 241; 127; 127; 128; 128; 116; 116; 39; 166; 166; 166; 166; 166; 166; 166; 166; 166; 165; 165; 165; 165; 165; 165; 165; 165; 165; 30; 169; 169; 169; 169; 169; 169; 169; 169; 169; 168; 168; 168; 168; 168; 168; 168; 168; 168; 167; 167; 167; 167; 167; 167; 167; 167; 167; 29; 208; 173; 173; 173; 173; 173; 173; 173; 173; 173; 251; 251; 199; 172; 172; 172; 172; 172; 172; 172; 172; 172; 233; 233; 171; 171; 171; 171; 171; 171; 171; 171; 171; 220; 220; 242; 242; 170; 170; 170; 170; 170; 170; 170; 170; 170; 31; 174; 174; 174; 174; 174; 174; 174; 174; 174; 24; 175; 175; 175; 175; 175; 175; 175; 175; 175; 7; 37; 176; 176; 176; 176; 176; 176; 176; 176; 176; 38; 15; 209; 178; 178; 178; 178; 178; 178; 178; 178; 178; 252; 252; 200; 177; 177; 177; 177; 177; 177; 177; 177; 177; 234; 234; 221; 221; 243; 243; 35; 211; 186; 186; 186; 186; 186; 186; 186; 186; 186; 254; 254; 203; 185; 185; 185; 185; 185; 185; 185; 185; 185; 237; 237; 184; 184; 184; 184; 184; 184; 184; 184; 184; 224; 224; 245; 245; 202; 183; 183; 183; 183; 183; 183; 183; 183; 183; 236; 236; 182; 182; 182; 182; 182; 182; 182; 182; 182; 223; 223; 210; 181; 181; 181; 181; 181; 181; 181; 181; 181; 253; 253; 201; 180; 180; 180; 180; 180; 180; 180; 180; 180; 235; 235; 179; 179; 179; 179; 179; 179; 179; 179; 179; 222; 222; 244; 244; 36; 212; 190; 190; 190; 190; 190; 190; 190; 190; 190; 255; 255; 204; 189; 189; 189; 189; 189; 189; 189; 189; 189; 238; 238; 188; 188; 188; 188; 188; 188; 188; 188; 188; 225; 225; 246; 246; 187; 187; 187; 187; 187; 187; 187; 187; 187; 16; 193; 132; 132; 132; 132; 132; 132; 132; 132; 132; 227; 227; 131; 131; 131; 131; 131; 131; 131; 131; 131; 214; 214; 213; 130; 130; 130; 130; 130; 130; 130; 130; 130; 248; 248; 205; 192; 192; 192; 192; 192; 192; 192; 192; 192; 239; 239; 191; 191; 191; 191; 191; 191; 191; 191; 191; 226; 226; 247; 247; 117; 117; 17; 133; 133; 133; 133; 133; 133; 133; 133; 133; 118; 118; 18; 134; 134; 134; 134; 134; 134; 134; 134; 134; 19; 135; 135; 135; 135; 135; 135; 135; 135; 135; 34; 137; 137; 137; 137; 137; 137; 137; 137; 137; 136; 136; 136; 136; 136; 136; 136; 136; 136; 119; 119; 120; 120; 20; 194; 139; 139; 139; 139; 139; 139; 139; 139; 139; 228; 228; 138; 138; 138; 138; 138; 138; 138; 138; 138; 215; 215; 121; 121; 21; 140; 140; 140; 140; 140; 140; 140; 140; 140; 22; 206; 143; 143; 143; 143; 143; 143; 143; 143; 143; 249; 249; 195; 142; 142; 142; 142; 142; 142; 142; 142; 142; 229; 229; 141; 141; 141; 141; 141; 141; 141; 141; 141; 216; 216; 240; 240; 23; 146; 146; 146; 146; 146; 146; 146; 146; 146; 145; 145; 145; 145; 145; 145; 145; 145; 145; 144; 144; 144; 144; 144; 144; 144; 144; 144; 33; 196; 149; 149; 149; 149; 149; 149; 149; 149; 149; 230; 230; 148; 148; 148; 148; 148; 148; 148; 148; 148; 217; 217; 122; 122; 32; 197; 151; 151; 151; 151; 151; 151; 151; 151; 151; 231; 231; 150; 150; 150; 150; 150; 150; 150; 150; 150; 218; 218; 123; 123; 124; 124; 40; 48; 49; 50; 50; 50; 50; 50; 51; 52; 53; 54; 55; 56; 57; 57; 58; 58; 58; 59; 59; 59; 60; 60; 61; 61; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74; 74; 74; 75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96; 97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 110; 110; 110; 111; 111; 111; 111; 111; 111; 112; 112; 112; 112; 112; 113; 113; 113; 113; 113; 114; 114; 114; 114|]
let table = new System.Collections.Generic.Dictionary<int, int[]>(1019)
table.Add(0, [|0|])
table.Add(4, [|1|])
table.Add(2, [|2|])
table.Add(1, [|3|])
table.Add(917504, [|4|])
table.Add(917505, [|4|])
table.Add(917506, [|4|])
table.Add(917508, [|4|])
table.Add(16777216, [|5|])
table.Add(16777217, [|5|])
table.Add(16777218, [|5|])
table.Add(16777220, [|5|])
table.Add(7536643, [|6|])
table.Add(7536640, [|7|])
table.Add(7536641, [|7|])
table.Add(7536642, [|7|])
table.Add(7536644, [|7|])
table.Add(851968, [|8|])
table.Add(851969, [|8|])
table.Add(851970, [|8|])
table.Add(851972, [|8|])
table.Add(1769472, [|9|])
table.Add(1769473, [|9|])
table.Add(1769474, [|9|])
table.Add(1769476, [|9|])
table.Add(9633792, [|10;17;18|])
table.Add(9633796, [|11;15;18|])
table.Add(9633793, [|12;18|])
table.Add(9633794, [|13;14;16;18|])
table.Add(2686976, [|19|])
table.Add(2686977, [|19|])
table.Add(2686978, [|19|])
table.Add(2686980, [|19|])
table.Add(9961472, [|20;27;28|])
table.Add(9961476, [|21;25;28|])
table.Add(9961473, [|22;28|])
table.Add(9961474, [|23;24;26;28|])
table.Add(2752512, [|29|])
table.Add(2752513, [|29|])
table.Add(2752514, [|29|])
table.Add(2752516, [|29|])
table.Add(10027008, [|30;37;38|])
table.Add(10027012, [|31;35;38|])
table.Add(10027009, [|32;38|])
table.Add(10027010, [|33;34;36;38|])
table.Add(1835008, [|39|])
table.Add(1835009, [|39|])
table.Add(1835010, [|39|])
table.Add(1835012, [|39|])
table.Add(8454144, [|40;47;49|])
table.Add(8454148, [|41;45;49|])
table.Add(8454145, [|42;49|])
table.Add(8454146, [|43;44;46;48;49|])
table.Add(10158080, [|50;57;58|])
table.Add(10158084, [|51;55;58|])
table.Add(10158081, [|52;58|])
table.Add(10158082, [|53;54;56;58|])
table.Add(10092544, [|59;66|])
table.Add(10092548, [|60;64|])
table.Add(10092545, [|61|])
table.Add(10092546, [|62;63;65;67|])
table.Add(1703936, [|68|])
table.Add(1703937, [|68|])
table.Add(1703938, [|68|])
table.Add(1703940, [|68|])
table.Add(10223616, [|69;76;77|])
table.Add(10223620, [|70;74;77|])
table.Add(10223617, [|71;77|])
table.Add(10223618, [|72;73;75;77|])
table.Add(2818048, [|78|])
table.Add(2818049, [|78|])
table.Add(2818050, [|78|])
table.Add(2818052, [|78|])
table.Add(10289152, [|79;86;87|])
table.Add(10289156, [|80;84;87|])
table.Add(10289153, [|81;87|])
table.Add(10289154, [|82;83;85;87|])
table.Add(2883584, [|88|])
table.Add(2883585, [|88|])
table.Add(2883586, [|88|])
table.Add(2883588, [|88|])
table.Add(10354688, [|89;96;97|])
table.Add(10354692, [|90;94;97|])
table.Add(10354689, [|91;97|])
table.Add(10354690, [|92;93;95;97|])
table.Add(65536, [|98|])
table.Add(65537, [|98|])
table.Add(65538, [|98|])
table.Add(65540, [|98|])
table.Add(131072, [|99|])
table.Add(131073, [|99|])
table.Add(131074, [|99|])
table.Add(131076, [|99|])
table.Add(196608, [|100|])
table.Add(196609, [|100|])
table.Add(196610, [|100|])
table.Add(196612, [|100|])
table.Add(262144, [|101|])
table.Add(262145, [|101|])
table.Add(262146, [|101|])
table.Add(262148, [|101|])
table.Add(327680, [|102|])
table.Add(327681, [|102|])
table.Add(327682, [|102|])
table.Add(327684, [|102|])
table.Add(393216, [|103|])
table.Add(393217, [|103|])
table.Add(393218, [|103|])
table.Add(393220, [|103|])
table.Add(589824, [|104|])
table.Add(589825, [|104|])
table.Add(589826, [|104|])
table.Add(589828, [|104|])
table.Add(524288, [|105|])
table.Add(524289, [|105|])
table.Add(524290, [|105|])
table.Add(524292, [|105|])
table.Add(655360, [|106|])
table.Add(655361, [|106|])
table.Add(655362, [|106|])
table.Add(655364, [|106|])
table.Add(720896, [|107|])
table.Add(720897, [|107|])
table.Add(720898, [|107|])
table.Add(720900, [|107|])
table.Add(1638400, [|108|])
table.Add(1638401, [|108|])
table.Add(1638402, [|108|])
table.Add(1638404, [|108|])
table.Add(10420224, [|109;116;117|])
table.Add(10420228, [|110;114;117|])
table.Add(10420225, [|111;117|])
table.Add(10420226, [|112;113;115;117|])
table.Add(8192000, [|118;119|])
table.Add(8192001, [|118;119|])
table.Add(8192002, [|118;119|])
table.Add(8192004, [|118;119|])
table.Add(8257536, [|120;121|])
table.Add(8257537, [|120;121|])
table.Add(8257538, [|120;121|])
table.Add(8257540, [|120;121|])
table.Add(2949120, [|122|])
table.Add(2949121, [|122|])
table.Add(2949122, [|122|])
table.Add(2949124, [|122|])
table.Add(10485760, [|123;130;131|])
table.Add(10485764, [|124;128;131|])
table.Add(10485761, [|125;131|])
table.Add(10485762, [|126;127;129;131|])
table.Add(3014656, [|132|])
table.Add(3014657, [|132|])
table.Add(3014658, [|132|])
table.Add(3014660, [|132|])
table.Add(10551296, [|133;140;141|])
table.Add(10551300, [|134;138;141|])
table.Add(10551297, [|135;141|])
table.Add(10551298, [|136;137;139;141|])
table.Add(3080192, [|142|])
table.Add(3080193, [|142|])
table.Add(3080194, [|142|])
table.Add(3080196, [|142|])
table.Add(13565952, [|143|])
table.Add(13565953, [|143|])
table.Add(13565954, [|143|])
table.Add(13565956, [|143|])
table.Add(10747904, [|144;151;152|])
table.Add(10747908, [|145;149;152|])
table.Add(10747905, [|146;152|])
table.Add(10747906, [|147;148;150;152|])
table.Add(16384000, [|153;154|])
table.Add(16384001, [|153;154|])
table.Add(16384002, [|153;154|])
table.Add(16384004, [|153;154|])
table.Add(12976128, [|155|])
table.Add(12976129, [|155|])
table.Add(12976130, [|155|])
table.Add(12976132, [|155|])
table.Add(10682368, [|156;163;164|])
table.Add(10682372, [|157;161;164|])
table.Add(10682369, [|158;164|])
table.Add(10682370, [|159;160;162;164|])
table.Add(15204352, [|165;166|])
table.Add(15204353, [|165;166|])
table.Add(15204354, [|165;166|])
table.Add(15204356, [|165;166|])
table.Add(10616832, [|167;174;175|])
table.Add(10616836, [|168;172;175|])
table.Add(10616833, [|169;175|])
table.Add(10616834, [|170;171;173;175|])
table.Add(14352384, [|176;177|])
table.Add(14352385, [|176;177|])
table.Add(14352386, [|176;177|])
table.Add(14352388, [|176;177|])
table.Add(15794176, [|178;179|])
table.Add(15794177, [|178;179|])
table.Add(15794178, [|178;179|])
table.Add(15794180, [|178;179|])
table.Add(8323073, [|180;181|])
table.Add(8323072, [|181|])
table.Add(8323074, [|181|])
table.Add(8323076, [|181|])
table.Add(8388608, [|182;183|])
table.Add(8388609, [|183|])
table.Add(8388610, [|183|])
table.Add(8388612, [|183|])
table.Add(7602176, [|184;185|])
table.Add(7602177, [|184;185|])
table.Add(7602178, [|184;185|])
table.Add(7602180, [|184;185|])
table.Add(2555904, [|186|])
table.Add(2555905, [|186|])
table.Add(2555906, [|186|])
table.Add(2555908, [|186|])
table.Add(10878976, [|187;194;195|])
table.Add(10878980, [|188;192;195|])
table.Add(10878977, [|189;195|])
table.Add(10878978, [|190;191;193;195|])
table.Add(10813440, [|196;203;204|])
table.Add(10813444, [|197;201;204|])
table.Add(10813441, [|198;204|])
table.Add(10813442, [|199;200;202;204|])
table.Add(1966080, [|205|])
table.Add(1966081, [|205|])
table.Add(1966082, [|205|])
table.Add(1966084, [|205|])
table.Add(11075584, [|206;213;214|])
table.Add(11075588, [|207;211;214|])
table.Add(11075585, [|208;214|])
table.Add(11075586, [|209;210;212;214|])
table.Add(11010048, [|215;222;223|])
table.Add(11010052, [|216;220;223|])
table.Add(11010049, [|217;223|])
table.Add(11010050, [|218;219;221;223|])
table.Add(10944512, [|224;231;232|])
table.Add(10944516, [|225;229;232|])
table.Add(10944513, [|226;232|])
table.Add(10944514, [|227;228;230;232|])
table.Add(1900544, [|233|])
table.Add(1900545, [|233|])
table.Add(1900546, [|233|])
table.Add(1900548, [|233|])
table.Add(13631488, [|234|])
table.Add(13631489, [|234|])
table.Add(13631490, [|234|])
table.Add(13631492, [|234|])
table.Add(11337728, [|235;242;243|])
table.Add(11337732, [|236;240;243|])
table.Add(11337729, [|237;243|])
table.Add(11337730, [|238;239;241;243|])
table.Add(16449536, [|244;245|])
table.Add(16449537, [|244;245|])
table.Add(16449538, [|244;245|])
table.Add(16449540, [|244;245|])
table.Add(13041664, [|246|])
table.Add(13041665, [|246|])
table.Add(13041666, [|246|])
table.Add(13041668, [|246|])
table.Add(11272192, [|247;254;255|])
table.Add(11272196, [|248;252;255|])
table.Add(11272193, [|249;255|])
table.Add(11272194, [|250;251;253;255|])
table.Add(15269888, [|256;257|])
table.Add(15269889, [|256;257|])
table.Add(15269890, [|256;257|])
table.Add(15269892, [|256;257|])
table.Add(11206656, [|258;265;266|])
table.Add(11206660, [|259;263;266|])
table.Add(11206657, [|260;266|])
table.Add(11206658, [|261;262;264;266|])
table.Add(14417920, [|267;268|])
table.Add(14417921, [|267;268|])
table.Add(14417922, [|267;268|])
table.Add(14417924, [|267;268|])
table.Add(15859712, [|269;270|])
table.Add(15859713, [|269;270|])
table.Add(15859714, [|269;270|])
table.Add(15859716, [|269;270|])
table.Add(11141120, [|271;278;279|])
table.Add(11141124, [|272;276;279|])
table.Add(11141121, [|273;279|])
table.Add(11141122, [|274;275;277;279|])
table.Add(2031616, [|280|])
table.Add(2031617, [|280|])
table.Add(2031618, [|280|])
table.Add(2031620, [|280|])
table.Add(11403264, [|281;288;289|])
table.Add(11403268, [|282;286;289|])
table.Add(11403265, [|283;289|])
table.Add(11403266, [|284;285;287;289|])
table.Add(1572864, [|290|])
table.Add(1572865, [|290|])
table.Add(1572866, [|290|])
table.Add(1572868, [|290|])
table.Add(11468800, [|291;298;299|])
table.Add(11468804, [|292;296;299|])
table.Add(11468801, [|293;299|])
table.Add(11468802, [|294;295;297;299|])
table.Add(458752, [|300|])
table.Add(458753, [|300|])
table.Add(458754, [|300|])
table.Add(458756, [|300|])
table.Add(2424832, [|301|])
table.Add(2424833, [|301|])
table.Add(2424834, [|301|])
table.Add(2424836, [|301|])
table.Add(11534336, [|302;309;310|])
table.Add(11534340, [|303;307;310|])
table.Add(11534337, [|304;310|])
table.Add(11534338, [|305;306;308;310|])
table.Add(2490368, [|311|])
table.Add(2490369, [|311|])
table.Add(2490370, [|311|])
table.Add(2490372, [|311|])
table.Add(983040, [|312|])
table.Add(983041, [|312|])
table.Add(983042, [|312|])
table.Add(983044, [|312|])
table.Add(13697024, [|313|])
table.Add(13697025, [|313|])
table.Add(13697026, [|313|])
table.Add(13697028, [|313|])
table.Add(11665408, [|314;321;322|])
table.Add(11665412, [|315;319;322|])
table.Add(11665409, [|316;322|])
table.Add(11665410, [|317;318;320;322|])
table.Add(16515072, [|323;324|])
table.Add(16515073, [|323;324|])
table.Add(16515074, [|323;324|])
table.Add(16515076, [|323;324|])
table.Add(13107200, [|325|])
table.Add(13107201, [|325|])
table.Add(13107202, [|325|])
table.Add(13107204, [|325|])
table.Add(11599872, [|326;333;334|])
table.Add(11599876, [|327;331;334|])
table.Add(11599873, [|328;334|])
table.Add(11599874, [|329;330;332;334|])
table.Add(15335424, [|335;336|])
table.Add(15335425, [|335;336|])
table.Add(15335426, [|335;336|])
table.Add(15335428, [|335;336|])
table.Add(14483456, [|337;338|])
table.Add(14483457, [|337;338|])
table.Add(14483458, [|337;338|])
table.Add(14483460, [|337;338|])
table.Add(15925248, [|339;340|])
table.Add(15925249, [|339;340|])
table.Add(15925250, [|339;340|])
table.Add(15925252, [|339;340|])
table.Add(2293760, [|341|])
table.Add(2293761, [|341|])
table.Add(2293762, [|341|])
table.Add(2293764, [|341|])
table.Add(13828096, [|342|])
table.Add(13828097, [|342|])
table.Add(13828098, [|342|])
table.Add(13828100, [|342|])
table.Add(12189696, [|343;350;351|])
table.Add(12189700, [|344;348;351|])
table.Add(12189697, [|345;351|])
table.Add(12189698, [|346;347;349;351|])
table.Add(16646144, [|352;353|])
table.Add(16646145, [|352;353|])
table.Add(16646146, [|352;353|])
table.Add(16646148, [|352;353|])
table.Add(13303808, [|354|])
table.Add(13303809, [|354|])
table.Add(13303810, [|354|])
table.Add(13303812, [|354|])
table.Add(12124160, [|355;362;363|])
table.Add(12124164, [|356;360;363|])
table.Add(12124161, [|357;363|])
table.Add(12124162, [|358;359;361;363|])
table.Add(15532032, [|364;365|])
table.Add(15532033, [|364;365|])
table.Add(15532034, [|364;365|])
table.Add(15532036, [|364;365|])
table.Add(12058624, [|366;373;374|])
table.Add(12058628, [|367;371;374|])
table.Add(12058625, [|368;374|])
table.Add(12058626, [|369;370;372;374|])
table.Add(14680064, [|375;376|])
table.Add(14680065, [|375;376|])
table.Add(14680066, [|375;376|])
table.Add(14680068, [|375;376|])
table.Add(16056320, [|377;378|])
table.Add(16056321, [|377;378|])
table.Add(16056322, [|377;378|])
table.Add(16056324, [|377;378|])
table.Add(13238272, [|379|])
table.Add(13238273, [|379|])
table.Add(13238274, [|379|])
table.Add(13238276, [|379|])
table.Add(11993088, [|380;387;388|])
table.Add(11993092, [|381;385;388|])
table.Add(11993089, [|382;388|])
table.Add(11993090, [|383;384;386;388|])
table.Add(15466496, [|389;390|])
table.Add(15466497, [|389;390|])
table.Add(15466498, [|389;390|])
table.Add(15466500, [|389;390|])
table.Add(11927552, [|391;398;399|])
table.Add(11927556, [|392;396;399|])
table.Add(11927553, [|393;399|])
table.Add(11927554, [|394;395;397;399|])
table.Add(14614528, [|400;401|])
table.Add(14614529, [|400;401|])
table.Add(14614530, [|400;401|])
table.Add(14614532, [|400;401|])
table.Add(13762560, [|402|])
table.Add(13762561, [|402|])
table.Add(13762562, [|402|])
table.Add(13762564, [|402|])
table.Add(11862016, [|403;410;411|])
table.Add(11862020, [|404;408;411|])
table.Add(11862017, [|405;411|])
table.Add(11862018, [|406;407;409;411|])
table.Add(16580608, [|412;413|])
table.Add(16580609, [|412;413|])
table.Add(16580610, [|412;413|])
table.Add(16580612, [|412;413|])
table.Add(13172736, [|414|])
table.Add(13172737, [|414|])
table.Add(13172738, [|414|])
table.Add(13172740, [|414|])
table.Add(11796480, [|415;422;423|])
table.Add(11796484, [|416;420;423|])
table.Add(11796481, [|417;423|])
table.Add(11796482, [|418;419;421;423|])
table.Add(15400960, [|424;425|])
table.Add(15400961, [|424;425|])
table.Add(15400962, [|424;425|])
table.Add(15400964, [|424;425|])
table.Add(11730944, [|426;433;434|])
table.Add(11730948, [|427;431;434|])
table.Add(11730945, [|428;434|])
table.Add(11730946, [|429;430;432;434|])
table.Add(14548992, [|435;436|])
table.Add(14548993, [|435;436|])
table.Add(14548994, [|435;436|])
table.Add(14548996, [|435;436|])
table.Add(15990784, [|437;438|])
table.Add(15990785, [|437;438|])
table.Add(15990786, [|437;438|])
table.Add(15990788, [|437;438|])
table.Add(2359296, [|439|])
table.Add(2359297, [|439|])
table.Add(2359298, [|439|])
table.Add(2359300, [|439|])
table.Add(13893632, [|440|])
table.Add(13893633, [|440|])
table.Add(13893634, [|440|])
table.Add(13893636, [|440|])
table.Add(12451840, [|441;448;449|])
table.Add(12451844, [|442;446;449|])
table.Add(12451841, [|443;449|])
table.Add(12451842, [|444;445;447;449|])
table.Add(16711680, [|450;451|])
table.Add(16711681, [|450;451|])
table.Add(16711682, [|450;451|])
table.Add(16711684, [|450;451|])
table.Add(13369344, [|452|])
table.Add(13369345, [|452|])
table.Add(13369346, [|452|])
table.Add(13369348, [|452|])
table.Add(12386304, [|453;460;461|])
table.Add(12386308, [|454;458;461|])
table.Add(12386305, [|455;461|])
table.Add(12386306, [|456;457;459;461|])
table.Add(15597568, [|462;463|])
table.Add(15597569, [|462;463|])
table.Add(15597570, [|462;463|])
table.Add(15597572, [|462;463|])
table.Add(12320768, [|464;471;472|])
table.Add(12320772, [|465;469;472|])
table.Add(12320769, [|466;472|])
table.Add(12320770, [|467;468;470;472|])
table.Add(14745600, [|473;474|])
table.Add(14745601, [|473;474|])
table.Add(14745602, [|473;474|])
table.Add(14745604, [|473;474|])
table.Add(16121856, [|475;476|])
table.Add(16121857, [|475;476|])
table.Add(16121858, [|475;476|])
table.Add(16121860, [|475;476|])
table.Add(12255232, [|477;484;485|])
table.Add(12255236, [|478;482;485|])
table.Add(12255233, [|479;485|])
table.Add(12255234, [|480;481;483;485|])
table.Add(1048576, [|486|])
table.Add(1048577, [|486|])
table.Add(1048578, [|486|])
table.Add(1048580, [|486|])
table.Add(12648448, [|487|])
table.Add(12648449, [|487|])
table.Add(12648450, [|487|])
table.Add(12648452, [|487|])
table.Add(8650752, [|488;495;496|])
table.Add(8650756, [|489;493;496|])
table.Add(8650753, [|490;496|])
table.Add(8650754, [|491;492;494;496|])
table.Add(14876672, [|497;498|])
table.Add(14876673, [|497;498|])
table.Add(14876674, [|497;498|])
table.Add(14876676, [|497;498|])
table.Add(8585216, [|499;506;507|])
table.Add(8585220, [|500;504;507|])
table.Add(8585217, [|501;507|])
table.Add(8585218, [|502;503;505;507|])
table.Add(14024704, [|508;509|])
table.Add(14024705, [|508;509|])
table.Add(14024706, [|508;509|])
table.Add(14024708, [|508;509|])
table.Add(13959168, [|510|])
table.Add(13959169, [|510|])
table.Add(13959170, [|510|])
table.Add(13959172, [|510|])
table.Add(8519680, [|511;518;519|])
table.Add(8519684, [|512;516;519|])
table.Add(8519681, [|513;519|])
table.Add(8519682, [|514;515;517;519|])
table.Add(16252928, [|520;521|])
table.Add(16252929, [|520;521|])
table.Add(16252930, [|520;521|])
table.Add(16252932, [|520;521|])
table.Add(13434880, [|522|])
table.Add(13434881, [|522|])
table.Add(13434882, [|522|])
table.Add(13434884, [|522|])
table.Add(12582912, [|523;530;531|])
table.Add(12582916, [|524;528;531|])
table.Add(12582913, [|525;531|])
table.Add(12582914, [|526;527;529;531|])
table.Add(15663104, [|532;533|])
table.Add(15663105, [|532;533|])
table.Add(15663106, [|532;533|])
table.Add(15663108, [|532;533|])
table.Add(12517376, [|534;541;542|])
table.Add(12517380, [|535;539;542|])
table.Add(12517377, [|536;542|])
table.Add(12517378, [|537;538;540;542|])
table.Add(14811136, [|543;544|])
table.Add(14811137, [|543;544|])
table.Add(14811138, [|543;544|])
table.Add(14811140, [|543;544|])
table.Add(16187392, [|545;546|])
table.Add(16187393, [|545;546|])
table.Add(16187394, [|545;546|])
table.Add(16187396, [|545;546|])
table.Add(7667712, [|547;548|])
table.Add(7667713, [|547;548|])
table.Add(7667714, [|547;548|])
table.Add(7667716, [|547;548|])
table.Add(1114112, [|549|])
table.Add(1114113, [|549|])
table.Add(1114114, [|549|])
table.Add(1114116, [|549|])
table.Add(8716288, [|550;557;558|])
table.Add(8716292, [|551;555;558|])
table.Add(8716289, [|552;558|])
table.Add(8716290, [|553;554;556;558|])
table.Add(7733248, [|559;560|])
table.Add(7733249, [|559;560|])
table.Add(7733250, [|559;560|])
table.Add(7733252, [|559;560|])
table.Add(1179648, [|561|])
table.Add(1179649, [|561|])
table.Add(1179650, [|561|])
table.Add(1179652, [|561|])
table.Add(8781824, [|562;569;570|])
table.Add(8781828, [|563;567;570|])
table.Add(8781825, [|564;570|])
table.Add(8781826, [|565;566;568;570|])
table.Add(1245184, [|571|])
table.Add(1245185, [|571|])
table.Add(1245186, [|571|])
table.Add(1245188, [|571|])
table.Add(8847360, [|572;579|])
table.Add(8847364, [|573;577;580|])
table.Add(8847361, [|574|])
table.Add(8847362, [|575;576;578|])
table.Add(2228224, [|581|])
table.Add(2228225, [|581|])
table.Add(2228226, [|581|])
table.Add(2228228, [|581|])
table.Add(8978432, [|582;589;590|])
table.Add(8978436, [|583;587;590|])
table.Add(8978433, [|584;590|])
table.Add(8978434, [|585;586;588;590|])
table.Add(8912896, [|591;598;599|])
table.Add(8912900, [|592;596;599|])
table.Add(8912897, [|593;599|])
table.Add(8912898, [|594;595;597;599|])
table.Add(7798784, [|600;601|])
table.Add(7798785, [|600;601|])
table.Add(7798786, [|600;601|])
table.Add(7798788, [|600;601|])
table.Add(7864320, [|602;603|])
table.Add(7864321, [|602;603|])
table.Add(7864322, [|602;603|])
table.Add(7864324, [|602;603|])
table.Add(1310720, [|604|])
table.Add(1310721, [|604|])
table.Add(1310722, [|604|])
table.Add(1310724, [|604|])
table.Add(12713984, [|605|])
table.Add(12713985, [|605|])
table.Add(12713986, [|605|])
table.Add(12713988, [|605|])
table.Add(9109504, [|606;613;614|])
table.Add(9109508, [|607;611;614|])
table.Add(9109505, [|608;614|])
table.Add(9109506, [|609;610;612;614|])
table.Add(14942208, [|615;616|])
table.Add(14942209, [|615;616|])
table.Add(14942210, [|615;616|])
table.Add(14942212, [|615;616|])
table.Add(9043968, [|617;624;625|])
table.Add(9043972, [|618;622;625|])
table.Add(9043969, [|619;625|])
table.Add(9043970, [|620;621;623;625|])
table.Add(14090240, [|626;627|])
table.Add(14090241, [|626;627|])
table.Add(14090242, [|626;627|])
table.Add(14090244, [|626;627|])
table.Add(7929856, [|628;629|])
table.Add(7929857, [|628;629|])
table.Add(7929858, [|628;629|])
table.Add(7929860, [|628;629|])
table.Add(1376256, [|630|])
table.Add(1376257, [|630|])
table.Add(1376258, [|630|])
table.Add(1376260, [|630|])
table.Add(9175040, [|631;638;639|])
table.Add(9175044, [|632;636;639|])
table.Add(9175041, [|633;639|])
table.Add(9175042, [|634;635;637;639|])
table.Add(1441792, [|640|])
table.Add(1441793, [|640|])
table.Add(1441794, [|640|])
table.Add(1441796, [|640|])
table.Add(13500416, [|641|])
table.Add(13500417, [|641|])
table.Add(13500418, [|641|])
table.Add(13500420, [|641|])
table.Add(9371648, [|642;649;650|])
table.Add(9371652, [|643;647;650|])
table.Add(9371649, [|644;650|])
table.Add(9371650, [|645;646;648;650|])
table.Add(16318464, [|651;652|])
table.Add(16318465, [|651;652|])
table.Add(16318466, [|651;652|])
table.Add(16318468, [|651;652|])
table.Add(12779520, [|653|])
table.Add(12779521, [|653|])
table.Add(12779522, [|653|])
table.Add(12779524, [|653|])
table.Add(9306112, [|654;661;662|])
table.Add(9306116, [|655;659;662|])
table.Add(9306113, [|656;662|])
table.Add(9306114, [|657;658;660;662|])
table.Add(15007744, [|663;664|])
table.Add(15007745, [|663;664|])
table.Add(15007746, [|663;664|])
table.Add(15007748, [|663;664|])
table.Add(9240576, [|665;672;673|])
table.Add(9240580, [|666;670;673|])
table.Add(9240577, [|667;673|])
table.Add(9240578, [|668;669;671;673|])
table.Add(14155776, [|674;675|])
table.Add(14155777, [|674;675|])
table.Add(14155778, [|674;675|])
table.Add(14155780, [|674;675|])
table.Add(15728640, [|676;677|])
table.Add(15728641, [|676;677|])
table.Add(15728642, [|676;677|])
table.Add(15728644, [|676;677|])
table.Add(1507328, [|678|])
table.Add(1507329, [|678|])
table.Add(1507330, [|678|])
table.Add(1507332, [|678|])
table.Add(9568256, [|679;686;687|])
table.Add(9568260, [|680;684;687|])
table.Add(9568257, [|681;687|])
table.Add(9568258, [|682;683;685;687|])
table.Add(9502720, [|688;695;696|])
table.Add(9502724, [|689;693;696|])
table.Add(9502721, [|690;696|])
table.Add(9502722, [|691;692;694;696|])
table.Add(9437184, [|697;704;705|])
table.Add(9437188, [|698;702;705|])
table.Add(9437185, [|699;705|])
table.Add(9437186, [|700;701;703;705|])
table.Add(2162688, [|706|])
table.Add(2162689, [|706|])
table.Add(2162690, [|706|])
table.Add(2162692, [|706|])
table.Add(12845056, [|707|])
table.Add(12845057, [|707|])
table.Add(12845058, [|707|])
table.Add(12845060, [|707|])
table.Add(9764864, [|708;715;716|])
table.Add(9764868, [|709;713;716|])
table.Add(9764865, [|710;716|])
table.Add(9764866, [|711;712;714;716|])
table.Add(15073280, [|717;718|])
table.Add(15073281, [|717;718|])
table.Add(15073282, [|717;718|])
table.Add(15073284, [|717;718|])
table.Add(9699328, [|719;726;727|])
table.Add(9699332, [|720;724;727|])
table.Add(9699329, [|721;727|])
table.Add(9699330, [|722;723;725;727|])
table.Add(14221312, [|728;729|])
table.Add(14221313, [|728;729|])
table.Add(14221314, [|728;729|])
table.Add(14221316, [|728;729|])
table.Add(7995392, [|730;731|])
table.Add(7995393, [|730;731|])
table.Add(7995394, [|730;731|])
table.Add(7995396, [|730;731|])
table.Add(2097152, [|732|])
table.Add(2097153, [|732|])
table.Add(2097154, [|732|])
table.Add(2097156, [|732|])
table.Add(12910592, [|733|])
table.Add(12910593, [|733|])
table.Add(12910594, [|733|])
table.Add(12910596, [|733|])
table.Add(9895936, [|734;741;742|])
table.Add(9895940, [|735;739;742|])
table.Add(9895937, [|736;742|])
table.Add(9895938, [|737;738;740;742|])
table.Add(15138816, [|743;744|])
table.Add(15138817, [|743;744|])
table.Add(15138818, [|743;744|])
table.Add(15138820, [|743;744|])
table.Add(9830400, [|745;752;753|])
table.Add(9830404, [|746;750;753|])
table.Add(9830401, [|747;753|])
table.Add(9830402, [|748;749;751;753|])
table.Add(14286848, [|754;755|])
table.Add(14286849, [|754;755|])
table.Add(14286850, [|754;755|])
table.Add(14286852, [|754;755|])
table.Add(8060928, [|756;757|])
table.Add(8060929, [|756;757|])
table.Add(8060930, [|756;757|])
table.Add(8060932, [|756;757|])
table.Add(8126464, [|758;759|])
table.Add(8126465, [|758;759|])
table.Add(8126466, [|758;759|])
table.Add(8126468, [|758;759|])
table.Add(2621440, [|760|])
table.Add(2621441, [|760|])
table.Add(2621442, [|760|])
table.Add(2621444, [|760|])
table.Add(3145728, [|761|])
table.Add(3145729, [|761|])
table.Add(3145730, [|761|])
table.Add(3145732, [|761|])
table.Add(3211264, [|762|])
table.Add(3211265, [|762|])
table.Add(3211266, [|762|])
table.Add(3211268, [|762|])
table.Add(3276800, [|763;764;765;766;767|])
table.Add(3276801, [|763;764;765;766;767|])
table.Add(3276802, [|763;764;765;766;767|])
table.Add(3276804, [|763;764;765;766;767|])
table.Add(3342336, [|768|])
table.Add(3342337, [|768|])
table.Add(3342338, [|768|])
table.Add(3342340, [|768|])
table.Add(3407872, [|769|])
table.Add(3407873, [|769|])
table.Add(3407874, [|769|])
table.Add(3407876, [|769|])
table.Add(3473410, [|770|])
table.Add(3538944, [|771|])
table.Add(3538945, [|771|])
table.Add(3538946, [|771|])
table.Add(3538948, [|771|])
table.Add(3604480, [|772|])
table.Add(3604481, [|772|])
table.Add(3604482, [|772|])
table.Add(3604484, [|772|])
table.Add(3670016, [|773|])
table.Add(3670017, [|773|])
table.Add(3670018, [|773|])
table.Add(3670020, [|773|])
table.Add(3735552, [|774;775|])
table.Add(3735553, [|774;775|])
table.Add(3735554, [|774;775|])
table.Add(3735556, [|774;775|])
table.Add(3801088, [|776;777;778|])
table.Add(3801089, [|776;777;778|])
table.Add(3801090, [|776;777;778|])
table.Add(3801092, [|776;777;778|])
table.Add(3866624, [|779;780;781|])
table.Add(3866625, [|779;780;781|])
table.Add(3866626, [|779;780;781|])
table.Add(3866628, [|779;780;781|])
table.Add(3932160, [|782;783|])
table.Add(3932161, [|782;783|])
table.Add(3932162, [|782;783|])
table.Add(3932164, [|782;783|])
table.Add(3997696, [|784;785;786|])
table.Add(3997697, [|784;785;786|])
table.Add(3997698, [|784;785;786|])
table.Add(3997700, [|784;785;786|])
table.Add(4063232, [|787|])
table.Add(4063233, [|787|])
table.Add(4063234, [|787|])
table.Add(4063236, [|787|])
table.Add(4128768, [|788|])
table.Add(4128769, [|788|])
table.Add(4128770, [|788|])
table.Add(4128772, [|788|])
table.Add(4194304, [|789|])
table.Add(4194305, [|789|])
table.Add(4194306, [|789|])
table.Add(4194308, [|789|])
table.Add(4259840, [|790|])
table.Add(4259841, [|790|])
table.Add(4259842, [|790|])
table.Add(4259844, [|790|])
table.Add(4325376, [|791|])
table.Add(4325377, [|791|])
table.Add(4325378, [|791|])
table.Add(4325380, [|791|])
table.Add(4390912, [|792|])
table.Add(4390913, [|792|])
table.Add(4390914, [|792|])
table.Add(4390916, [|792|])
table.Add(4456448, [|793|])
table.Add(4456449, [|793|])
table.Add(4456450, [|793|])
table.Add(4456452, [|793|])
table.Add(4521984, [|794|])
table.Add(4521985, [|794|])
table.Add(4521986, [|794|])
table.Add(4521988, [|794|])
table.Add(4587520, [|795|])
table.Add(4587521, [|795|])
table.Add(4587522, [|795|])
table.Add(4587524, [|795|])
table.Add(4653056, [|796|])
table.Add(4653057, [|796|])
table.Add(4653058, [|796|])
table.Add(4653060, [|796|])
table.Add(4718592, [|797|])
table.Add(4718593, [|797|])
table.Add(4718594, [|797|])
table.Add(4718596, [|797|])
table.Add(4784128, [|798|])
table.Add(4784129, [|798|])
table.Add(4784130, [|798|])
table.Add(4784132, [|798|])
table.Add(4849664, [|799;800;801|])
table.Add(4849665, [|799;800;801|])
table.Add(4849666, [|799;800;801|])
table.Add(4849668, [|799;800;801|])
table.Add(4915200, [|802|])
table.Add(4915201, [|802|])
table.Add(4915202, [|802|])
table.Add(4915204, [|802|])
table.Add(4980736, [|803|])
table.Add(4980737, [|803|])
table.Add(4980738, [|803|])
table.Add(4980740, [|803|])
table.Add(5046272, [|804|])
table.Add(5046273, [|804|])
table.Add(5046274, [|804|])
table.Add(5046276, [|804|])
table.Add(5111808, [|805|])
table.Add(5111809, [|805|])
table.Add(5111810, [|805|])
table.Add(5111812, [|805|])
table.Add(5177344, [|806|])
table.Add(5177345, [|806|])
table.Add(5177346, [|806|])
table.Add(5177348, [|806|])
table.Add(5242880, [|807|])
table.Add(5242881, [|807|])
table.Add(5242882, [|807|])
table.Add(5242884, [|807|])
table.Add(5308416, [|808|])
table.Add(5308417, [|808|])
table.Add(5308418, [|808|])
table.Add(5308420, [|808|])
table.Add(5373952, [|809|])
table.Add(5373953, [|809|])
table.Add(5373954, [|809|])
table.Add(5373956, [|809|])
table.Add(5439488, [|810|])
table.Add(5439489, [|810|])
table.Add(5439490, [|810|])
table.Add(5439492, [|810|])
table.Add(5505024, [|811|])
table.Add(5505025, [|811|])
table.Add(5505026, [|811|])
table.Add(5505028, [|811|])
table.Add(5570560, [|812|])
table.Add(5570561, [|812|])
table.Add(5570562, [|812|])
table.Add(5570564, [|812|])
table.Add(5636096, [|813|])
table.Add(5636097, [|813|])
table.Add(5636098, [|813|])
table.Add(5636100, [|813|])
table.Add(5701632, [|814|])
table.Add(5701633, [|814|])
table.Add(5701634, [|814|])
table.Add(5701636, [|814|])
table.Add(5767168, [|815|])
table.Add(5767169, [|815|])
table.Add(5767170, [|815|])
table.Add(5767172, [|815|])
table.Add(5832704, [|816|])
table.Add(5832705, [|816|])
table.Add(5832706, [|816|])
table.Add(5832708, [|816|])
table.Add(5898240, [|817|])
table.Add(5898241, [|817|])
table.Add(5898242, [|817|])
table.Add(5898244, [|817|])
table.Add(5963776, [|818|])
table.Add(5963777, [|818|])
table.Add(5963778, [|818|])
table.Add(5963780, [|818|])
table.Add(6029312, [|819|])
table.Add(6029313, [|819|])
table.Add(6029314, [|819|])
table.Add(6029316, [|819|])
table.Add(6094848, [|820|])
table.Add(6094849, [|820|])
table.Add(6094850, [|820|])
table.Add(6094852, [|820|])
table.Add(6160388, [|821|])
table.Add(6225920, [|822|])
table.Add(6225921, [|822|])
table.Add(6225922, [|822|])
table.Add(6225924, [|822|])
table.Add(6291456, [|823|])
table.Add(6291457, [|823|])
table.Add(6291458, [|823|])
table.Add(6291460, [|823|])
table.Add(6356992, [|824|])
table.Add(6356993, [|824|])
table.Add(6356994, [|824|])
table.Add(6356996, [|824|])
table.Add(6422528, [|825|])
table.Add(6422529, [|825|])
table.Add(6422530, [|825|])
table.Add(6422532, [|825|])
table.Add(6488064, [|826|])
table.Add(6488065, [|826|])
table.Add(6488066, [|826|])
table.Add(6488068, [|826|])
table.Add(6553600, [|827|])
table.Add(6553601, [|827|])
table.Add(6553602, [|827|])
table.Add(6553604, [|827|])
table.Add(6619136, [|828|])
table.Add(6619137, [|828|])
table.Add(6619138, [|828|])
table.Add(6619140, [|828|])
table.Add(6684672, [|829|])
table.Add(6684673, [|829|])
table.Add(6684674, [|829|])
table.Add(6684676, [|829|])
table.Add(6750208, [|830|])
table.Add(6750209, [|830|])
table.Add(6750210, [|830|])
table.Add(6750212, [|830|])
table.Add(6815744, [|831|])
table.Add(6815745, [|831|])
table.Add(6815746, [|831|])
table.Add(6815748, [|831|])
table.Add(6881280, [|832|])
table.Add(6881281, [|832|])
table.Add(6881282, [|832|])
table.Add(6881284, [|832|])
table.Add(6946816, [|833|])
table.Add(6946817, [|833|])
table.Add(6946818, [|833|])
table.Add(6946820, [|833|])
table.Add(7012352, [|834|])
table.Add(7012353, [|834|])
table.Add(7012354, [|834|])
table.Add(7012356, [|834|])
table.Add(7077888, [|835|])
table.Add(7077889, [|835|])
table.Add(7077890, [|835|])
table.Add(7077892, [|835|])
table.Add(7143424, [|836|])
table.Add(7143425, [|836|])
table.Add(7143426, [|836|])
table.Add(7143428, [|836|])
table.Add(7208960, [|837;838;839;840|])
table.Add(7208961, [|837;838;839;840|])
table.Add(7208962, [|837;838;839;840|])
table.Add(7208964, [|837;838;839;840|])
table.Add(7274496, [|841;842;843;844;845;846|])
table.Add(7274497, [|841;842;843;844;845;846|])
table.Add(7274498, [|841;842;843;844;845;846|])
table.Add(7274500, [|841;842;843;844;845;846|])
table.Add(7340032, [|847;848;849;850;851|])
table.Add(7340033, [|847;848;849;850;851|])
table.Add(7340034, [|847;848;849;850;851|])
table.Add(7340036, [|847;848;849;850;851|])
table.Add(7405568, [|852;853;854;855;856|])
table.Add(7405569, [|852;853;854;855;856|])
table.Add(7405570, [|852;853;854;855;856|])
table.Add(7405572, [|852;853;854;855;856|])
table.Add(7471104, [|857;858;859;860|])
table.Add(7471105, [|857;858;859;860|])
table.Add(7471106, [|857;858;859;860|])
table.Add(7471108, [|857;858;859;860|])

let private rules = [|257; 261; 259; 258; 13; 115; 14; 260; 0; 40; 147; 257; 147; 261; 261; 147; 257; 258; 147; 259; 259; 147; 258; 259; 147; 261; 261; 147; 259; 259; 147; 257; 257; 147; 259; 48; 152; 257; 152; 261; 261; 152; 257; 258; 152; 259; 259; 152; 258; 259; 152; 261; 261; 152; 259; 259; 152; 257; 257; 152; 259; 49; 153; 257; 153; 261; 261; 153; 257; 258; 153; 259; 259; 153; 258; 259; 153; 261; 261; 153; 259; 259; 153; 257; 257; 153; 259; 50; 129; 257; 155; 261; 261; 155; 257; 258; 155; 259; 259; 155; 258; 259; 155; 261; 261; 155; 259; 259; 155; 257; 257; 155; 259; 259; 155; 259; 51; 257; 155; 261; 261; 155; 257; 258; 155; 259; 259; 155; 258; 259; 155; 261; 261; 155; 259; 259; 155; 257; 257; 155; 259; 52; 257; 154; 261; 261; 154; 257; 258; 154; 259; 259; 154; 258; 259; 154; 261; 261; 154; 259; 259; 154; 257; 257; 154; 259; 53; 156; 257; 156; 261; 261; 156; 257; 258; 156; 259; 259; 156; 258; 259; 156; 261; 261; 156; 259; 259; 156; 257; 257; 156; 259; 54; 157; 257; 157; 261; 261; 157; 257; 258; 157; 259; 259; 157; 258; 259; 157; 261; 261; 157; 259; 259; 157; 257; 257; 157; 259; 55; 158; 257; 158; 261; 261; 158; 257; 258; 158; 259; 259; 158; 258; 259; 158; 261; 261; 158; 259; 259; 158; 257; 257; 158; 259; 56; 57; 58; 0; 1; 59; 60; 0; 4; 0; 0; 6; 0; 6; 0; 9; 61; 159; 257; 159; 261; 261; 159; 257; 258; 159; 259; 259; 159; 258; 259; 159; 261; 261; 159; 259; 259; 159; 257; 257; 159; 259; 62; 0; 0; 160; 257; 160; 261; 261; 160; 257; 258; 160; 259; 259; 160; 258; 259; 160; 261; 261; 160; 259; 259; 160; 257; 257; 160; 259; 63; 161; 257; 161; 261; 261; 161; 257; 258; 161; 259; 259; 161; 258; 259; 161; 261; 261; 161; 259; 259; 161; 257; 257; 161; 259; 64; 207; 164; 257; 164; 261; 261; 164; 257; 258; 164; 259; 259; 164; 258; 259; 164; 261; 261; 164; 259; 259; 164; 257; 257; 164; 259; 65; 0; 163; 257; 163; 261; 261; 163; 257; 258; 163; 259; 259; 163; 258; 259; 163; 261; 261; 163; 259; 259; 163; 257; 257; 163; 259; 66; 0; 257; 162; 261; 261; 162; 257; 258; 162; 259; 259; 162; 258; 259; 162; 261; 261; 162; 259; 259; 162; 257; 257; 162; 259; 67; 0; 0; 0; 0; 0; 166; 257; 166; 261; 261; 166; 257; 258; 166; 259; 259; 166; 258; 259; 166; 261; 261; 166; 259; 259; 166; 257; 257; 166; 259; 68; 257; 165; 261; 261; 165; 257; 258; 165; 259; 259; 165; 258; 259; 165; 261; 261; 165; 259; 259; 165; 257; 257; 165; 259; 69; 169; 257; 169; 261; 261; 169; 257; 258; 169; 259; 259; 169; 258; 259; 169; 261; 261; 169; 259; 259; 169; 257; 257; 169; 259; 70; 257; 168; 261; 261; 168; 257; 258; 168; 259; 259; 168; 258; 259; 168; 261; 261; 168; 259; 259; 168; 257; 257; 168; 259; 7; 257; 167; 261; 261; 167; 257; 258; 167; 259; 259; 167; 258; 259; 167; 261; 261; 167; 259; 259; 167; 257; 257; 167; 259; 8; 208; 173; 257; 173; 261; 261; 173; 257; 258; 173; 259; 259; 173; 258; 259; 173; 261; 261; 173; 259; 259; 173; 257; 257; 173; 259; 71; 0; 172; 257; 172; 261; 261; 172; 257; 258; 172; 259; 259; 172; 258; 259; 172; 261; 261; 172; 259; 259; 172; 257; 257; 172; 259; 72; 0; 257; 171; 261; 261; 171; 257; 258; 171; 259; 259; 171; 258; 259; 171; 261; 261; 171; 259; 259; 171; 257; 257; 171; 259; 73; 0; 0; 257; 170; 261; 261; 170; 257; 258; 170; 259; 259; 170; 258; 259; 170; 261; 261; 170; 259; 259; 170; 257; 257; 170; 259; 74; 174; 257; 174; 261; 261; 174; 257; 258; 174; 259; 259; 174; 258; 259; 174; 261; 261; 174; 259; 259; 174; 257; 257; 174; 259; 75; 175; 257; 175; 261; 261; 175; 257; 258; 175; 259; 259; 175; 258; 259; 175; 261; 261; 175; 259; 259; 175; 257; 257; 175; 259; 76; 0; 0; 0; 0; 176; 257; 176; 261; 261; 176; 257; 258; 176; 259; 259; 176; 258; 259; 176; 261; 261; 176; 259; 259; 176; 257; 257; 176; 259; 77; 168; 209; 178; 257; 178; 261; 261; 178; 257; 258; 178; 259; 259; 178; 258; 259; 178; 261; 261; 178; 259; 259; 178; 257; 257; 178; 259; 78; 0; 177; 257; 177; 261; 261; 177; 257; 258; 177; 259; 259; 177; 258; 259; 177; 261; 261; 177; 259; 259; 177; 257; 257; 177; 259; 79; 0; 0; 0; 211; 186; 257; 186; 261; 261; 186; 257; 258; 186; 259; 259; 186; 258; 259; 186; 261; 261; 186; 259; 259; 186; 257; 257; 186; 259; 80; 0; 185; 257; 185; 261; 261; 185; 257; 258; 185; 259; 259; 185; 258; 259; 185; 261; 261; 185; 259; 259; 185; 257; 257; 185; 259; 81; 0; 257; 184; 261; 261; 184; 257; 258; 184; 259; 259; 184; 258; 259; 184; 261; 261; 184; 259; 259; 184; 257; 257; 184; 259; 202; 0; 0; 183; 257; 183; 261; 261; 183; 257; 258; 183; 259; 259; 183; 258; 259; 183; 261; 261; 183; 259; 259; 183; 257; 257; 183; 259; 82; 0; 257; 182; 261; 261; 182; 257; 258; 182; 259; 259; 182; 258; 259; 182; 261; 261; 182; 259; 259; 182; 257; 257; 182; 259; 210; 0; 181; 257; 181; 261; 261; 181; 257; 258; 181; 259; 259; 181; 258; 259; 181; 261; 261; 181; 259; 259; 181; 257; 257; 181; 259; 83; 0; 180; 257; 180; 261; 261; 180; 257; 258; 180; 259; 259; 180; 258; 259; 180; 261; 261; 180; 259; 259; 180; 257; 257; 180; 259; 84; 0; 257; 179; 261; 261; 179; 257; 258; 179; 259; 259; 179; 258; 259; 179; 261; 261; 179; 259; 259; 179; 257; 257; 179; 259; 200; 0; 0; 212; 190; 257; 190; 261; 261; 190; 257; 258; 190; 259; 259; 190; 258; 259; 190; 261; 261; 190; 259; 259; 190; 257; 257; 190; 259; 85; 0; 189; 257; 189; 261; 261; 189; 257; 258; 189; 259; 259; 189; 258; 259; 189; 261; 261; 189; 259; 259; 189; 257; 257; 189; 259; 86; 0; 257; 188; 261; 261; 188; 257; 258; 188; 259; 259; 188; 258; 259; 188; 261; 261; 188; 259; 259; 188; 257; 257; 188; 259; 87; 0; 0; 257; 187; 261; 261; 187; 257; 258; 187; 259; 259; 187; 258; 259; 187; 261; 261; 187; 259; 259; 187; 257; 257; 187; 259; 88; 193; 132; 257; 132; 261; 261; 132; 257; 258; 132; 259; 259; 132; 258; 259; 132; 261; 261; 132; 259; 259; 132; 257; 257; 132; 259; 89; 0; 257; 131; 261; 261; 131; 257; 258; 131; 259; 259; 131; 258; 259; 131; 261; 261; 131; 259; 259; 131; 257; 257; 131; 259; 90; 0; 130; 257; 130; 261; 261; 130; 257; 258; 130; 259; 259; 130; 258; 259; 130; 261; 261; 130; 259; 259; 130; 257; 257; 130; 259; 91; 0; 192; 257; 192; 261; 261; 192; 257; 258; 192; 259; 259; 192; 258; 259; 192; 261; 261; 192; 259; 259; 192; 257; 257; 192; 259; 92; 0; 257; 191; 261; 261; 191; 257; 258; 191; 259; 259; 191; 258; 259; 191; 261; 261; 191; 259; 259; 191; 257; 257; 191; 259; 10; 0; 0; 0; 133; 257; 133; 261; 261; 133; 257; 258; 133; 259; 259; 133; 258; 259; 133; 261; 261; 133; 259; 259; 133; 257; 257; 133; 259; 93; 0; 134; 257; 134; 261; 261; 134; 257; 258; 134; 259; 259; 134; 258; 259; 134; 261; 261; 134; 259; 259; 134; 257; 257; 134; 259; 11; 135; 257; 135; 261; 261; 135; 257; 258; 135; 259; 259; 135; 258; 259; 135; 261; 261; 135; 259; 259; 135; 257; 257; 135; 259; 94; 9; 137; 6; 257; 137; 261; 261; 137; 257; 258; 137; 259; 259; 137; 258; 259; 137; 261; 261; 137; 259; 259; 137; 257; 257; 137; 259; 95; 257; 136; 261; 261; 136; 257; 258; 136; 259; 259; 136; 258; 259; 136; 261; 261; 136; 259; 259; 136; 257; 257; 136; 259; 96; 0; 0; 194; 139; 257; 139; 261; 261; 139; 257; 258; 139; 259; 259; 139; 258; 259; 139; 261; 261; 139; 259; 259; 139; 257; 257; 139; 259; 97; 0; 257; 138; 261; 261; 138; 257; 258; 138; 259; 259; 138; 258; 259; 138; 261; 261; 138; 259; 259; 138; 257; 257; 138; 259; 98; 0; 0; 140; 257; 140; 261; 261; 140; 257; 258; 140; 259; 259; 140; 258; 259; 140; 261; 261; 140; 259; 259; 140; 257; 257; 140; 259; 99; 206; 143; 257; 143; 261; 261; 143; 257; 258; 143; 259; 259; 143; 258; 259; 143; 261; 261; 143; 259; 259; 143; 257; 257; 143; 259; 100; 0; 142; 257; 142; 261; 261; 142; 257; 258; 142; 259; 259; 142; 258; 259; 142; 261; 261; 142; 259; 259; 142; 257; 257; 142; 259; 101; 0; 257; 141; 261; 261; 141; 257; 258; 141; 259; 259; 141; 258; 259; 141; 261; 261; 141; 259; 259; 141; 257; 257; 141; 259; 102; 0; 0; 146; 257; 146; 261; 261; 146; 257; 258; 146; 259; 259; 146; 258; 259; 146; 261; 261; 146; 259; 259; 146; 257; 257; 146; 259; 103; 257; 145; 261; 261; 145; 257; 258; 145; 259; 259; 145; 258; 259; 145; 261; 261; 145; 259; 259; 145; 257; 257; 145; 259; 104; 257; 144; 261; 261; 144; 257; 258; 144; 259; 259; 144; 258; 259; 144; 261; 261; 144; 259; 259; 144; 257; 257; 144; 259; 105; 196; 149; 257; 149; 261; 261; 149; 257; 258; 149; 259; 259; 149; 258; 259; 149; 261; 261; 149; 259; 259; 149; 257; 257; 149; 259; 106; 0; 257; 148; 261; 261; 148; 257; 258; 148; 259; 259; 148; 258; 259; 148; 261; 261; 148; 259; 259; 148; 257; 257; 148; 259; 107; 0; 0; 197; 151; 257; 151; 261; 261; 151; 257; 258; 151; 259; 259; 151; 258; 259; 151; 261; 261; 151; 259; 259; 151; 257; 257; 151; 259; 108; 0; 257; 150; 261; 261; 150; 257; 258; 150; 259; 259; 150; 258; 259; 150; 261; 261; 150; 259; 259; 150; 257; 257; 150; 259; 109; 0; 0; 0; 24; 1; 31; 2; 41; 2; 110; 42; 111; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 154; 2; 154; 259; 257; 257; 259; 43; 0; 2; 44; 2; 27; 112; 28; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 125; 45; 126; 3; 46; 5; 113; 47; 4; 241; 198; 250; 219; 162; 232; 127; 258; 261; 128; 257; 257; 116; 5; 165; 5; 1; 25; 4; 26; 4; 1; 167; 6; 168; 6; 242; 199; 251; 220; 171; 233; 3; 170; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 9; 168; 6; 9; 39; 4; 29; 114; 30; 1; 6; 168; 6; 243; 200; 252; 221; 168; 234; 245; 203; 254; 224; 184; 237; 223; 182; 236; 244; 201; 253; 222; 179; 235; 246; 204; 255; 225; 188; 238; 4; 187; 2; 1; 37; 8; 38; 6; 15; 1; 214; 131; 227; 117; 213; 6; 247; 205; 248; 226; 191; 239; 118; 168; 4; 261; 257; 258; 259; 2; 35; 9; 136; 1; 18; 1; 19; 4; 9; 36; 119; 16; 2; 17; 120; 215; 138; 228; 4; 168; 121; 9; 168; 8; 240; 195; 249; 216; 141; 229; 4; 191; 6; 9; 145; 8; 144; 3; 257; 257; 1; 0; 259; 258; 0; 217; 148; 230; 34; 20; 122; 218; 150; 231; 123; 33; 2; 21; 124; 22; 4; 23; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
let private canInferEpsilon = [|false; false; false; false; false; false; false; false; false; false; false; false; true; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; true; true; true; true; true; true; true; true; true; true; true; true; true; true; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; true; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree) -> tree.AstToDot numToString)

let private rulesStart = [|0; 1; 2; 3; 4; 6; 8; 8; 9; 10; 11; 14; 17; 20; 23; 26; 29; 32; 35; 36; 37; 40; 43; 46; 49; 52; 55; 58; 61; 62; 63; 66; 69; 72; 75; 78; 81; 84; 87; 88; 89; 92; 95; 98; 101; 104; 107; 110; 113; 116; 117; 120; 123; 126; 129; 132; 135; 138; 141; 142; 145; 148; 151; 154; 157; 160; 163; 166; 167; 168; 171; 174; 177; 180; 183; 186; 189; 192; 193; 194; 197; 200; 203; 206; 209; 212; 215; 218; 219; 220; 223; 226; 229; 232; 235; 238; 241; 244; 245; 246; 247; 249; 250; 251; 253; 256; 258; 260; 261; 262; 265; 268; 271; 274; 277; 280; 283; 286; 287; 287; 288; 288; 289; 290; 293; 296; 299; 302; 305; 308; 311; 314; 315; 316; 319; 322; 325; 328; 331; 334; 337; 340; 341; 342; 343; 346; 349; 352; 355; 358; 361; 364; 367; 368; 368; 369; 370; 373; 376; 379; 382; 385; 388; 391; 394; 395; 395; 396; 399; 402; 405; 408; 411; 414; 417; 420; 421; 421; 422; 422; 423; 423; 424; 424; 425; 425; 426; 427; 430; 433; 436; 439; 442; 445; 448; 451; 452; 455; 458; 461; 464; 467; 470; 473; 476; 477; 478; 481; 484; 487; 490; 493; 496; 499; 502; 503; 506; 509; 512; 515; 518; 521; 524; 527; 528; 531; 534; 537; 540; 543; 546; 549; 552; 553; 554; 555; 558; 561; 564; 567; 570; 573; 576; 579; 580; 580; 581; 582; 585; 588; 591; 594; 597; 600; 603; 606; 607; 607; 608; 611; 614; 617; 620; 623; 626; 629; 632; 633; 633; 634; 634; 635; 638; 641; 644; 647; 650; 653; 656; 659; 660; 661; 664; 667; 670; 673; 676; 679; 682; 685; 686; 687; 690; 693; 696; 699; 702; 705; 708; 711; 712; 716; 717; 720; 723; 726; 729; 732; 735; 738; 741; 742; 743; 744; 745; 748; 751; 754; 757; 760; 763; 766; 769; 770; 770; 771; 772; 775; 778; 781; 784; 787; 790; 793; 796; 797; 797; 798; 798; 799; 799; 800; 801; 802; 805; 808; 811; 814; 817; 820; 823; 826; 827; 827; 828; 829; 832; 835; 838; 841; 844; 847; 850; 853; 854; 854; 855; 858; 861; 864; 867; 870; 873; 876; 879; 880; 880; 881; 881; 882; 883; 886; 889; 892; 895; 898; 901; 904; 907; 908; 908; 909; 912; 915; 918; 921; 924; 927; 930; 933; 934; 934; 935; 936; 939; 942; 945; 948; 951; 954; 957; 960; 961; 961; 962; 963; 966; 969; 972; 975; 978; 981; 984; 987; 988; 988; 989; 992; 995; 998; 1001; 1004; 1007; 1010; 1013; 1014; 1014; 1015; 1015; 1016; 1017; 1018; 1021; 1024; 1027; 1030; 1033; 1036; 1039; 1042; 1043; 1043; 1044; 1045; 1048; 1051; 1054; 1057; 1060; 1063; 1066; 1069; 1070; 1070; 1071; 1074; 1077; 1080; 1083; 1086; 1089; 1092; 1095; 1096; 1096; 1097; 1097; 1098; 1101; 1104; 1107; 1110; 1113; 1116; 1119; 1122; 1123; 1124; 1125; 1128; 1131; 1134; 1137; 1140; 1143; 1146; 1149; 1150; 1150; 1151; 1154; 1157; 1160; 1163; 1166; 1169; 1172; 1175; 1176; 1176; 1177; 1178; 1181; 1184; 1187; 1190; 1193; 1196; 1199; 1202; 1203; 1203; 1204; 1205; 1208; 1211; 1214; 1217; 1220; 1223; 1226; 1229; 1230; 1230; 1231; 1234; 1237; 1240; 1243; 1246; 1249; 1252; 1255; 1256; 1256; 1257; 1257; 1258; 1258; 1259; 1260; 1263; 1266; 1269; 1272; 1275; 1278; 1281; 1284; 1285; 1285; 1286; 1287; 1290; 1293; 1296; 1299; 1302; 1305; 1308; 1311; 1312; 1313; 1316; 1319; 1322; 1325; 1328; 1331; 1334; 1337; 1338; 1341; 1344; 1347; 1350; 1353; 1356; 1359; 1362; 1365; 1366; 1369; 1372; 1375; 1378; 1381; 1384; 1387; 1390; 1391; 1391; 1392; 1392; 1393; 1394; 1395; 1398; 1401; 1404; 1407; 1410; 1413; 1416; 1419; 1420; 1420; 1421; 1424; 1427; 1430; 1433; 1436; 1439; 1442; 1445; 1446; 1446; 1447; 1447; 1448; 1449; 1452; 1455; 1458; 1461; 1464; 1467; 1470; 1473; 1474; 1475; 1476; 1479; 1482; 1485; 1488; 1491; 1494; 1497; 1500; 1501; 1501; 1502; 1503; 1506; 1509; 1512; 1515; 1518; 1521; 1524; 1527; 1528; 1528; 1529; 1532; 1535; 1538; 1541; 1544; 1547; 1550; 1553; 1554; 1554; 1555; 1555; 1556; 1557; 1560; 1563; 1566; 1569; 1572; 1575; 1578; 1581; 1582; 1585; 1588; 1591; 1594; 1597; 1600; 1603; 1606; 1607; 1610; 1613; 1616; 1619; 1622; 1625; 1628; 1631; 1632; 1633; 1634; 1637; 1640; 1643; 1646; 1649; 1652; 1655; 1658; 1659; 1659; 1660; 1663; 1666; 1669; 1672; 1675; 1678; 1681; 1684; 1685; 1685; 1686; 1686; 1687; 1688; 1689; 1692; 1695; 1698; 1701; 1704; 1707; 1710; 1713; 1714; 1714; 1715; 1718; 1721; 1724; 1727; 1730; 1733; 1736; 1739; 1740; 1740; 1741; 1741; 1742; 1742; 1743; 1746; 1749; 1752; 1756; 1761; 1767; 1774; 1782; 1784; 1786; 1790; 1792; 1795; 1798; 1799; 1801; 1802; 1804; 1807; 1809; 1812; 1816; 1819; 1823; 1832; 1842; 1853; 1856; 1859; 1862; 1865; 1868; 1875; 1878; 1883; 1888; 1891; 1894; 1897; 1905; 1914; 1924; 1927; 1934; 1937; 1940; 1943; 1946; 1949; 1952; 1955; 1958; 1961; 1964; 1967; 1974; 1977; 1980; 1983; 1986; 1989; 1993; 2002; 2009; 2012; 2015; 2018; 2021; 2024; 2027; 2029; 2035; 2039; 2042; 2045; 2048; 2056; 2057; 2059; 2062; 2066; 2068; 2071; 2075; 2080; 2086; 2093; 2096; 2100; 2105; 2111; 2118; 2120; 2123; 2127; 2132; 2138; 2147; 2157; 2168; 2180|]
let private probabilities = [|1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0|]
let startRule = 5
let indexatorFullCount = 262
let rulesCount = 861
let indexEOF = 260
let nonTermCount = 257
let termCount = 5
let termStart = 257
let termEnd = 261
let literalStart = 262
let literalEnd = 261
let literalsCount = 0

let slots = dict <| [|(-1, 0); (262145, 1); (262146, 2); (327681, 3); (458753, 4); (524289, 5); (589825, 6); (655362, 7); (720898, 8); (786434, 9); (851970, 10); (917506, 11); (983042, 12); (1048578, 13); (1114114, 14); (1179649, 15); (1245185, 16); (1310722, 17); (1376258, 18); (1441794, 19); (1507330, 20); (1572866, 21); (1638402, 22); (1703938, 23); (1769474, 24); (1835009, 25); (1900545, 26); (1966082, 27); (2031618, 28); (2097154, 29); (2162690, 30); (2228226, 31); (2293762, 32); (2359298, 33); (2424834, 34); (2490369, 35); (2555905, 36); (2621442, 37); (2686978, 38); (2752514, 39); (2818050, 40); (2883586, 41); (2949122, 42); (3014658, 43); (3080194, 44); (3145730, 45); (3211265, 46); (3276802, 47); (3342338, 48); (3407874, 49); (3473410, 50); (3538946, 51); (3604482, 52); (3670018, 53); (3735554, 54); (3801089, 55); (3866626, 56); (3932162, 57); (3997698, 58); (4063234, 59); (4128770, 60); (4194306, 61); (4259842, 62); (4325378, 63); (4390913, 64); (4456449, 65); (4521986, 66); (4587522, 67); (4653058, 68); (4718594, 69); (4784130, 70); (4849666, 71); (4915202, 72); (4980738, 73); (5046273, 74); (5111809, 75); (5177346, 76); (5242882, 77); (5308418, 78); (5373954, 79); (5439490, 80); (5505026, 81); (5570562, 82); (5636098, 83); (5701633, 84); (5767169, 85); (5832706, 86); (5898242, 87); (5963778, 88); (6029314, 89); (6094850, 90); (6160386, 91); (6225922, 92); (6291458, 93); (6356993, 94); (6422529, 95); (6488065, 96); (6553601, 97); (6553602, 98); (6619137, 99); (6684673, 100); (6750209, 101); (6750210, 102); (6815745, 103); (6815746, 104); (6815747, 105); (6881281, 106); (6881282, 107); (6946817, 108); (6946818, 109); (7012353, 110); (7077889, 111); (7143426, 112); (7208962, 113); (7274498, 114); (7340034, 115); (7405570, 116); (7471106, 117); (7536642, 118); (7602178, 119); (7667713, 120); (7798785, 121); (7929857, 122); (7995393, 123); (8060930, 124); (8126466, 125); (8192002, 126); (8257538, 127); (8323074, 128); (8388610, 129); (8454146, 130); (8519682, 131); (8585217, 132); (8650753, 133); (8716290, 134); (8781826, 135); (8847362, 136); (8912898, 137); (8978434, 138); (9043970, 139); (9109506, 140); (9175042, 141); (9240577, 142); (9306113, 143); (9371649, 144); (9437186, 145); (9502722, 146); (9568258, 147); (9633794, 148); (9699330, 149); (9764866, 150); (9830402, 151); (9895938, 152); (9961473, 153); (10092545, 154); (10158081, 155); (10223618, 156); (10289154, 157); (10354690, 158); (10420226, 159); (10485762, 160); (10551298, 161); (10616834, 162); (10682370, 163); (10747905, 164); (10878977, 165); (10944514, 166); (11010050, 167); (11075586, 168); (11141122, 169); (11206658, 170); (11272194, 171); (11337730, 172); (11403266, 173); (11468801, 174); (11599873, 175); (11730945, 176); (11862017, 177); (11993089, 178); (12124161, 179); (12189697, 180); (12255234, 181); (12320770, 182); (12386306, 183); (12451842, 184); (12517378, 185); (12582914, 186); (12648450, 187); (12713986, 188); (12779521, 189); (12845058, 190); (12910594, 191); (12976130, 192); (13041666, 193); (13107202, 194); (13172738, 195); (13238274, 196); (13303810, 197); (13369345, 198); (13434881, 199); (13500418, 200); (13565954, 201); (13631490, 202); (13697026, 203); (13762562, 204); (13828098, 205); (13893634, 206); (13959170, 207); (14024705, 208); (14090242, 209); (14155778, 210); (14221314, 211); (14286850, 212); (14352386, 213); (14417922, 214); (14483458, 215); (14548994, 216); (14614529, 217); (14680066, 218); (14745602, 219); (14811138, 220); (14876674, 221); (14942210, 222); (15007746, 223); (15073282, 224); (15138818, 225); (15204353, 226); (15269889, 227); (15335425, 228); (15400962, 229); (15466498, 230); (15532034, 231); (15597570, 232); (15663106, 233); (15728642, 234); (15794178, 235); (15859714, 236); (15925249, 237); (16056321, 238); (16121857, 239); (16187394, 240); (16252930, 241); (16318466, 242); (16384002, 243); (16449538, 244); (16515074, 245); (16580610, 246); (16646146, 247); (16711681, 248); (16842753, 249); (16908290, 250); (16973826, 251); (17039362, 252); (17104898, 253); (17170434, 254); (17235970, 255); (17301506, 256); (17367042, 257); (17432577, 258); (17563649, 259); (17694721, 260); (17760258, 261); (17825794, 262); (17891330, 263); (17956866, 264); (18022402, 265); (18087938, 266); (18153474, 267); (18219010, 268); (18284545, 269); (18350081, 270); (18415618, 271); (18481154, 272); (18546690, 273); (18612226, 274); (18677762, 275); (18743298, 276); (18808834, 277); (18874370, 278); (18939905, 279); (19005441, 280); (19070978, 281); (19136514, 282); (19202050, 283); (19267586, 284); (19333122, 285); (19398658, 286); (19464194, 287); (19529730, 288); (19595265, 289); (19660801, 290); (19660802, 291); (19660803, 292); (19660804, 293); (19726337, 294); (19791874, 295); (19857410, 296); (19922946, 297); (19988482, 298); (20054018, 299); (20119554, 300); (20185090, 301); (20250626, 302); (20316161, 303); (20381697, 304); (20447233, 305); (20512769, 306); (20578306, 307); (20643842, 308); (20709378, 309); (20774914, 310); (20840450, 311); (20905986, 312); (20971522, 313); (21037058, 314); (21102593, 315); (21233665, 316); (21299201, 317); (21364738, 318); (21430274, 319); (21495810, 320); (21561346, 321); (21626882, 322); (21692418, 323); (21757954, 324); (21823490, 325); (21889025, 326); (22020097, 327); (22151169, 328); (22282241, 329); (22347777, 330); (22413313, 331); (22478850, 332); (22544386, 333); (22609922, 334); (22675458, 335); (22740994, 336); (22806530, 337); (22872066, 338); (22937602, 339); (23003137, 340); (23134209, 341); (23199745, 342); (23265282, 343); (23330818, 344); (23396354, 345); (23461890, 346); (23527426, 347); (23592962, 348); (23658498, 349); (23724034, 350); (23789569, 351); (23920641, 352); (23986178, 353); (24051714, 354); (24117250, 355); (24182786, 356); (24248322, 357); (24313858, 358); (24379394, 359); (24444930, 360); (24510465, 361); (24641537, 362); (24772609, 363); (24838145, 364); (24903682, 365); (24969218, 366); (25034754, 367); (25100290, 368); (25165826, 369); (25231362, 370); (25296898, 371); (25362434, 372); (25427969, 373); (25559041, 374); (25624578, 375); (25690114, 376); (25755650, 377); (25821186, 378); (25886722, 379); (25952258, 380); (26017794, 381); (26083330, 382); (26148865, 383); (26279937, 384); (26345473, 385); (26411010, 386); (26476546, 387); (26542082, 388); (26607618, 389); (26673154, 390); (26738690, 391); (26804226, 392); (26869762, 393); (26935297, 394); (27066369, 395); (27131905, 396); (27197442, 397); (27262978, 398); (27328514, 399); (27394050, 400); (27459586, 401); (27525122, 402); (27590658, 403); (27656194, 404); (27721729, 405); (27852801, 406); (27918338, 407); (27983874, 408); (28049410, 409); (28114946, 410); (28180482, 411); (28246018, 412); (28311554, 413); (28377090, 414); (28442625, 415); (28573697, 416); (28704769, 417); (28770305, 418); (28835841, 419); (28901378, 420); (28966914, 421); (29032450, 422); (29097986, 423); (29163522, 424); (29229058, 425); (29294594, 426); (29360130, 427); (29425665, 428); (29556737, 429); (29622273, 430); (29687810, 431); (29753346, 432); (29818882, 433); (29884418, 434); (29949954, 435); (30015490, 436); (30081026, 437); (30146562, 438); (30212097, 439); (30343169, 440); (30408706, 441); (30474242, 442); (30539778, 443); (30605314, 444); (30670850, 445); (30736386, 446); (30801922, 447); (30867458, 448); (30932993, 449); (31064065, 450); (31195137, 451); (31260674, 452); (31326210, 453); (31391746, 454); (31457282, 455); (31522818, 456); (31588354, 457); (31653890, 458); (31719426, 459); (31784961, 460); (31850497, 461); (31916033, 462); (31981570, 463); (32047106, 464); (32112642, 465); (32178178, 466); (32243714, 467); (32309250, 468); (32374786, 469); (32440322, 470); (32505857, 471); (32636929, 472); (32702466, 473); (32768002, 474); (32833538, 475); (32899074, 476); (32964610, 477); (33030146, 478); (33095682, 479); (33161218, 480); (33226753, 481); (33357825, 482); (33423361, 483); (33488898, 484); (33554434, 485); (33619970, 486); (33685506, 487); (33751042, 488); (33816578, 489); (33882114, 490); (33947650, 491); (34013185, 492); (34144257, 493); (34209793, 494); (34275330, 495); (34340866, 496); (34406402, 497); (34471938, 498); (34537474, 499); (34603010, 500); (34668546, 501); (34734082, 502); (34799617, 503); (34930689, 504); (34996226, 505); (35061762, 506); (35127298, 507); (35192834, 508); (35258370, 509); (35323906, 510); (35389442, 511); (35454978, 512); (35520513, 513); (35651585, 514); (35782657, 515); (35913729, 516); (35979265, 517); (36044802, 518); (36110338, 519); (36175874, 520); (36241410, 521); (36306946, 522); (36372482, 523); (36438018, 524); (36503554, 525); (36569089, 526); (36700161, 527); (36765697, 528); (36831234, 529); (36896770, 530); (36962306, 531); (37027842, 532); (37093378, 533); (37158914, 534); (37224450, 535); (37289986, 536); (37355521, 537); (37421057, 538); (37486594, 539); (37552130, 540); (37617666, 541); (37683202, 542); (37748738, 543); (37814274, 544); (37879810, 545); (37945346, 546); (38010881, 547); (38076417, 548); (38076418, 549); (38076419, 550); (38141954, 551); (38207490, 552); (38273026, 553); (38338562, 554); (38404098, 555); (38469634, 556); (38535170, 557); (38600706, 558); (38666241, 559); (38731778, 560); (38797314, 561); (38862850, 562); (38928386, 563); (38993922, 564); (39059458, 565); (39124994, 566); (39190530, 567); (39256065, 568); (39387137, 569); (39518209, 570); (39583745, 571); (39649281, 572); (39714818, 573); (39780354, 574); (39845890, 575); (39911426, 576); (39976962, 577); (40042498, 578); (40108034, 579); (40173570, 580); (40239105, 581); (40370177, 582); (40435714, 583); (40501250, 584); (40566786, 585); (40632322, 586); (40697858, 587); (40763394, 588); (40828930, 589); (40894466, 590); (40960001, 591); (41091073, 592); (41222145, 593); (41287681, 594); (41353218, 595); (41418754, 596); (41484290, 597); (41549826, 598); (41615362, 599); (41680898, 600); (41746434, 601); (41811970, 602); (41877505, 603); (41943041, 604); (42008577, 605); (42074114, 606); (42139650, 607); (42205186, 608); (42270722, 609); (42336258, 610); (42401794, 611); (42467330, 612); (42532866, 613); (42598401, 614); (42729473, 615); (42795009, 616); (42860546, 617); (42926082, 618); (42991618, 619); (43057154, 620); (43122690, 621); (43188226, 622); (43253762, 623); (43319298, 624); (43384833, 625); (43515905, 626); (43581442, 627); (43646978, 628); (43712514, 629); (43778050, 630); (43843586, 631); (43909122, 632); (43974658, 633); (44040194, 634); (44105729, 635); (44236801, 636); (44367873, 637); (44433409, 638); (44498946, 639); (44564482, 640); (44630018, 641); (44695554, 642); (44761090, 643); (44826626, 644); (44892162, 645); (44957698, 646); (45023233, 647); (45088770, 648); (45154306, 649); (45219842, 650); (45285378, 651); (45350914, 652); (45416450, 653); (45481986, 654); (45547522, 655); (45613057, 656); (45678594, 657); (45744130, 658); (45809666, 659); (45875202, 660); (45940738, 661); (46006274, 662); (46071810, 663); (46137346, 664); (46202881, 665); (46268417, 666); (46333953, 667); (46399490, 668); (46465026, 669); (46530562, 670); (46596098, 671); (46661634, 672); (46727170, 673); (46792706, 674); (46858242, 675); (46923777, 676); (47054849, 677); (47120386, 678); (47185922, 679); (47251458, 680); (47316994, 681); (47382530, 682); (47448066, 683); (47513602, 684); (47579138, 685); (47644673, 686); (47775745, 687); (47906817, 688); (47972353, 689); (48037889, 690); (48103426, 691); (48168962, 692); (48234498, 693); (48300034, 694); (48365570, 695); (48431106, 696); (48496642, 697); (48562178, 698); (48627713, 699); (48758785, 700); (48824322, 701); (48889858, 702); (48955394, 703); (49020930, 704); (49086466, 705); (49152002, 706); (49217538, 707); (49283074, 708); (49348609, 709); (49479681, 710); (49610753, 711); (49741825, 712); (49807361, 713); (49807362, 714); (49807363, 715); (49872897, 716); (49872898, 717); (49872899, 718); (49938433, 719); (49938434, 720); (49938435, 721); (50003969, 722); (50003970, 723); (50003971, 724); (50003972, 725); (50069505, 726); (50069506, 727); (50069507, 728); (50069508, 729); (50069509, 730); (50135041, 731); (50135042, 732); (50135043, 733); (50135044, 734); (50135045, 735); (50135046, 736); (50200577, 737); (50200578, 738); (50200579, 739); (50200580, 740); (50200581, 741); (50200582, 742); (50200583, 743); (50266113, 744); (50266114, 745); (50266115, 746); (50266116, 747); (50266117, 748); (50266118, 749); (50266119, 750); (50266120, 751); (50331649, 752); (50331650, 753); (50397185, 754); (50397186, 755); (50528257, 756); (50528258, 757); (50593793, 758); (50593794, 759); (50593795, 760); (50659329, 761); (50659330, 762); (50659331, 763); (50724865, 764); (50790401, 765); (50790402, 766); (50855937, 767); (50921473, 768); (50921474, 769); (50987009, 770); (50987010, 771); (50987011, 772); (51052545, 773); (51052546, 774); (51118081, 775); (51118082, 776); (51118083, 777); (51183617, 778); (51183618, 779); (51183619, 780); (51183620, 781); (51249153, 782); (51249154, 783); (51249155, 784); (51314689, 785); (51314690, 786); (51314691, 787); (51314692, 788); (51380225, 789); (51380226, 790); (51380227, 791); (51380228, 792); (51380229, 793); (51380230, 794); (51380231, 795); (51380232, 796); (51380233, 797); (51445761, 798); (51445762, 799); (51445763, 800); (51445764, 801); (51445765, 802); (51445766, 803); (51445767, 804); (51445768, 805); (51445769, 806); (51445770, 807); (51511297, 808); (51511298, 809); (51511299, 810); (51511300, 811); (51511301, 812); (51511302, 813); (51511303, 814); (51511304, 815); (51511305, 816); (51511306, 817); (51511307, 818); (51576833, 819); (51576834, 820); (51576835, 821); (51642369, 822); (51642370, 823); (51642371, 824); (51707905, 825); (51707906, 826); (51707907, 827); (51773441, 828); (51773442, 829); (51773443, 830); (51838977, 831); (51838978, 832); (51838979, 833); (51904513, 834); (51904516, 835); (51904519, 836); (51970049, 837); (51970050, 838); (51970051, 839); (52035585, 840); (52035586, 841); (52035587, 842); (52035588, 843); (52035589, 844); (52101121, 845); (52101122, 846); (52101123, 847); (52101124, 848); (52101125, 849); (52166657, 850); (52166658, 851); (52166659, 852); (52232193, 853); (52232194, 854); (52232195, 855); (52297729, 856); (52297730, 857); (52297731, 858); (52363265, 859); (52363266, 860); (52363267, 861); (52363268, 862); (52363269, 863); (52363270, 864); (52363271, 865); (52363272, 866); (52428801, 867); (52428802, 868); (52428803, 869); (52428804, 870); (52428805, 871); (52428806, 872); (52428807, 873); (52428808, 874); (52428809, 875); (52494337, 876); (52494338, 877); (52494339, 878); (52494340, 879); (52494341, 880); (52494342, 881); (52494343, 882); (52494344, 883); (52494345, 884); (52494346, 885); (52559873, 886); (52559874, 887); (52559875, 888); (52625409, 889); (52625410, 890); (52625411, 891); (52625412, 892); (52625413, 893); (52625414, 894); (52625415, 895); (52690945, 896); (52690946, 897); (52690947, 898); (52756481, 899); (52756482, 900); (52756483, 901); (52822017, 902); (52822018, 903); (52822019, 904); (52887553, 905); (52887554, 906); (52887555, 907); (52953089, 908); (52953090, 909); (52953091, 910); (53018625, 911); (53018626, 912); (53018627, 913); (53084161, 914); (53084162, 915); (53084163, 916); (53149697, 917); (53149698, 918); (53149699, 919); (53215233, 920); (53215234, 921); (53215235, 922); (53280769, 923); (53280770, 924); (53280771, 925); (53346305, 926); (53346306, 927); (53346307, 928); (53411841, 929); (53411842, 930); (53411843, 931); (53411844, 932); (53411845, 933); (53411846, 934); (53411847, 935); (53477377, 936); (53477378, 937); (53477379, 938); (53542913, 939); (53542914, 940); (53542915, 941); (53608449, 942); (53608450, 943); (53608451, 944); (53673985, 945); (53673986, 946); (53673987, 947); (53739521, 948); (53739522, 949); (53739523, 950); (53870593, 951); (53870594, 952); (53870595, 953); (53870596, 954); (53870597, 955); (53870598, 956); (53870599, 957); (53870600, 958); (53870601, 959); (53936129, 960); (53936130, 961); (53936131, 962); (53936132, 963); (53936133, 964); (53936134, 965); (53936135, 966); (54001665, 967); (54001666, 968); (54001667, 969); (54067201, 970); (54067202, 971); (54067203, 972); (54132737, 973); (54132738, 974); (54132739, 975); (54198273, 976); (54198274, 977); (54198275, 978); (54263809, 979); (54263810, 980); (54263811, 981); (54329345, 982); (54329346, 983); (54329347, 984); (54394881, 985); (54394882, 986); (54460417, 987); (54460418, 988); (54460419, 989); (54460422, 990); (54525953, 991); (54525956, 992); (54591489, 993); (54591490, 994); (54591491, 995); (54657025, 996); (54657026, 997); (54657027, 998); (54722561, 999); (54722562, 1000); (54722563, 1001); (54788097, 1002); (54788098, 1003); (54788099, 1004); (54788100, 1005); (54788101, 1006); (54788102, 1007); (54788103, 1008); (54788104, 1009); (54853633, 1010); (54919169, 1011); (54919170, 1012); (54984705, 1013); (54984706, 1014); (54984707, 1015); (55050241, 1016); (55050242, 1017); (55050243, 1018); (55050244, 1019); (55115777, 1020); (55115778, 1021); (55181313, 1022); (55181314, 1023); (55181315, 1024); (55246849, 1025); (55246850, 1026); (55246851, 1027); (55246852, 1028); (55312385, 1029); (55312386, 1030); (55312387, 1031); (55312388, 1032); (55312389, 1033); (55377921, 1034); (55377922, 1035); (55377923, 1036); (55377924, 1037); (55377925, 1038); (55377926, 1039); (55443457, 1040); (55443458, 1041); (55443459, 1042); (55443460, 1043); (55443461, 1044); (55443462, 1045); (55443463, 1046); (55508993, 1047); (55508994, 1048); (55508995, 1049); (55574529, 1050); (55574530, 1051); (55574531, 1052); (55574532, 1053); (55640065, 1054); (55640066, 1055); (55640067, 1056); (55640068, 1057); (55640069, 1058); (55705601, 1059); (55705602, 1060); (55705603, 1061); (55705604, 1062); (55705605, 1063); (55705606, 1064); (55771137, 1065); (55771138, 1066); (55771139, 1067); (55771140, 1068); (55771141, 1069); (55771142, 1070); (55771143, 1071); (55836673, 1072); (55836674, 1073); (55902209, 1074); (55902210, 1075); (55902211, 1076); (55967745, 1077); (55967746, 1078); (55967747, 1079); (55967748, 1080); (56033281, 1081); (56033282, 1082); (56033283, 1083); (56033284, 1084); (56033285, 1085); (56098817, 1086); (56098818, 1087); (56098819, 1088); (56098820, 1089); (56098821, 1090); (56098822, 1091); (56164353, 1092); (56164354, 1093); (56164355, 1094); (56164356, 1095); (56164357, 1096); (56164358, 1097); (56164359, 1098); (56164360, 1099); (56164361, 1100); (56229889, 1101); (56229890, 1102); (56229891, 1103); (56229892, 1104); (56229893, 1105); (56229894, 1106); (56229895, 1107); (56229896, 1108); (56229897, 1109); (56229898, 1110); (56295425, 1111); (56295426, 1112); (56295427, 1113); (56295428, 1114); (56295429, 1115); (56295430, 1116); (56295431, 1117); (56295432, 1118); (56295433, 1119); (56295434, 1120); (56295435, 1121); (56360961, 1122); (56360962, 1123); (56360963, 1124); (56360964, 1125); (56360965, 1126); (56360966, 1127); (56360967, 1128); (56360968, 1129); (56360969, 1130); (56360970, 1131); (56360971, 1132); (56360972, 1133)|]

let private parserSource = new ParserSourceGLL<Token> (Token.RNGLR_EOF 0, tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots, probabilities)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> int -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTree.buildAbstract<Token> parserSource


