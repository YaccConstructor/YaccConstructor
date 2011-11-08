let tests = 
  [{
     FullGrammarPath =  "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_1.yrd.in";
       "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_2.yrd.in";
       "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_3.yrd.in";
       "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_4.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/action_code/checkers/checker_on_glr/lex_checker_on_glr.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_checker_on_glr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_checker_on_glr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_checker_on_glr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/action_code/checkers/simple_checker/simple_checker.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/action_code/checkers/simple_checker/simple_checker_1.yrd.in";
       "../../../Tests/GNESCC/action_code/checkers/simple_checker/simple_checker_2.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/action_code/checkers/simple_checker/lex_simple_checker.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_simple_checker")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_simple_checker")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_simple_checker") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/action_code/l_attrs/l_attr/l_attr.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/action_code/l_attrs/l_attr/l_attr_1.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/action_code/l_attrs/l_attr/lex_l_attr.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_l_attr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_l_attr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_l_attr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/customers/claret/braces/braces.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/customers/claret/braces/braces_1.yrd.in";
       "../../../Tests/GNESCC/customers/claret/braces/braces_2.yrd.in";
       "../../../Tests/GNESCC/customers/claret/braces/braces_3.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/customers/claret/braces/lex_braces.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_braces")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_braces")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_braces") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/customers/rl/RL.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/customers/rl/min_test_0.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_1.rl.in";
       "../../../Tests/GNESCC/customers/rl/min_test_10.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_11.rl.in";
       "../../../Tests/GNESCC/customers/rl/min_test_2.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_3.rl.in";
       "../../../Tests/GNESCC/customers/rl/min_test_4.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_5.rl.in";
       "../../../Tests/GNESCC/customers/rl/min_test_6.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_7.rl.in";
       "../../../Tests/GNESCC/customers/rl/min_test_8.rl.in"; "../../../Tests/GNESCC/customers/rl/min_test_9.rl.in";
       "../../../Tests/GNESCC/customers/rl/test0.rl.in"; "../../../Tests/GNESCC/customers/rl/test1.rl.in";
       "../../../Tests/GNESCC/customers/rl/test2.rl.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/customers/rl/lex_RL.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_RL")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_RL")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_RL") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_1.yrd.in";
       "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_2.yrd.in";
       "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_3.yrd.in";
       "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_4.yrd.in";
       "../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_5.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/glr/arithm_glr/lex_arithm_glr.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_arithm_glr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_arithm_glr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_arithm_glr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/glr/reduce_reduce/reduce_reduce.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/glr/reduce_reduce/reduce_reduce_1.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/glr/reduce_reduce/lex_reduce_reduce.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_reduce_reduce")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_reduce_reduce")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_reduce_reduce") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_1/test_rec_rule_1.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_rec_rule_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_rec_rule_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_rec_rule_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_2/test_rec_rule_2.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_rec_rule_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_rec_rule_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_rec_rule_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_3/test_rec_rule_3.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_rec_rule_3")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_rec_rule_3")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_rec_rule_3") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_regexp/test_regexp.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_regexp")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_regexp")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_regexp") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_1.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_2.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_3.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_4.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_5.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_6.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_7.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/complex/alt_in_cls/lex_alt_in_cls.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_alt_in_cls")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_alt_in_cls")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_alt_in_cls") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/cls_with_head/cls_with_head.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/regexp/complex/cls_with_head/cls_with_head_1.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/cls_with_head/cls_with_head_2.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/complex/cls_with_head/lex_cls_with_head.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls_with_head")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls_with_head")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls_with_head") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/cls_with_tail/cls_with_tail.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/regexp/complex/cls_with_tail/cls_with_tail_1.yrd.in";
       "../../../Tests/GNESCC/regexp/complex/cls_with_tail/cls_with_tail_2.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/complex/cls_with_tail/lex_cls_with_tail.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls_with_tail")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls_with_tail")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls_with_tail") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/seq_in_cls_1/seq_in_cls_1.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq_in_cls_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq_in_cls_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq_in_cls_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/seq_in_cls_2/seq_in_cls_2.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq_in_cls_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq_in_cls_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq_in_cls_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/complex/seq_nonterms/seq_nonterms.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/regexp/complex/seq_nonterms/seq_nonterms_1.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/complex/seq_nonterms/lex_seq_nonterms.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq_nonterms")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq_nonterms")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq_nonterms") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/simple/alt/alt.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/regexp/simple/alt/alt_1.yrd.in"; "../../../Tests/GNESCC/regexp/simple/alt/alt_2.yrd.in"
       ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/simple/alt/lex_alt.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_alt")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_alt")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_alt") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/simple/cls/cls.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/regexp/simple/cls/cls_1.yrd.in"; "../../../Tests/GNESCC/regexp/simple/cls/cls_2.yrd.in";
       "../../../Tests/GNESCC/regexp/simple/cls/cls_3.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/simple/cls/lex_cls.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/simple/opt/opt.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/regexp/simple/opt/opt_1.yrd.in"; "../../../Tests/GNESCC/regexp/simple/opt/opt_2.yrd.in"
       ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/simple/opt/lex_opt.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_opt")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_opt")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_opt") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/regexp/simple/seq/seq.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/regexp/simple/seq/seq_1.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/regexp/simple/seq/lex_seq.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/summator_1/summator_1.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/summator_1/summator_1_1.yrd.in"; "../../../Tests/GNESCC/summator_1/summator_1_2.yrd.in";
       "../../../Tests/GNESCC/summator_1/summator_1_3.yrd.in"; "../../../Tests/GNESCC/summator_1/summator_1_4.yrd.in" ]
     FullLexerPath =  "../../../Tests/GNESCC/summator_1/lex_summator_1.fsl"
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_summator_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_summator_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_summator_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_1/test_1.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_1/test_1_0.yrd.in"; "../../../Tests/GNESCC/test_1/test_1_1.yrd.in";
       "../../../Tests/GNESCC/test_1/test_1_2.yrd.in"; "../../../Tests/GNESCC/test_1/test_1_3.yrd.in" ]
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_2/test_2.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_3/test_3.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_3/test_3_1.yrd.in" ]
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_3")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_3")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_3") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_4/test_4.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_4/test_4_0.yrd.in"; "../../../Tests/GNESCC/test_4/test_4_1.yrd.in";
       "../../../Tests/GNESCC/test_4/test_4_2.yrd.in"; "../../../Tests/GNESCC/test_4/test_4_3.yrd.in" ]
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_4")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_4")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_4") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_5/test_5.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_5/test_5_1.yrd.in"; "../../../Tests/GNESCC/test_5/test_5_2.yrd.in" ]
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_5")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_5")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_5") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_6/test_6.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_6")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_6")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_6") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_seq/test_seq_1.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  ""
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_test_seq_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_test_seq_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_test_seq_1") };]
