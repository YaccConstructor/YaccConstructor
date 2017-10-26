(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
# YARD
YARD is powerful translation specification language. It allows you to use EBNF, metarules and conditional generation. Also YARD support L and S attributed grammars.

<h2>Why YARD?</h2>

<h3>Comparison</h3>
<table class="table" border="3" style="width:100%">
  <tr>
    <td></td>
    <td>YARD</td>
    <td>ANTLR</td>
    <td>YACC</td>
    <td>Bison</td>
  </tr>
  <tr>
    <td>Parsing algorithm</td>
    <td>RNGLR, GLL</td> 
    <td>LL(*)</td>
    <td>LALR(1)</td>
    <td>LALR(1), LR(1), IELR(1), GLR</td>
  </tr>
  <tr>
    <td>Lexer</td>
    <td>+</td> 
    <td>+</td>
    <td>-</td>
    <td>-</td>
  </tr>
  <tr>
    <td>License</td>
    <td>Apache 2.0</td> 
    <td>BSD</td>
    <td>CPL & CDDL</td>
    <td>GNU GPL</td>
  </tr>
  <tr>
    <td>Output languages</td>
    <td>F#</td> 
    <td>C#, Java, Python (ANTLR 4) <br> C, C++, C#, Java & 7 more (ANTLR 3)</td>
    <td>C + specific language implementations</td>
    <td>C, C++, Java, XML</td>
  </tr>
  <tr>
    <td>Input grammar notation</td>
    <td>EBNF</td> 
    <td>EBNF</td>
    <td>YACC (BNF)</td>
    <td>YACC (BNF)</td>
  </tr>
  <tr>
    <td>Literals</td>
    <td>+</td> 
    <td>+</td>
    <td>+</td>
    <td>+</td>
  </tr>
  <tr>
    <td>Predicates</td>
    <td>+</td> 
    <td>+</td>
    <td>-</td>
    <td>-</td>
  </tr>
  <tr>
    <td>L-attributes</td>
    <td>+</td> 
    <td>+</td>
    <td>-</td>
    <td>-</td>
  </tr>
  <tr>
    <td>Parametrized rules</td>
    <td>+</td> 
    <td>+(limited) <sup>[1]</sup></td>
    <td>-</td>
    <td>-</td>
  </tr>
  <tr>
    <td>Rules priority</td>
    <td>+</td> 
    <td>+</td>
    <td>+</td>
    <td>+</td>
  </tr>
  <tr>
    <td>Bindings to synthetized attributes</td>
    <td>+</td> 
    <td>+</td>
    <td>-</td>
    <td>-</td>
  </tr>
  <tr>
    <td>Grammar modularity support</td>
    <td>+</td> 
    <td>+</td>
    <td>-</td>
    <td>-</td>
  </tr>
</table>

[1] &mdash; in ANTLR rule are specified using the syntax of the generated language, meanwhile YARD can accept RegExps' or other rules too!

<h2>Lexing</h2>
Basically, you can use anything as a lexer and then transform it into suitable form. If you do not want things to get complicated, you can use modified version of FsLex that is bundled with YaccConstructor.

<h2>Grammar structure</h2>

<h3>Grammar definition</h3>
Definition in its general form looks like this:

<pre>
  info
  tokens
  options    
  head   
  grammar 
  foot
</pre>

<p>
Where<br>
  &emsp;<b>info</b>:    text that contains some information and would not be used anywhere.<br>
  &emsp;<b>tokens</b>:  tokens type specification<br>
  &emsp;<b>options</b>: command-line arguments can be written here instead of being passed in command line<br>
  &emsp;<b>head</b>:    F# code that will be copied to the beginning of a generated F# file. Usually used for some open-s.<br>
  &emsp;<b>grammar</b>: grammar description.<br>
  &emsp;<b>foot</b>:    F# code that will be copied to the end of a generated F# file.<br>
</p>

<h3>Grammar description</h3>
<p>Basically, grammar description is list of modules. A module in general form looks like this:</p>

<pre>
  [&lt;AllPublic&gt;]
  name
  openings
  rules
</pre>

<p>
Where<br>
  &emsp;<b>AllPublic</b>: this annotation makes all rules visible when using this module. By default, all rules are private except the ones explicitly marked public.<br>
  &emsp;<b>name</b>:      name of the module.<br>
  &emsp;<b>openings</b>:  usage (opening) of another grammars from .yrd files<br>
  &emsp;<b>rules</b>:     rules that are present in grammar<br>
</p>

<h3>Rules</h3>
<p>A rule in general form looks like this:</p>

<pre>
  [&lt;Start&gt;]
  public modifier
  name
  args
  body
</pre>

<p>
Where<br>
  &emsp;<b>Start</b>:  this annotation makes this rule a starting one.<br>
  &emsp;<b>public</b>: this modifier allows rule to be seen from other modules.<br>
  &emsp;<b>name</b>:   name of the rule<br>
  &emsp;<b>args</b>:   heritable rule's arguments<br>
  &emsp;<b>body</b>:   rule's body (production)<br>
</p>

<h2>Syntax in examples</h2>
<h3>Grammar</h3>
<pre>
grammar: ('{'header'}')? rules ('{'footer'}')?
</pre>

Example of YARD grammar file:
<pre>
{
let helperFunction x y = x+y
}
s: NUMBER;
{
let helperFunction2 x y = x*y
}
</pre>

<h3>Sequence</h3>
<pre>
s: e PLUS e;
</pre>
or in EBNF-like notation you can use
<pre>
s = e, PLUS, e;
</pre>

<h3>Alternative</h3>
<pre>
s: DECNUMBER|HEXNUMBER;
s: NUMBER (PLUS|MULT) NUMBER;
s: { None }|n=NUMBER{Some n};
</pre>
Note that empty branch is an epsilon, so s can produce epsilon or NUMBER. It is important that empty branch must have action code as it is necessary for type checking.

<h3>Zero or more</h3>
<pre>
grammar: rule*;
</pre>
or in EBNF-like notation you can use
<pre>
grammar = (: rule :);
</pre>

<h3>One or more</h3>
<pre>
grammar: rule+;
</pre>

<h3>Option</h3>
<pre>
s: PLUS?;
</pre>
or
<pre>
s: [PLUS];
</pre>
Square-bracketed syntax is useful for big optional subexpressions. Instead of 
<pre>
s: (n PLUS expr)?;
</pre>
you can write
<pre>
s: [n PLUS expr];
</pre>
which contains grouping and optionality.

<h3>Literal</h3>
<pre>
stmt_block: 'BEGIN' stmt+ 'END';
</pre>

<h3>Metarules</h3>
<pre>
not_empty_list&lt;item sep&gt;: item (sep item)*;
statements: not_empty_list&lt;statement SEMICOLON&gt;;
args: not_empty_list&lt;arg COMMA&gt;;
</pre>

<h3>Action code</h3>
<pre>
s: n {printfn "n is detected!!!"}
</pre>

<h3>Bindings</h3>
<pre>
s: n=NUMBER {printfn "Number value is %A" n}
s: &lt;hd::tl&gt;=not_empty_list&lt;NUMBER COLON&gt; {List.fold myFunction hd tl}
</pre>

<h3>L-attributes</h3>
<pre>
proc: declarations=var_declarations IN expressions[declarations];
expressions[declarations]: 
     expr_lst=expression+ {check_undeclared_variables_in_expressions expr_lst declarations};
</pre>

<h3>Conditional generation</h3>
<pre>
exec_literal:
&num;if ms
("EXEC" | "EXECUTE")
&num;elif pl
"EXECUTE" "IMMEDIATE"
&num;endif
LBRACE LITERAL RBRACE
;
</pre>
Means, that if you run generator with key -D "ms" then you get parser for grammar
<pre>
exec_literal:
("EXEC" | "EXECUTE")
LBRACE LITERAL RBRACE
;
</pre>
Else, if you run generator with key -D "pl" then you get parser for grammar
<pre>
exec_literal:
"EXECUTE" "IMMEDIATE"
LBRACE LITERAL RBRACE
;
</pre>

<h3>Labels</h3>
<pre>
s: x (@l1(y z)| @l2(a b)) g;
</pre>
Where @l1 and @l2 are labels. Labels are used for working with dialects.

<h2>Experimental syntax</h2>
<h3>Syntactic factor</h3>
Note that though syntactic factor is implemented in YARD, it is currently not supported by generators.

<pre>
  strict_rule = 5 * " ";
</pre>

Extended annotation for repetition rule:

<pre>
  grammar = rule *[2..5] ;
</pre>
*)