

//
// An simple expression parser, to demonstrate
// ANTLR's creation of "sibling" list trees.
// by
// Ian Kaplan
//


options {
	language="Cpp";
}


exprlist
  : ( assignment_statement )* EOF!
  ;

assignment_statement
  : assignment SEMICOLON!
  ;

assignment
  : (IDENT ASSIGN )? expr
  ;

primary_expr
  : IDENT 
  | constant 
  | (LPAREN! expr RPAREN! ) 
  ;

sign_expr
  : (MINUS)? primary_expr
  ;

mul_expr
  : sign_expr (( TIMES | DIVIDE | MOD ) sign_expr)*
  ;

expr
  : mul_expr (( PLUS | MINUS ) mul_expr)*
  ;


constant
  : (ICON | CHCON)
  ;

WS_     :       (' '
        |       '\t'
        |       '\n'
        |       '\r')
                { _ttype = Token::SKIP; }
        ;

IDENT
options {
	paraphrase = "identifier";
}
  :  ('a'..'z' | 'A'..'Z' | '_' ) ( ('a'..'z' | 'A'..'Z' | '_') | ('0'..'9' ))*
  ;

ICON
options {
	paraphrase = "integer constant";
}
  : '0'..'9' ('0'..'9')*
  ;

CHCON
options {
	paraphrase = "character constant";
}
  : "'" '\0'..'\255' "'"
  ;

COMMA
options {
	paraphrase = ",";
}
  : ','
  ;

SEMICOLON
options {
	paraphrase = ";";
}
  : ';'
  ;

LPAREN
options {
	paraphrase = "(";
}
  : '('
  ;

RPAREN
options {
	paraphrase = ")";
}
  : ')'
  ;


LCURL
options {
	paraphrase = "{";
}
  : '{'
  ;

RCURL
options {
	paraphrase = "}";
}
  : '}'
  ;

PLUS
options {
	paraphrase = "+";
}
  : '+'
  ;

MINUS
options {
	paraphrase = "-";
}
  : '-'
  ;

TIMES
options {
	paraphrase = "*";
}
  : '*'
  ;

DIVIDE
options {
	paraphrase = "/";
}
  : '/'
  ;

MOD
options {
	paraphrase = "%";
}
  : '%'
  ;

ASSIGN
options {
	paraphrase = "=";
}
  : '='
  ;



