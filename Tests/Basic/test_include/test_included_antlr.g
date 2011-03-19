

//
// An simple expression parser, to demonstrate
// ANTLR's creation of "sibling" list trees.
// by
// Ian Kaplan
//



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
