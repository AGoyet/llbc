/* file: parser.mly */
/* Parser for llbc. */


%{
open Llbc
%}

%token EOF
%token LPAREN RPAREN POINT UNDERSCORE COMMA LBRACKET RBRACKET SEMICOLON
%token <string> IDENT
%token P M
%token EP LP PP MP CP 
%token EN LN PN MN BT
%token NU

%left P PP PN
%left M MP MN
%left LP LN UNDERSCORE CP EP EN POINT NU
%left LPAREN RIPAREN
%left COMMA LBRACKET RBRACKET SEMICOLON IDENT
%left EOF


%start input
%type <Llbc.term> input


%% /* Grammar rules and actions follow */

input:    EOF  		{ raise Empty}
        | termp EOF	        { Term_p($1) }
        | termn EOF	        { Term_n($1) }
;

termp:   LPAREN termp RPAREN  {$2} 
        | EP             {Ep}
        | LP IDENT POINT termp { Lp(Var_p($2),$4) }
        | IDENT UNDERSCORE termn  { Vp(Var_p($1),$3) }
        | termp PP termp { Pp($1,$3) }
        | termp P termp  { Pp($1,$3) }
        | termp MP termn { Mp($1,$3) }
        | termp M termn  { Mp($1,$3) }
        | CP links stack POINT termp { Cp($2,$3,$5) }
        | NU IDENT POINT termp {Nu(Var_p($2),$4)}

links:    /* empty */    { [] }
        | LPAREN IDENT COMMA IDENT RPAREN links  { (Var_p($2), Var_n($4))::$6 }

stack:   /* empty */    { [] }
        | LBRACKET inside_stack RBRACKET { $2 }

inside_stack:   /* empty */    { [] }
              |  IDENT { [Var_n($1)] }
              |  IDENT SEMICOLON inside_stack { Var_n($1)::$3 }

termn:   LPAREN termn RPAREN  {$2} 
        | EN             {En}
        | LN IDENT POINT termn { Ln(Var_n($2),$4) }
        | IDENT UNDERSCORE termp  { Vn(Var_n($1),$3) }
        | termn PN termn { Pn($1,$3) }
        | termn P termn  { Pn($1,$3) }
        | termn MN termn { Mn($1,$3) }
        | termn M termn  { Mn($1,$3) }
        | BT termp  { Bt($2) }

;

%%

