
(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, unknown characters
   and raises End_of_file on EOF. *)
{
  open Parser (* Assumes the parser file is "rtcalc.mly". *)
  open Llbc
}

let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9' '\'' ]

rule token = parse
  | [' ' '\t' '\n']	{ token lexbuf }
  | '('         {LPAREN}
  | ')'         {RPAREN}
  | '['         {LBRACKET}
  | ']'         {RBRACKET}
  | '.'     {POINT}
  | '_'   {UNDERSCORE}
  | ','   {COMMA}
  | ';'   {SEMICOLON}


  | 'e'  {EP}
  | 'l'  {LP}
  | '+'  {P}
  | '<'  {M}
  | 'V'  {CP}

  | "ep"        {EP}
  | "lp"        {LP}
  | "pp"        {PP}
  | "mp"        {MP}
  | "cp"        {CP}
  | "nu"        {NU}

  | "en"        {EN}
  | "ln"        {LN}
  | "nn"        {PN}
  | "mn"        {MN}
  | "bt"       {BT}
  | "BT"       {BT}

  | "ε"        {EP}
  | "λ"        {LP}
  | "ν"        {NU}

  | "ὲ"        {EN}
  | "ƛ"        {LN}

  | ident ident_num* as word  { IDENT(word) }
  | eof		{ EOF }
