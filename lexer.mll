(* file: lexer.mll *)
{
  open Parser
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
  | "pn"        {PN}
  | "mn"        {MN}
  | "bt"        {BT}
  | "BT"        {BT}

  | "ε"        {EP}
  | "λ"        {LP}
  | "ν"        {NU}

  | "ὲ"        {EN}
  | "ƛ"        {LN}

  | "̅ε"        {EN}
  | "̅λ"        {LN}
  
  | ident ident_num* as word  { IDENT(word) }
  | eof		{ EOF }
