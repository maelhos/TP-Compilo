

type token =
  | ADD | SUB | MUL | DIV | LPAR | RPAR | RBRA | LBRA
  | VAR of string
  | CONST of int;;