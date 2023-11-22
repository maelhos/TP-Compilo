type ast =
  | Imm of int (* valeur immediate *)
  | Arg of int (* reference au nieme argument *)
  | Add of (ast * ast) (* addition des deux sous-arbres *)
  | Sub of (ast * ast) (* soustraction *)
  | Mul of (ast * ast) (* multiplication *)
  | Div of (ast * ast) (* division *);;

(* exemples :
  ast de [ x y ] ( x + y ) / 2 
*)
let ast1 = Div(Add(Arg 0, Arg 1), Imm 2);;

(* 
  ast de [ x ] x + 2*5 
*)
let ast2 = Add(Arg 0, Mul(Imm 2, Imm 5));;


(* Erreur associee a la non-implementation : *)
exception CompilerError of string;;
