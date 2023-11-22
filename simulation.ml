(*
  Le but de ce tp est de compiler des fonctions
  simples calcultant des expressions mathématiques (voir) :
  [ x, y ] 24*(x + 3x) * y - 1 / y
  vers un assembleur très simple

  Les expressions suivent la grammaire formelle:

  G = {\BigSigma, V, S, R}
  L = {a, b, c, ..., z}
  W = L^+
  \BigSigma = {+, -, *, /, \bold,,[, ], (, )} \cup \mathbb Z \cup L
  V = {S, E, F}
  R : S -> [ F ] E
      F -> F,F | x \in W 
      E ->  E + E
        | E - E
        | E * E
        | E / E
        | (E)
        | x \in W

*)

type instr = 
  | IM of int
  | AR of int
  | SW | PU | PO | AD | SU | MU | DI;;

let simulate (prgm: instr list) (args: int array) : int = 
  let r0 = ref 0 in
  let r1 = ref 0 in
  let stack = Stack.create () in
  let  run (i: instr) : unit =
    match i with
      | IM n -> r0 := n
      | AR n -> r0 := args.(n)
      | SW -> let tmp = !r0 in (r0 := !r1; r1 := tmp)
      | PU -> Stack.push (!r0) stack
      | PO -> r0 := Stack.pop stack
      | AD -> r0 := !r0 + !r1
      | SU -> r0 := !r0 - !r1
      | MU -> r0 := !r0 * !r1
      | DI -> r0 := !r0 / !r1
    in List.iter run prgm; !r0;;




