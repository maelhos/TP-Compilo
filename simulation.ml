(*
  Le but de ce tp est de compiler des fonctions
  simples calcultant des expressions mathématiques (voir) :
  [ x, y ] 24*(x + 3*x) * y - 1 / y
  vers un assembleur très simple

  Les expressions suivent la grammaire formelle:

  G = {\BigSigma, V, S, R}
  L = {a, b, c, ..., z}
  W = L^+
  \BigSigma = {+, -, *, /, \bold,,[, ], (, )} \cup \mathbb Z \cup L
  V = {S, E, F}
  R : S -> [ J ] E
      J -> \epsilon | J W 
      E ->  T
        | E + E
        | E - E
      T -> F
        | E / E
        | E * E
      F -> \mathbb Z
        | W
        | (E)

*)

let simulate (prgm: Instr.t list) (args: int array) : int = 
  let r0 = ref 0 in
  let r1 = ref 0 in
  let stack = Stack.create () in
  let  run (i: Instr.t) : unit =
    match i with
      | Instr.IM n -> r0 := n
      | Instr.AR n -> r0 := args.(n)
      | Instr.SW -> let tmp = !r0 in (r0 := !r1; r1 := tmp)
      | Instr.PU -> Stack.push (!r0) stack
      | Instr.PO -> r0 := Stack.pop stack
      | Instr.AD -> r0 := !r0 + !r1
      | Instr.SU -> r0 := !r0 - !r1
      | Instr.MU -> r0 := !r0 * !r1
      | Instr.DI -> r0 := !r0 / !r1
    in List.iter run prgm; !r0;;




