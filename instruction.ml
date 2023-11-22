

type instr = 
  | IM of int
  | AR of int
  | SW | PU | PO | AD | SU | MU | DI;;

let simulate (prgm: instr list) (args: int array) : int = 
  let r0 = ref 0 in
  let r1 = ref 0 in
  let stack = Stack.create () in
  let rec run (prgm: instr list) : int =
    match prgm with
    | [] -> !r0
    | h::t -> 
      ((match h with
      | IM n -> r0 := n
      | AR n -> r0 := args.(n)
      | SW -> let tmp = !r0 in (r0 := !r1; r1 := tmp)
      | PU -> Stack.push (!r0) stack
      | PO -> r0 := Stack.pop stack
      | AD -> r0 := !r0 + !r1
      | SU -> r0 := !r0 - !r1
      | MU -> r0 := !r0 * !r1
      | DI -> r0 := !r0 / !r1
        ); run t )
    in run prgm;;




