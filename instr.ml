
type t = 
  | IM of int
  | AR of int
  | SW | PU | PO | AD | SU | MU | DI;;

let print_instr (i: t) : unit =
  match i with
  | IM n -> print_string "IM "; print_int n
  | AR n -> print_string "AR "; print_int n
  | SW -> print_string "SW"
  | PU -> print_string "PU"
  | PO -> print_string "PO"
  | AD -> print_string "AD"
  | SU -> print_string "SU"
  | MU -> print_string "MU"
  | DI -> print_string "DI";;

let print_prgm (prgm: t list) : unit = 
  print_char '['; List.iter (fun a -> print_instr a; print_string ", ") prgm; print_char ']';;