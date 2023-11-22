
let transpo (ast: Ast.ast) : Instr.t * Ast.ast * Ast.ast = 
  match ast with
  | Ast.Add(a, b) -> (Instr.AD, a, b)
  | Ast.Sub(a, b) -> (Instr.SU, a, b)
  | Ast.Mul(a, b) -> (Instr.MU, a, b)
  | Ast.Div(a, b) -> (Instr.DI, a, b)
  | _ -> failwith "Ne devrais pas être transposé ici ..."

let assemble (ast: Ast.ast) : Instr.t list = 
  let ret = ref [] in 
  let rec aux (ast: Ast.ast) (aleft: bool) : unit = 
    match ast with
    | Ast.Imm n -> begin
      (if not aleft then
        ret := Instr.SW::!ret);
        ret := (Instr.IM n)::!ret;
      (if not aleft then
        ret := Instr.SW::!ret)
      end
    | Ast.Arg n -> begin
      (if not aleft then
        ret := Instr.SW::!ret);
        ret := (Instr.AR n)::!ret;
      (if not aleft then
        ret := Instr.SW::!ret)
      end
    | _ -> let op, left_ast, right_ast = transpo ast in begin
      if not aleft then
        ret := Instr.PU::!ret;

      aux left_ast true;
      aux right_ast false;
      ret := op::!ret;

      if not aleft then
        ret := Instr.PO::!ret;
    end
  in aux ast true;