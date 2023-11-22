
let rec opti_constants (ast: Ast.ast) : Ast.ast = 
  match ast with
  | Ast.Add (a, b) -> begin
    match opti_constants a, opti_constants b with
      | Ast.Imm a, Ast.Imm b -> Ast.Imm (a + b)
      |(a, b) -> Ast.Add(a, b)
  end
  | Ast.Sub (a, b) -> begin
    match opti_constants a, opti_constants b with
      | Ast.Imm a, Ast.Imm b -> Ast.Imm (a - b)
      | (a, b) -> Ast.Sub(a, b)
  end
  | Ast.Mul (a, b) -> begin
    match opti_constants a, opti_constants b with
      | Ast.Imm a, Ast.Imm b -> Ast.Imm (a * b)
      | (a, b) -> Ast.Mul(a, b)
  end
  | Ast.Div (a, b) -> begin
    match opti_constants a, opti_constants b with
      | Ast.Imm a, Ast.Imm b -> Ast.Imm (a / b)
      | (a, b) -> Ast.Div(a, b)
  end
  | _ -> ast