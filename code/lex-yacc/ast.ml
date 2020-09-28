type data = int

type expr =
| NumExpr of int
| PlusExpr of expr * expr
| MinusExpr of expr * expr
| TimesExpr of expr * expr
| DivExpr of expr * expr
| NegExpr of expr

let rec eval = fun e -> match e with
| NumExpr(n) -> n
| PlusExpr(e1,e2) -> (eval e1) + (eval e2)
| MinusExpr(e1,e2) -> (eval e1) - (eval e2)
| TimesExpr(e1,e2) -> (eval e1) * (eval e2)
| DivExpr(e1,e2) -> (eval e1) / (eval e2)
| NegExpr(e) -> -(eval e)

let rec str_expr = fun e -> match e with
| NumExpr(n) -> Printf.sprintf "%d" n
| PlusExpr(e1,e2) -> Printf.sprintf "%s + %s" (str_expr e1) (str_expr e2)
| MinusExpr(e1,e2) -> Printf.sprintf "%s - %s" (str_expr e1) (str_expr e2)
| TimesExpr(e1,e2) -> Printf.sprintf "%s * %s" (str_expr e1) (str_expr e2)
| DivExpr(e1,e2) -> Printf.sprintf "%s / %s" (str_expr e1) (str_expr e2)
| NegExpr(e) -> Printf.sprintf "-%s" (str_expr e)
