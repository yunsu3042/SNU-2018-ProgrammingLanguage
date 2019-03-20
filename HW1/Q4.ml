type formula = TRUE | FALSE | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr

and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec cal : expr -> int = fun expr ->
  match expr with
   | NUM x -> x
   | PLUS (x, y) -> cal x + cal y
   | MINUS (x, y) -> cal x - cal y

let rec eval : formula -> bool = fun wff ->
  match wff with
  | TRUE -> true
  | FALSE -> false
  | NOT form -> not (eval form)
  | ANDALSO (lform, rform) -> eval lform && eval rform
  | ORELSE (lform , rform) -> eval lform || eval rform
  | IMPLY (lform, rform) -> (eval lform && eval rform) || not (eval lform)
  | LESS (lexpr, rexpr) -> cal lexpr < cal rexpr





(* let expr0 = NUM 3
let expr1 = PLUS (NUM 10, NUM 20)
let expr2 = MINUS (NUM 20, NUM 10)
let wff = LESS (expr1, expr2)
let wff1 = FALSE
let wff2 = FALSE
let wff3 = IMPLY(wff1, wff2)

let result = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8)))
let _ = print_endline (string_of_int(cal expr1))
let _ = print_endline (string_of_int(cal expr2))
let _ = print_endline (string_of_bool(eval wff3))
let _ = print_endline (string_of_bool result) *)
