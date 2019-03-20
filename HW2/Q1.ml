exception FreeVariable
type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

let rec sigma : int * int -> exp -> float = fun (init, bound) expr ->
  let fInit = (float_of_int init) in
  let fBound = (float_of_int bound) in
  if init == bound then cal (fBound, true) expr
  else (cal (fInit, true) expr) +. sigma (init + 1, bound) expr

and integral : float * float -> exp -> float = fun (init, bound) expr ->
  if abs_float(init -. bound) < 0.1 then 0.0
  else if init == bound then (cal (bound, true) expr) *. 0.1
  else ((cal (init, true) expr) *. 0.1) +. integral (init +. 0.1, bound) expr

and cal : (float * bool) -> exp -> float = fun (xVal, flag) value ->
  match value with
 | X -> if flag then xVal else raise FreeVariable
 | INT x -> (float_of_int x)
 | REAL x -> x
 | ADD (x, y) -> (cal (xVal, flag) x) +. (cal (xVal, flag) y)
 | SUB (x, y) -> (cal (xVal, flag) x) -. (cal (xVal, flag) y)
 | MUL (x, y) -> (cal (xVal, flag) x) *. (cal (xVal, flag) y)
 | DIV (x, y) -> (cal (xVal, flag) x) /. (cal (xVal, flag) y)
 | SIGMA (x, y, z) ->
   let init = int_of_float (cal (xVal, flag) x) in
   let bound = int_of_float (cal (xVal, flag) y) in
   if init > bound then 0.0
   else sigma (init, bound) z
 | INTEGRAL (x, y, z) ->
   let init =  cal (xVal, flag) x in
   let bound = cal (xVal, flag) y in
   if init > bound  then -. (integral (bound, init) z)
   else integral (init, bound) z

let calculate : exp -> float = fun expr ->
  cal (0.0, false) expr

(* let x = calculate(INTEGRAL(REAL 1.0, REAL (-5.0), X)) *)
