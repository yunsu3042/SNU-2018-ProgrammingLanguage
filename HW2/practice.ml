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
  if init == bound then cal fBound expr
  else (cal fInit expr) +. sigma (init + 1, bound) expr

and integral : float * float -> exp -> float = fun (init, bound) expr ->
  if abs_float(init -. bound) < 0.1 then 0.0
  else if init == bound then (cal bound expr) *. 0.1
  else ((cal init expr) *. 0.1) +. integral (init +. 0.1, bound) expr


and cal : float -> exp -> float = fun xVal value ->
  match value with
 | X -> xVal
 | INT x -> (float_of_int x)
 | REAL x -> x
 | ADD (x, y) -> (cal xVal x) +. (cal xVal y)
 | SUB (x, y) -> (cal xVal x) -. (cal xVal y)
 | MUL (x, y) -> (cal xVal x) *. (cal xVal y)
 | DIV (x, y) -> (cal xVal x) /. (cal xVal y)
 | SIGMA (x, y, z) ->
   let init = int_of_float (cal xVal x) in
   let bound = int_of_float (cal xVal y) in
   if init > bound then 0.0
   else sigma (init, bound) z
 | INTEGRAL (x, y, z) ->
   let init =  cal xVal x in
   let bound = cal xVal y in
   if init > bound  then -. (integral (bound, init) z)
   else integral (init, bound) z


let calculate : exp -> float = fun expr ->
  cal 0.0 expr

let form = SIGMA(INT 1, INT 2, SUB(MUL(X, X), INT 1))
let form2 = INTEGRAL(REAL 1.0, REAL 100.0, SUB(MUL(X, X), INT 1))
let form3 = INTEGRAL(REAL 1.0, REAL 2.0, SUB(MUL(X, X), INT 1))
let u = calculate form
let v = calculate form2
let w = calculate form3


let _ = print_endline (string_of_float u)
let _ = print_endline (string_of_float v)
