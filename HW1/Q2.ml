let rec helperK :  (int * int -> float) * int * int * int-> float = fun (m, i, j, k) ->
  if k < 1 then 1.0
  else if j == k then m (i, j)
  else m (i, j)  *. helperK(m, i, j + 1, k)

let rec helperN : (int * int -> float) * int * int * int -> float = fun (m, i, n, k) ->
  if n < 1 then 0.0
  else if i == n then helperK (m, i, 1, k)
  else helperK (m, i, 1, k) +. helperN (m, i + 1, n, k)

let sumprod : (int * int -> float) * int * int -> float = fun (m, n, k) ->
  helperN (m, 1, n, k)

(*
let m : int * int -> float = fun (a, b) ->
  float a +. float b

let matrix (i, j) = ((float_of_int i) *. 10.) +. (float_of_int j)
let result = sumprod (matrix, 10, 0)

let _ = print_endline (string_of_float result) *)
