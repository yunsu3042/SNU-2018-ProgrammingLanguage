
let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
  if a > b then 0
  else if a == b then f b
  else f a + sigma (a + 1, b, f)



(* let k = sigma (1, 10, fun x -> x)
let _ = print_endline (string_of_int k)

let f n: int = n
let _ = print_endline (string_of_int (sigma (1, 0, f))) *)
