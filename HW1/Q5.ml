type nat = ZERO | SUCC of nat

let rec nat_of_int : int -> nat = fun i ->
  if i == 0 then ZERO
  else if i > 0 then SUCC (nat_of_int (i - 1))
  else SUCC (nat_of_int (i + 1))


let rec int_of_nat : nat -> int = fun natural ->
  match natural with
  | ZERO -> 0
  | SUCC natObj -> 1 + int_of_nat(natObj)


let natadd : nat * nat -> nat = fun (a, b) ->
  let x = int_of_nat a in
  let y = int_of_nat b in
  let z : int = x + y in
  nat_of_int z

let natmul : nat * nat -> nat = fun (a, b) ->
  let x = int_of_nat a in
  let y = int_of_nat b in
  let z : int = x * y in
  nat_of_int z

(* let nat1 : nat = nat_of_int 2
let nat2 : nat = nat_of_int 3

let nat3 : nat = natadd (nat1, nat2)
let nat4 : nat = natmul (nat1, nat2)

let result1 : int = int_of_nat nat3
let result2 : int = int_of_nat nat4


let test = natadd (ZERO, (SUCC (SUCC ZERO)))
let test2 = natmul (SUCC (SUCC (SUCC (SUCC ZERO))), SUCC (SUCC (SUCC ZERO)))
let _ = print_endline (string_of_int result1)
let _ = print_endline (string_of_int result2)
let _ = print_endline (string_of_int (int_of_nat test))
let _ = print_endline (string_of_int (int_of_nat test2)) *)
