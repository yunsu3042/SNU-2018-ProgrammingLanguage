exception InvalidArgument

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let diff (a, str) =
  let rec aux var = function
    | CONST x -> CONST 0
    | VAR x -> if (compare x var) = 0 then CONST 1 else VAR x
    | POWER (x, n) ->
      if (compare x var) = 0 then TIMES [CONST n; POWER (x, (n - 1))]
      else CONST 0
    | TIMES (x :: tl) ->begin
    match tl with
      | [] -> aux var x
      | tl -> SUM [TIMES (aux var x:: tl); TIMES [x; aux var (TIMES tl)]]
    end
    | TIMES [] -> raise InvalidArgument
    | SUM (x :: tl) -> begin
      match tl with
      | [] -> aux var x
      | tl -> SUM [aux var x; aux var (SUM tl)]
      end
    | SUM [] -> raise InvalidArgument in
  aux str a

(* let form = SUM ([TIMES [CONST 5; TIMES([VAR "x";VAR "x"])]; CONST 1])
let result = diff (form, "x") *)
