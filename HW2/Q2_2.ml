exception InvalidArgument

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let diff (a,str) =
  let rec help (var: string) (acc: ae list) (input: ae list) : ae =
  match input with
    | CONST x :: t -> help var ((aux var x) :: acc) t
    | VAR x :: t -> help var ( (aux var x) :: acc) t
    | POWER (x, n) :: t -> help var ((aux var (POWER (x, n))) :: acc) t
    | TIMES x -> begin
    match x with
      | [] -> raise InvalidArgument
      | x ::(_ :: _ as tl) -> SUM [TIMES (help var x:: tl); TIMES (x :: help var tl)]
      | x -> help var x
    end

    | SUM x :: t -> begin
      match t with
      | [] -> help var x :: []
      | tl -> SUM [help var x; help var tl]
    end
    | [] -> acc
    | SUM [] -> raise InvalidArgument

  and aux var = function
  | CONST x -> CONST 0
  | VAR x -> if (compare x var) = 0 then CONST 0 else VAR x
  | POWER (x, n) ->
    if (compare x var) = 0 then TIMES [CONST n; POWER (x, (n - 1))]
    else CONST 0
  | TIMES x -> help var x
  | SUM x -> help var x in
  aux str a
