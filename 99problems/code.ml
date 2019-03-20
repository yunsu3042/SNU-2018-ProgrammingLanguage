let rand_select (input : 'a list) (number : int) : 'a list =
  let rec select (li : 'a list)(n : int) : 'a =
    match li with
    | x :: tl -> if n = 0 then x else select tl (n - 1)
    | [] :: -> in
  let rec add (n : int) : 'a list = 
    match n with
    | 0 -> []
    | x -> select input (Random.int (List.length input))
