let rec sweep_S (s : svalue list)(locs : (loc list) ref) : (locs list) ref =
  match s with
  | hd :: tl ->
    (match hd with
    | V (v) ->
      (match v with
      | L (l) -> let _ = (locs := l :: !locs)
      | _ -> ())
    | P (f, c, e) -> let _ = (locs := !(sweep_E e locs))
    | M (id, eval) ->
      (match eval with
      | Loc (l) -> let _ = (locs := l :: !locs)
      | Proc (x, c, e') -> let _ = (locs := !(sweep_E e' locs))))
    sweep_S tl locs
  | [] -> locs

let rec sweep_E (e : (string * evalue) list)(locs : (loc list) ref) : (loc list) ref =
  match e with
  | (id, eval) :: tl ->
   (match eval with
    | Loc (l) ->
        let _ = (locs := l :: !locs) in
        sweep_E tl locs
    | Proc (x, c, e') ->
        let _ = (locs := !(sweep_E e' locs)) in
        sweep_E tl locs)
  | [] -> locs


let rec sweep_k (k: (command * environment) list) (locs : (loc list) ref)
: (loc list) ref  =
  match k with
  | (c, e) :: tl ->
    let _ = (locs := !(sweep_E e locs)) in
    sweep_k tl locs
  | [] -> locs
