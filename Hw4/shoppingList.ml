module SS = Set.Make(String)
type require = id * (cond list)
and cond = Items of gift list
         | Same of id
         | Common of cond * cond
         | Except of cond * gift list

and gift = int
and id = A | B | C | D | E

let empty = SS.empty

let print_set s =
     SS.iter print_endline s

let con1 = Items( [1; 2])
let con2 = Common(Same(B), Same(C))
let con3 = Common(Same(C), Items([2;3]))
let con4 = Except(Same(A), [3])

let emptySets = [empty; empty; empty; empty; empty]

let rec update(sets: SS.t list)(x: SS.t)(n : int) : SS.t list =
  match sets with
  | hd :: tl -> if n = 0 then x :: tl else hd :: (update tl x (n-1))
  | _ -> []

let get_idx(name: id) =
  match name with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4

let iList_to_sList (intList : int list) =
  let rec aux (il: int list) =
    match il with
    | hd :: tl -> (
      let c = string_of_int hd in
      c :: aux tl
      )
    | _ -> [] in
  List.rev (aux intList)

let get_set (setList : SS.t list) (n: int) =
  List.nth setList n

let makeSet(str_list : string list) : SS.t =
  List.fold_right SS.add str_list empty

let rec is_equal_set_list(setL1 : SS.t list) (setL2 : SS.t list) =
  match (setL1, setL2) with
  | (hd1 :: tl1, hd2 ::tl2) -> (
      let cf = SS.equal hd1 hd2 in
      if cf then is_equal_set_list tl1 tl2 else false
    )
  | _ -> true

let rec do_do_do (con : cond) (sets: SS.t list) : SS.t =
  match con with
  | Items(gl) -> (
      let str_list = iList_to_sList gl in
      let set = makeSet str_list in
      set
    )
  | Same(name) -> (
      let nameIdx = get_idx(name) in
      let set = get_set sets nameIdx in
      set
    )
  | Common(c1, c2) -> (
      let con1 = do_do_do c1 sets in
      let con2 = do_do_do c2 sets in
      let set = SS.inter con1 con2 in
      set
    )
  | Except(c1, gl) -> (
      let con1 = do_do_do c1 sets in
      let str_list = iList_to_sList gl in
      let diffSet = makeSet str_list in
      let set = SS.diff con1 diffSet in
      set
    )

let rec do_do (name: id)(condList : cond list)(setList : SS.t list) =
  let n = get_idx(name) in
  let curSet = get_set setList n in

  match condList with
  | Items(gl) :: tl -> (
      let str_list = iList_to_sList gl  in
      let toAddset = makeSet str_list in
      let set = SS.union toAddset curSet in
      let setList' = update setList set n in
      do_do name tl setList'
    )
  | Same(setName) :: tl -> (
      let setIdx = get_idx(setName) in
      let toUnionset = get_set setList setIdx in
      let set = SS.union toUnionset curSet in
      let setList' = update setList set n in
      do_do name tl setList'
    )
  | Common(c1, c2) :: tl -> (
      let set1 = do_do_do c1 setList in
      let set2 = do_do_do c2 setList in
      let setCommon = SS.inter set1 set2 in
      let set = SS.union curSet setCommon in
      let setList' = update setList set n in
      do_do name tl setList'
    )
  | Except(c1, gl) :: tl -> (
      let set1 = do_do_do c1 setList in
      let str_list = iList_to_sList gl in
      let set2 = makeSet str_list in
      let setExcept = SS.diff set1 set2 in
      let set = SS.union curSet setExcept in
      let setList' = update setList set n in
      do_do name tl setList'
    )
  | _ -> setList

let rec run (rList : require list)(setList : SS.t list) =
  match rList with
  | (name, condList) :: tl -> (
      let setList' = do_do name condList setList in
      run tl setList'
    )
  | _ -> setList

let convert_set (set : SS.t) =
  let string_list = SS.elements set in
  let int_list = List.map int_of_string string_list in
  List.sort compare int_list

let rec do_while (rList : require list)(setList : SS.t list) =
  let setList' = run rList setList in
  let equal = is_equal_set_list setList setList' in
  if equal then setList else do_while rList setList'

let mapping_set (setList : SS.t list) =
  let rec aux (setList: SS.t list) (n : int) =
    match setList with
    | hd :: tl -> (
      let gl = convert_set hd in
      match n with
      | 0 -> (A, gl) :: aux tl (n + 1)
      | 1 -> (B, gl) :: aux tl (n + 1)
      | 2 -> (C, gl) :: aux tl (n + 1)
      | 3 -> (D, gl) :: aux tl (n + 1)
      | 4 -> (E, gl) :: aux tl (n + 1)
      | _ -> []
      )
    | _ -> [] in
  aux setList 0

let rec find (l : require list)(name : id) =
  match l with
  | (na, _) as hd :: tl -> if na = name then hd else find tl name
  | _ -> List.nth l 0

let require_sort (l: require list) =
  let r1 = find l A in
  let r2 = find l B in
  let r3 = find l C in
  let r4 = find l D in
  let r5 = find l E in
  r1 :: r2 :: r3 :: r4 :: r5 :: []

let shoppingList (l : require list) : (id * gift list) list =
  let emptySetL = [empty;empty;empty;empty;empty] in
  let require_list = require_sort l in
  let setList = do_while require_list emptySetL in
  mapping_set setList
