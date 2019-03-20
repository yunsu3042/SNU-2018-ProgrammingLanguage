exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank h =
  match h with
  | EMPTY -> -1
  | NODE (r,_ ,_ ,_ ) -> r

let findMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_ ,x,_ ,_ ) -> x

let shake (x,lh,rh) =
  if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let value h =
  match h with
  | EMPTY -> -1
  | NODE(_, x, _, _) -> x

let left h =
  match h with
  | EMPTY -> EMPTY
  | NODE(_, _, lh, _) -> lh

let right h =
  match h with
  | EMPTY -> EMPTY
  | NODE(_, _, _, rh) -> rh

let rec merge ((rA: heap), (rB:heap)) : heap =
  match rA, rB with
  | EMPTY, h -> h
  | h, EMPTY -> h
  | a, b -> (
    if (value a) < (value b) then shake (value a, left a, merge(right a, b))
    else shake (value b, left b, merge(right b, a))
    )

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_ ,x,lh,rh) -> merge(lh,rh)


(* let heap1 = NODE (1, 1, NODE (0, 5, EMPTY, EMPTY), NODE (0, 3, EMPTY, EMPTY))
let heap2 = NODE (0, 2, NODE (0, 4, EMPTY, EMPTY), EMPTY)
let oneNode = NODE (0, 5, EMPTY, EMPTY)
let root = merge (heap1, heap2)

let x = deleteMin x
let y = findMin x *)
