module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ: queue = [], []
    let enQ ((inQ: queue), (elem: element)) : queue =
      match inQ with
      | x, y -> (elem :: x, y)
    let deQ (inQ: queue) : element * queue =
      match inQ with
      | [], [] -> raise EMPTY_Q
      | x, [] ->
        let reverse = List.rev x in
        (List.hd reverse, ([], List.tl reverse))
      | [], x ->  (List.hd x, ([], List.tl x))
      | x, y -> (List.hd y, (x, List.tl y))
  end


(* let myQ = IntListQ.emptyQ

let oneQ = IntListQ.enQ (myQ, [100])

let addQ = IntListQ.enQ (addQ, [])
let (pop, subQ) = IntListQ.deQ oneQ; *)
