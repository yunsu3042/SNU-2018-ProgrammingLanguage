(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val fault : t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
  val toInt : t -> int
end

module Loc : LOC =
struct
  type t = Location of int
  let fault = Location(-1)
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
  let toInt (Location(a)) = a
end

(* Memory Signature *)
module type MEM =
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  (* U는 Unallocated *)
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c ->
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr"))
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec recAlloc n mem lacc =
  match n with
  | 0 -> (mem, List.rev lacc)
  | x -> (
    let (l, mem') = Mem.alloc mem in
    recAlloc (n - 1) mem' (l :: lacc)
  )

  let rec recBind env id_list l_list =
    match (id_list, l_list) with
    | (x :: id_tl, l :: l_tl) -> (
        let env' = Env.bind env x (Addr l) in
        recBind env' id_tl l_tl
      )
    | _ -> env

  let rec recStore mem l_list v_list =
    match (l_list, v_list) with
    | (l :: l_tl, v :: v_tl) -> (
        let mem' =  Mem.store mem l v in
        recStore mem' l_tl v_tl
      )
    | _ -> mem

  let rec recGetLoc env id_list lacc=
    match id_list with
    | id :: tl -> (
         let l = lookup_env_loc env id in
         recGetLoc env tl (l :: lacc)
      )
    | _ -> List.rev lacc

  let rec splitTuple tuple_list xacc yacc =
    match tuple_list with
    | (x, y) :: tl -> splitTuple tl (x :: xacc) (y:: yacc)
    | _ -> ((List.rev xacc), (List.rev yacc))

  let bindRecord rfun id loc =
    fun x-> if x = id then loc else rfun x

  let rec makeRecord rfun id_list l_list =
    match (id_list, l_list) with
    | (id :: i_tl, loc :: l_tl) -> (
        let rfun' = bindRecord rfun id loc in
        makeRecord rfun' i_tl l_tl
      )
    | _ -> Record(rfun)

  let rec eval mem env e =
    match e with
    (*WRITEBOOL 제출 전에 꼭 삭제하기*)
    | READ x ->
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | VAR x ->
      let l = lookup_env_loc env x in
      ((Mem.load mem l), mem)
    | TRUE ->
      (Bool(true), mem)
    | FALSE ->
      (Bool(false), mem)
    | NUM n ->
      (Num(n), mem)
    | UNIT ->
      (Unit, mem)
    | ADD (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num(n1 + n2), mem'')
    | SUB (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num(n1 - n2), mem'')
    | MUL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num(n1 * n2), mem'')
    | DIV (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num(n1 / n2), mem'')
    | EQUAL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      (
        match (v1, v2) with
        | Num n1, Num n2 -> if n1 = n2 then (Bool(true), mem'') else (Bool(false), mem'')
        | Bool b1, Bool b2 -> if b1 = b2 then (Bool(true), mem'') else (Bool(false), mem'')
        | Unit, Unit -> (Bool(true), mem'')
        | _ -> (Bool(false), mem'')
      )
    | LESS (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      if n1 < n2 then (Bool(true), mem'') else (Bool(false), mem'')
    | NOT e ->
      let (v, mem') = eval mem env e in
      let b = value_bool v in
      (Bool(not b), mem')
    | SEQ (e1, e2) ->
      let (v, mem') = eval mem env e1 in
      eval mem' env e2
    | IF (e1, e2, e3) ->
      let (v, mem') = eval mem env e1 in
      let b = value_bool v in
      if b = true then eval mem' env e2 else eval mem' env e3
    | WHILE (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let b = value_bool v1 in
      if b = true then (
          let (_, mem'') = (eval mem' env e2) in
          eval mem'' env (WHILE (e1, e2))
        )
        else (Unit, mem')

    | LETF (f, xlist, e1, e2) ->
      let env' = Env.bind env f (Proc(xlist, e1, env)) in
      eval mem env' e2

    | CALLV  (f, elist) ->
      let (v_list, mem_N) = recEval mem env elist [] in
      let (id_list, e', env') = lookup_env_proc env f in
      if (List.length elist) = (List.length id_list)
        then(
          let (mem_N', l_list) = recAlloc (List.length id_list) mem_N [] in
          let env'' = recBind env' id_list l_list in
          let mem_N'' = recStore mem_N' l_list v_list in
          let env''' = Env.bind env'' f (Proc(id_list, e', env')) in
          eval mem_N'' env''' e'
        )
        else raise (Error "InvalidArg")

    | CALLR (f, id_list) ->
      let l_list = recGetLoc env id_list [] in
      let (x_list, e, env') = lookup_env_proc env f in
      if (List.length x_list) = (List.length id_list)
        then (
          let env'' = recBind env' x_list l_list in
          eval mem env'' e
        )
        else raise (Error "InvalidArg")

    | RECORD tuple_list ->
      let (id_list, e_list) = splitTuple tuple_list [] [] in
      if List.length id_list = 0
        then (Unit, mem)
        else (
          let (v_list, mem') = recEval mem env e_list [] in
          let (mem'', l_list) = recAlloc (List.length id_list) mem' [] in
          let mem''' = recStore mem'' l_list v_list in
          let rfun = fun x -> (Loc.fault) in
          let r = makeRecord rfun id_list l_list in
          (r, mem''')
        )

    | FIELD (e, x) ->
      let (Record(rfun), mem') = eval mem env e in
      let loc = rfun x in
      if loc = Loc.fault then raise (Error "Unbound") else
      (
        let v = Mem.load mem' loc in
        (v, mem')
      )

    | ASSIGNF (e1, x, e2) ->
      let (Record(rfun), mem') = eval mem env e1 in
      let (v, mem'') = eval mem' env e2 in
      let loc = rfun x in
      let mem''' = Mem.store mem loc v in
      (v, mem''')


    (* TODO : Implement rest of the cases *)
    | _ -> failwith "Unimplemented"

  and recEval mem env e_list vacc =
    match e_list with
    | hd :: tl ->
      (
        let (v, mem') = (eval mem env hd) in
        recEval mem' env tl (v::vacc)
      )
    | [] -> (List.rev vacc, mem)



  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end
