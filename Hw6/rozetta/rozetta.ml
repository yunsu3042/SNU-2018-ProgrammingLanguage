(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 *)

let common_loc = "l#"
let continue = "CONTINUE#"
let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
    let fun_body =
    (* 1. bind continue function on stack *)
    [Sonata.BIND continue] @
    (* 2. push continue function on stack *)
    trans' begin
      command @
      Sm5.PUSH (Sm5.Id continue) ::
      Sm5.PUSH (Sm5.Val Sm5.Unit) ::
      Sm5.PUSH (Sm5.Id common_loc) :: [Sm5.CALL]
    end
    (* @ [Sonata.CALL] *)
    in
    Sonata.Fn (arg, fun_body)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->
    let c1' = c1 @ cmds in
    let c2' = c2 @ cmds in
    [Sonata.JTR (trans' c1', trans' c2')]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: [] -> [Sonata.CALL]
  | Sm5.CALL :: cmds ->
    let fun_block = "f!" in
    (* 1. bind return function *)
    Sonata.PUSH (Sonata.Fn ("x#", trans' cmds))
    :: Sonata.BIND continue
    (* 2. remove l :: v :: (f, x, e) *)
    :: Sonata.BIND common_loc
    :: Sonata.PUSH (Sonata.Id common_loc)
    :: Sonata.STORE
    :: Sonata.BIND fun_block
    (* 3. push return function *)
    :: Sonata.PUSH (Sonata.Id continue)
    (* 4. restore l :: v :: (f, x, e) *)
    :: Sonata.PUSH (Sonata.Id fun_block)
    :: Sonata.PUSH (Sonata.Id common_loc)
    :: Sonata.LOAD
    :: Sonata.PUSH (Sonata.Id common_loc)
    (* 5. UNBIND POP : continue, common_loc, fun_block 3times *)
    :: Sonata.UNBIND
    :: Sonata.POP
    :: Sonata.UNBIND
    :: Sonata.POP
    :: Sonata.UNBIND
    :: Sonata.POP
    :: Sonata.CALL
    :: []
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

let trans : Sm5.command -> Sonata.command = fun command ->
  trans' ([Sm5.MALLOC; Sm5.BIND common_loc] @ command)
