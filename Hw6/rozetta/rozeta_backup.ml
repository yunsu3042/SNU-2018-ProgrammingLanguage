(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 *)

 let rec addBack (origin : Sonata.cmd list) (to_add : Sonata.cmd list) =
  match origin with
  | x :: tl -> x :: addBack tl to_add
  | [] -> to_add

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
    (* let contId = Sonata.Id "K#" in *)
    let continue = Sonata.MALLOC :: Sonata.BIND "x#" :: Sonata.PUSH (Sonata.Id "x#") :: Sonata.STORE
    :: Sonata.BIND "f#" :: Sonata.PUSH (Sonata.Id "x#") :: Sonata.LOAD
    :: Sonata.PUSH (Sonata.Id "f#") :: Sonata.UNBIND :: Sonata.UNBIND :: Sonata.POP :: Sonata.POP
    :: Sonata.LOAD :: Sonata.PUSH (Sonata.Val (Sonata.Unit))
    :: Sonata.MALLOC :: Sonata.CALL :: [] in
    let sonata_cmd = trans' command in
    let func_cmd = addBack sonata_cmd continue in
    Sonata.Fn (arg, func_cmd)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds -> Sonata.JTR ((trans' c1), (trans' c2)) :: (trans' cmds)
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds ->
    let contName = "K#" in
    let condId = Sonata.Id contName in
    Sonata.PUSH (Sonata.Fn ("x#", trans' cmds))
    :: Sonata.MALLOC
    :: Sonata.BIND contName
    :: Sonata.PUSH condId
    :: Sonata.STORE
    :: Sonata.BIND "tmp1!!"
    :: Sonata.PUSH (Sonata.Id "tmp1!!")
    :: Sonata.STORE
    :: Sonata.BIND "tmp2!!"
    :: Sonata.BIND "fun_trick!!"
    :: Sonata.PUSH condId
    :: Sonata.PUSH (Sonata.Id "fun_trick!!")
    :: Sonata.PUSH (Sonata.Id "tmp2!!")
    :: Sonata.PUSH (Sonata.Id "tmp1!!")
    :: Sonata.LOAD
    :: Sonata.PUSH (Sonata.Id "tmp1!!")
    :: Sonata.CALL
    :: []
    (* failwith "TODO : fill in here" *)

  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
  trans' command
