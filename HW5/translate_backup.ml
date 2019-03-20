(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP] (* unbind pop 을 해주지 않으면 상위에 있는 expression에 영향을 미칠 수 있|  *)
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> []
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.NOT (e) -> trans e @ [Sm5.NOT]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE]
    | K.SEQ (e1, e2) -> trans e1 @ trans e2
    | K.IF (e1, e2, e3) -> trans e1 @ [Sm5.JTR(trans e2, trans e3)]
    | K.WRITE (e) -> trans e @ [Sm5.PUT]
    | K.LETF (f, x, body_e, next_e) ->
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans body_e)); Sm5.BIND f]
      @ trans next_e @ [Sm5.UNBIND; Sm5.POP]
    | K.CALLV (f, e) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)]
      @ trans e @
      [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, x) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f);
      Sm5.PUSH (Sm5.Id x) ; Sm5.LOAD; Sm5.PUSH (Sm5.Id x) ;Sm5.CALL]
    | K.WHILE (cond_e, body_e) ->
      let x = "x#" in
      let f = "while!" in
      let call = K.CALLR(f, x) in
      let loop = K.SEQ(body_e, call) in
      let fun_body = K.IF(cond_e, loop, K.UNIT) in
      let next_e = K.CALLR (f, x) in
      let def_fun = (K.LETF (f, x, fun_body, next_e)) in
      let my_while = (K.LETV (x, K.NUM 1, def_fun)) in
      trans my_while
    | K.FOR (i, init_e, bound_e, body_e) ->
      let f = "for!" in
      let init = K.ASSIGN(i, init_e) in
      let cond = K.LESS (K.VAR i, K.ADD(bound_e, K.NUM 1)) in
      let add = K.ASSIGN(i, K.ADD(K.VAR i, K.NUM 1)) in
      let call = K.CALLR(f, i) in
      let body = K.SEQ(body_e, add) in
      let loop = K.SEQ(body, call) in
      let fun_body = K.IF(cond, loop, K.UNIT) in
      let next_e = K.CALLR (f, i) in
      let def_fun = K.LETF (f, i, fun_body, next_e) in
      let my_for = K.SEQ(init, def_fun) in
      trans my_for





    (* | K.FOR (i, init_e, bound_e, add_e) ->  *)

    (* | K.WHILE (cond_e, body_e) -> *)

    (* | K.WHILE (e1, e2) ->
    | WHILE of exp * exp          (* while loop *)
    | FOR of id * exp * exp * exp (* for loop *)
    | READ of id
    | WRITE of exp *)

    | _ -> failwith "Unimplemented"
end
