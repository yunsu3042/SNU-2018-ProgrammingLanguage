(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0

let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

let emptyTnv = (fun x -> raise (M.TypeError ("unbound id: " ^ x)))


type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
type id = string


let rec printType str ty =
  match ty with
  | TInt -> print_endline(str ^ " : TInt")
  | TBool -> print_endline(str ^ " : TBool")
  | TString -> print_endline(str ^ " : Tstring")
  | TPair (t1, t2) -> print_endline(str ^ " : Tpair")
  | TLoc t -> print_endline(str ^ " : TLoc")
  | TFun (t1, t2) -> print_endline(str ^ " : TFun")
  | TVar v -> print_endline(str ^ " : TVar")
  (* Modify, or add more if needed *)

(*
  type exp =
           | VAR of id
           | FN of id * exp
           | APP of exp * exp
           | LET of decl * exp

  and const = S of string | N of int | B of bool
  and id = string
  and decl =
    | REC of id * id * exp  (* Recursive function decl. (fun_id, arg_id, body) *)
    | VAL of id * exp       (* Value decl, including non-recursive functions *)
  and bop = ADD | SUB | EQ | AND | OR

  (* domains *)
  type loc = int
  type value = Int of int
             | String of string
             | Bool of bool
             | Loc of loc
             | Pair of value * value
             | Closure of closure
  and closure = fexpr * env
  and fexpr = Fun of id * exp
            | RecFun of id * id * exp
  and env = id -> value
  type memory = int * (loc -> value)

  type typ =
    | TInt
    | TBool
    | TString
    | TPair of typ * typ
    | TLoc of typ
    | TFun of typ * typ
    | TVar of var
    (* Modify, or add more if needed *)

  type types = TyInt                     (* integer type *)
             | TyBool                    (* boolean type *)
             | TyString                  (* string type *)
             | TyPair of types * types   (* pair type *)
             | TyLoc of types            (* location type *)
             | TyArrow of types * types  (* function type *) *)

type tnv = id -> typ
let bind tnv (x, t) = (fun y -> if y = x then t else tnv y)
let getLoc = function
  | TLoc t -> t
  | _ -> raise (M.TypeError "getLoc type error")

let getPair = function
  | TPair (t1, t2) -> (t1, t2)
  | _ -> raise (M.TypeError "getPair type error")

let getArrow = function
  | TFun (t1, t2) -> (t1, t2)
  | _ -> raise (M.TypeError "getArrow type error")

let rec scheck exp tnv =
  match exp with
  | M.CONST (S s) -> (TString, tnv)
  | M.CONST (N n) -> (TInt, tnv)
  | M.CONST (B b) -> (TBool, tnv)
  | M.IF (c, e1, e2) ->
    let (c, _) = scheck c tnv in
    let (t1, _) = scheck e1 tnv in
    let (t2, _) = scheck e2 tnv in
    if (t1 <> t2)
    then raise (M.TypeError "If body different")
    else
    (
      match c with
      | TBool -> (t1, tnv)
      | _ -> raise (M.TypeError "If condition error" )
      )
  | M.READ -> (TInt, tnv)
  | M.WRITE e ->
    let (t, tnv') = scheck e tnv in
    (
      match t with
      | TInt -> (TInt, tnv')
      | TString -> (TString, tnv')
      | TBool -> (TBool, tnv')
      | _ -> raise (M.TypeError "Write error" )
      )
  | M.MALLOC e ->
    let (t, tnv') = scheck e tnv in
    (TLoc t, tnv')
  | M.ASSIGN (e1, e2) ->
    let (tloc, tnv') = scheck e1 tnv in
    let (t2, tnv'') = scheck e2 tnv' in
    let t1 = getLoc tloc in
    if t1 = t2 then (t1, tnv'') else raise (M.TypeError "Assign Error")
  | M.BANG e ->
    let (tloc, tnv') = scheck e tnv in
    let t = getLoc tloc in
    (t, tnv)
  | M.SEQ (e1, e2) ->
    let (t1, tnv') = scheck e1 tnv in
    let (t2, tnv'') = scheck e2 tnv' in
    (t2, tnv'')

  | M.PAIR (e1, e2) ->
    let (t1, _) = scheck e1 tnv in
    let (t2, _) = scheck e2 tnv in
    ((TPair (t1, t2)), tnv)
  | M.FST e ->
    let (t, tnv') = scheck e tnv in
    let (t1, t2) = getPair t in
    (t1, tnv)
  | M.SND e ->
    let (t, tnv') = scheck e tnv in
    let (t1, t2) = getPair t in
    (t2, tnv)
  | M.VAR id ->
    let t = tnv id  in
    (t, tnv)
  | M.FN (id, e) ->(
    try
      let tnv' = bind tnv (id, TInt) in
      let (t, tnv'') = scheck e tnv' in
      let t' = TFun (TInt, t) in
      (t', tnv'')
    (* with TypeError str -> raise (TypeError "FN type error") *)
    with M.TypeError str ->
      (
        try
          let tnv' = bind tnv (id, TString) in
          let (t, tnv'') = scheck e tnv' in
          let t' = TFun (TString, t) in
          (t', tnv'')
        with M.TypeError str ->
          (
            try
              let tnv' = bind tnv (id, TBool) in
              let (t, tnv'') = scheck e tnv' in
              let t' = TFun (TBool, t) in
              (t', tnv'')
            with M.TypeError str -> raise (M.TypeError "FN type error")
            )
        )
      )

  | M.APP (e1, e2) ->
    let (arrow, _) = scheck e1 tnv in
    let (t2', _) = scheck e2 tnv in
    let (t1, t2) = getArrow arrow in
    if t2 = t2' then (t2, tnv) else raise (M.TypeError "App type error")

  (* | LET (REC(f, x, e), e1) ->
    let tnv' = bind tnv (f, Closure(RecFun(f, x, e), tnv)) in  *)

  | M.LET (VAL(x, e), e1)->
    let (t1, tnv') = scheck e tnv in
    let tnv'' = bind tnv' (x, t1) in
    let (t2, tnv''') = scheck e1 tnv'' in
    (t2, tnv''')

  | M.BOP (op, e1, e2) ->
    let (t1, tnv') = scheck e1 tnv in
    (* let _ = printType "t1" t1 in *)
    let (t2, tnv'') = scheck e2 tnv' in
    (* let _ = printType "t2" t2 in *)
    if (t1 <> t2)
    then raise (M.TypeError "BOP type error")
    else
    (
      match op with
      | ADD -> if (t1 = TInt) then (TInt, tnv'') else raise (M.TypeError "ADD type error")
      | SUB -> if (t1 = TInt) then (TInt, tnv'') else raise (M.TypeError "SUB type error")
      | AND -> if (t1 = TBool) then (TBool, tnv'') else raise (M.TypeError "AND type error")
      | OR -> if (t1 = TBool) then (TBool, tnv'') else raise (M.TypeError "OR type error")
      | EQ ->
        (
          match t1 with
          | TInt -> (TBool, tnv'')
          | TString -> (TBool, tnv'')
          | TBool -> (TBool, tnv'')
          | TLoc t -> (TBool, tnv'')
          | _ -> raise (M.TypeError "BOP type error")
          )
      )

let rec transType s_type =
  match s_type with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair(transType t1, transType t2)
  | TLoc (t) -> M.TyLoc (transType t)
  | TFun (t1, t2) -> M.TyArrow(transType t1, transType t2)
  | _ -> raise (M.TypeError "unhandled type")



(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let (result, _) = scheck exp emptyTnv in
  transType result
