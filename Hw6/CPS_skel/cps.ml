(*
 * SNU 4190.310 Programming Languages
 * Homework "Continuation Passing Style" Skeleton
 *)

open M0

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst =
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | Rec (f, x, e) ->
    let x' = new_name () in
    let f' = new_name () in
    let subst' = (f, f') :: (x, x') :: subst in
    Rec (f', x', alpha_conv e subst')
  | Ifz (e1, e2, e3) ->
    Ifz (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Add (e1, e2) -> Add (alpha_conv e1 subst, alpha_conv e2 subst)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subst, alpha_conv e2 subst)
  | Fst e -> Fst (alpha_conv e subst)
  | Snd e -> Snd (alpha_conv e subst)

(* TODO : Complete this function *)
let rec cps' exp =
  let k = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, App (Var k, Num n))
  | Var x -> Fn (k, App (Var k, Var x))
  | Fn (x, e) -> Fn (k, App(Var k, Fn(x, e)))
  | Rec (f, x, e) -> Fn (k, App(Var k, Rec (f, x, e)))
  (* Non constant expressions *)
  | App (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      App (cps' e1,
        Fn (v1,
          App (cps' e2,
            Fn (v2,
              App (Var k, App (Var v1, Var v2))
              )
            )
          )
        )
      )
  | Ifz (e1, e2, e3) ->
    let v1 = new_name () in
    let v2 = new_name () in
    let v3 = new_name () in
    Fn(k, App( cps' e1, Fn(v1, App(cps' e2, Fn(v2, App(cps' e3, Fn(v3, App(Var k, Ifz(Var v1, Var v2, Var v3)))))))))
  | Add (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      App (cps' e1,
        Fn (v1,
          App (cps' e2,
            Fn (v2,
              App (Var k, Add (Var v1, Var v2))
              )
            )
          )
        )
      )
  | Pair (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      App (cps' e1,
        Fn (v1,
          App (cps' e2,
            Fn (v2,
              App (App(Var k, Var v1), Var v2)
              )
            )
          )
        )
      )

  | Fst e ->
  (* let a = new_name() in
  let b = new_name() in
  let a' = new_name() in
  let b' = new_name() in
  let f = new_name() in
  let g = new_name() in *)
  let v1 = new_name() in
  (* let test = Fn(a', App(App(Var a', Num 100), Num 200)) in
  let ltrue = Fn(a, Fn (b, Var b)) in *)
  (* Fn (k, App (cps' e, Fn(v1, App(Var k, App(ltrue, Var v1))))) *)
  (* Fn (k, App(Var k, Fst e)) *)
  (* Fn(k, App(Var k, App(ltrue, e))) *)
  Fn (k, App(cps' e, Fn(v1, App(Var k, (Fst (Var v1))))))

  | Snd e ->
    (* let a = new_name() in
    let b = new_name() in
    let a' = new_name() in
    let b' = new_name() in
    let f = new_name() in
    let g = new_name() in *)
    let v1 = new_name() in
    (* let v2 = new_name() in
    let lfalse = Fn(a, Fn (b, Var a)) in
    let test = Fn(a', App(App(Var a', Num 100), Num 200)) in *)
    (* Fn (k, App (test, Fn(v1, Fn(v2, App(Var k, App(App(lfalse, Var v1), Var v2)))))) *)
    (* Fn (k, App (cps' e, Fn (v1, Fn(v2, App (Var k, App(App(lfalse, Var v1), Var v2)))))) *)
    (* Fn (k, App (cps' e, Fn (v1, App (Var k, Var v1)))) *)
    (* Fn (k, App(Var k, Snd e)) *)
    Fn (k, App(cps' e, Fn(v1, App(Var k, (Snd (Var v1))))))

let cps exp = cps' (alpha_conv exp [])
