(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

(*
 * SNU 4190.310 Programming Languages
 * Homework "Continuation Passing Style" Skeleton
 *)

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
  | If (e1, e2, e3) ->
    If (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Equal (e1, e2) -> Equal (alpha_conv e1 subst, alpha_conv e2 subst)
  | Raise e -> Raise (alpha_conv e subst)
  | Handle (e1, n , e2) -> Handle (alpha_conv e1 subst, n, alpha_conv e2 subst)
(* TODO : Complete this function *)

let curry = fun k h exp -> Fn (k, Fn(h, exp))
let app2 = fun f a b -> App(App(f, a),b)

let rec cps' exp =
  let k = new_name () in
  let h = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, Fn (h, App (Var k, Num n)))
  | Var x -> Fn (k, Fn (h, App (Var k, Var x)))
  | Fn (x, e) -> Fn (k, Fn (h, App (Var k, Fn(x, cps' e))))
  (* Non constant expressions *)
  | App (e1, e2) ->
    let f = new_name () in
    let v = new_name () in
    curry k h (app2 (cps' e1) (Fn(f, app2 (cps' e2) (Fn(v, app2 (App(Var f, Var v)) (Var k) (Var h))) (Var h))) (Var h))
  | If (e1, e2, e3) ->
    let v = new_name () in
    curry k h (app2 (cps' e1) (Fn(v, (If((Var v), (app2 (cps' e2) (Var k) (Var h)), (app2 (cps' e3) (Var k) (Var h)))))) (Var h))
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    curry k h (app2 (cps' e1) (Fn(v1, app2 (cps' e2) (Fn(v2, App(Var k, Equal((Var v1), (Var v2))))) (Var h))) (Var h))
  | Raise e ->
    curry k h (app2 (cps' e) (Var h) (Var h))
  | Handle (e1, n , e2) ->
    let v = new_name () in
    let x = new_name () in
    curry k h (app2 (cps' e1) (Var k) (Fn(v, If(Equal(Var v, Num n), (app2 (cps' e2) (Var k) (Var h)), App(Var h, Var v)))))

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  let x = new_name () in
  app2 (cps' (alpha_conv e [])) (Fn(x, Var x)) (Fn(x, Num 201812))
