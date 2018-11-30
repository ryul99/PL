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

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

let typ_env : (M.id * M.types) list = ref []

(* TODO : Implement this function *)
let rec check : M.exp -> M.types = fun exp ->
  match exp with
  | CONST c -> (
    match c with
    | S _ -> M.TyString
    | N _ -> M.TyInt
    | B _ -> M.TyBool
  )
  | VAR id -> raise (M.TypeError "Type Checker Unimplemented")
  | FN (id, e) -> raise (M.TypeError "Type Checker Unimplemented")
  | APP (e1, e2) -> raise (M.TypeError "Type Checker Unimplemented")
  | LET (decl, e) -> raise (M.TypeError "Type Checker Unimplemented")
  | IF (e1, e2, e3) -> (
    let aa = check e1 in
    (
      match aa with
      | TyBool -> if (check e2 = check e3) then (check e2) else (raise (M.TypeError "type is not same"))
      | _ -> raise (M.TypeError "not a Bool")
    )
  )
  | BOP (bop, e1, e2) -> raise (M.TypeError "Type Checker Unimplemented")
  | READ -> M.TyInt
  | WRITE exp -> (
    let et = check exp in
    match et with
    | TyString -> M.TyString
    | TyInt -> M.TyInt
    | TyBool -> M.TyBool
    | _ -> raise (M.TypeError "cant write")
  )
  | MALLOC e -> M.TyLoc (check e)
  | ASSIGN (e1, e2) -> raise (M.TypeError "Type Checker Unimplemented")
  | BANG e -> raise (M.TypeError "Type Checker Unimplemented")
  | SEQ (e1, e2) -> raise (M.TypeError "Type Checker Unimplemented")
  | PAIR (e1, e2) -> M.TyPair (check e1, check e2)
  | FST e -> (
    match e with
    | PAIR (ee, _) -> check ee
    | _ -> raise (M.TypeError "Type Checker Unimplemented")
  )
  | SND e -> (
    match e with
    | PAIR (_, ee) -> check ee
    | _ -> raise (M.TypeError "Type Checker Unimplemented")
  )
  | _ -> raise (M.TypeError "Type Checker Unimplemented")
