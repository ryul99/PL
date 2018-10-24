type require = id * (cond list)
and cond
= Items of gift list (* 선물들 *)
| Same of id (* 어느 조카의 선물들 *)
| Common of cond * cond (* 두조건에 공통된 선물들 *)
| Except of cond * gift list (* 조건에서 어느 선물들은 빼고 *)
and gift = int (* 선물 번호 *)
and id = A | B | C | D | E (* 조카 이름 *)

let allL = Hashtbl.create 10 in
let cAll = Hashtbl.create 10 in
let _ = Hashtbl.add allL A [] in
let _ = Hashtbl.add allL B [] in
let _ = Hashtbl.add allL C [] in
let _ = Hashtbl.add allL D [] in
let _ = Hashtbl.add allL E [] in
let _ = Hashtbl.add cAll A 0 in
let _ = Hashtbl.add cAll B 0 in
let _ = Hashtbl.add cAll C 0 in
let _ = Hashtbl.add cAll D 0 in
let _ = Hashtbl.add cAll E 0 in

(* let allL: (id * (ref gift list)) list = ((A,[]), (B,[]), (C,[]), (D,[]), (E,[])) *)

let conc: gift list -> gift list -> gift list = fun a b -> (
  let fx: gift list -> gift -> gift list = fun l x -> (if (List.mem x l) then (l) else (l @ x::[])) in
  List.sort compare (List.fold_left fx a b)
) in

let rec com: gift list -> gift list -> gift list -> gift list = fun re a b -> (
  match a with
  | [] -> re
  | _ -> (
    match b with
    | [] -> re
    | _ -> (
      if (List.hd a = List.hd b) then (com (conc re ((List.hd a)::[])) (List.tl a) (List.tl b)) else (
        if (List.hd a > List.hd b) then (com re a (List.tl b)) else (com re (List.tl a) b)
      )
    )
  )
) in

let rec eval: cond -> gift list = fun c -> (
  match c with
  | Items gl -> gl
  | Same i -> Hashtbl.find allL i
  | Common (a, b) -> (
    let al: gift list = List.sort compare (eval a) in
    let bl: gift list = List.sort compare (eval b) in
    com [] al bl
  )
  | Except (a, b) -> (
    let al: gift list = eval a in
    let re: gift list = [] in
    let fx: gift list -> gift -> gift list = fun ret aa -> if(not (List.mem aa b)) then (conc ret (aa::[])) else (ret) in
    let rrr = List.fold_left fx re al in
    (List.sort compare rrr)
  )
) in

let rec evalR: cond list -> gift list -> gift list = fun conL re -> (
  if(List.length conL = 0) then (re) else (
    evalR (List.tl conL) (conc re (eval (List.hd conL)))
  )
) in

let rec shopL: require list -> unit = fun req -> (
  let reD = ref 0 in

  let shp: require -> unit = fun reqqq -> (
    let _ = Hashtbl.replace allL (fst reqqq) (evalR (snd reqqq) []) in
    let _ = reD := if (Hashtbl.find cAll (fst reqqq) = (List.length (Hashtbl.find allL (fst reqqq)))) then (!reD) else (1) in
    let _ = Hashtbl.replace cAll (fst reqqq) (List.length (Hashtbl.find allL (fst reqqq))) in
    ()
  ) in

  let _ = List.iter shp req in
  let r = if(!reD = 0) then () else (shopL req) in
  r
) in

let shoppingList: require list -> (id * gift list) list = fun req -> (
  let _ = shopL req in
  (A, Hashtbl.find allL A)::(B, Hashtbl.find allL B)::(C, Hashtbl.find allL C)::(D, Hashtbl.find allL D)::(E, Hashtbl.find allL E)::[]
)
