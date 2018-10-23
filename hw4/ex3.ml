type require = id * (cond list)
and cond
= Items of gift list (* 선물들 *)
| Same of id (* 어느 조카의 선물들 *)
| Common of cond * cond (* 두조건에 공통된 선물들 *)
| Except of cond * gift list (* 조건에서 어느 선물들은 빼고 *)
and gift = int (* 선물 번호 *)
and id = A | B | C | D | E (* 조카 이름 *)

let AL: gift list = []
let BL: gift list = []
let CL: gift list = []
let DL: gift list = []
let EL: gift list = []

let allL: (id * (gift list)) list = ((A,[]), (B,[]), (C,[]), (D,[]), (E,[]))

let rec com: gift list -> gift list -> gift list -> gift list = fun re a b -> (
  match a with
  | [] -> re
  | _ -> (
    match b with
    | [] -> re
    | _ -> (
      if (List.hd a = List.hd b) then (com (re::(List.hd a)::[]) (List.tl a) (List.tl b)) else (
        if (List.hd a > List.hd b) then (com re a (List.tl b)) else (com re (List.tl a) b)
      )
    )
  )

)

let rec eval: cond -> gift list = fun c -> (
  match cond with
  | Items gl -> gl
  | Same i -> List.assoc i
  | Common (a, b) -> (
    let al: gift list = List.sort List.compare (eval a) in
    let bl: gift list = List.sort List.compare (eval b) in
    com al bl
  )
  | Except (a, b) -> (
    let al: gift list = eval a in
    let re: gift list = [] in
    let fx: gift list -> gift -> gift list = fun ret aa -> if(not (List.mem aa b)) then (ret::aa::[]) else (ret) in
    let rrr = List.fold_left fx re al in
    rrr
  )
)


let shoppingList: require list -> (id * gift list) list = fun req -> (
  let 
)
