(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
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

  let rec eval mem env e =
    match e with
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
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x -> (Mem.load mem (lookup_env_loc env x), mem)
    | ADD (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 + value_int n2), mem'')
    | SUB (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 - value_int n2), mem'')
    | MUL (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 * value_int n2), mem'')
    | DIV (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 / value_int n2), mem'')
    | EQUAL (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (
        match n1 with
        | Unit -> (
          match n2 with
          | Unit -> (Bool true, mem'')
          | _ -> (Bool false, mem'')
          )
        | Num _ -> (
          match n2 with
          | Num _ -> (Bool (value_int n1 = value_int n2), mem'')
          | _ -> (Bool false, mem'')
          )
        | Bool _ -> (
          match n2 with
          | Bool _ -> (Bool (value_bool n1 = value_bool n2), mem'')
          | _ -> (Bool false, mem'')
          )
        | _ -> (Bool false, mem'')
      )
    | LESS (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Bool (value_int n1 < value_int n2), mem'')
    | NOT e ->
      let (b, mem') = eval mem env e in
      (Bool (not (value_bool b)), mem')
    | SEQ (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (n2, mem'')
    | IF (e, e1, e2) ->
      let (v1, mem') = (eval mem env e) in
      (match value_bool v1 with
      | true ->
        let (v, mem'') = eval mem' env e1 in
        (v, mem'')
      | false ->
        let (v, mem'') = eval mem' env e2 in
        (v, mem''))
    | WHILE (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      (match value_bool v1 with
      | true ->
        let (v1, mem1) = eval mem' env e2 in
        let (v2, mem2) = eval mem1 env (WHILE (e1, e2)) in
        (v2, mem2)
      | false ->
        (Unit, mem'))
    | LETF (id, idl, e1, e2) ->
      let env' = Env.bind env id (Proc (idl, e1, env)) in
      let (v, mem') = eval mem env' e2 in
      (v, mem')
    | CALLV (f, exl) ->
      (
        let (xl, ex', env') = lookup_env_proc env f in
        let fx a b =
        (
          let (p, q) = eval (snd a) env b in
          ((fst a) @ p::[], q)
        ) in
        let (vl, memn) = List.fold_left fx ([], mem) exl in
        let fy c' a' b' =
        (
          let (loc, memn') = Mem.alloc (fst c') in
           ((Mem.store memn' loc b'), (Env.bind (snd c') a' (Addr loc)))
        ) in
        if (List.length xl != List.length vl) then (raise (Error "InvalidArg")) else
        (let (mem', env'') = List.fold_left2 fy (memn, env') xl vl in
        let env''' = Env.bind env'' f (Proc (xl, ex', env')) in
        let (v', mem') = eval mem' env''' ex' in
        (v', mem'))
      )
    | CALLR (f, yl) ->
      (
        let (xl, ex, env') = lookup_env_proc env f in
        let fx a xb yc = (Env.bind a xb (Addr (lookup_env_loc env yc))) in
        if (List.length xl != List.length yl) then (raise (Error "InvalidArg")) else
        (let env'' = List.fold_left2 fx env' xl yl in
        let env''' = Env.bind env'' f (Proc (xl, ex, env')) in
        let (v, mem') = eval mem env''' ex in
        (v, mem'))
      )
    | RECORD l ->
      (
        match List.length l with
        | 0 -> (Unit, mem)
        | _ ->
        (
          let fx a b =
          (
            let (p, q) = eval (snd a) env (snd b) in
            ((fst a) @ ((fst b), p)::[], q)
          ) in
          let eE = fun x -> raise (Error "Unbound") in
          let (xvl, memn) = List.fold_left fx ([], mem) l in
          let bind ff id loc = (fun x -> if x = id then loc else ff x) in
          let fy a b = (* (ff, mem) (x, v) *)
          (
            let (lc, memn') = Mem.alloc (snd a) in
            ((bind (fst a) (fst b) lc), (Mem.store memn' lc (snd b)))
          ) in
          let (finalE, finalM) = List.fold_left fy (eE, memn) xvl in
          (Record finalE, finalM)
          (* (
            match finalE with
            | Env.E e -> (Record e, finalM)
          ) *)
        )
      )
    | FIELD (ex, id) ->
    (
      let (r, mem') = eval mem env ex in
      ((Mem.load mem' ((value_record r) id)), mem')
    )
    | ASSIGNF (ex1, id, ex2) ->
    (
      let (r, mem1) = eval mem env ex1 in
      let (v, mem2) = eval mem1 env ex2 in
      (v, (Mem.store mem2 ((value_record r) id) v))
    )
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end
