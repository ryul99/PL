type exp = X
          | INT of int
          | REAL of float
          | ADD of exp * exp
          | SUB of exp * exp
          | MUL of exp * exp
          | DIV of exp * exp
          | SIGMA of exp * exp * exp
          | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec cal : exp -> float -> float = fun x p ->
(
  match x with
  | X -> p
  | INT d -> float_of_int d
  | REAL i -> i
  | ADD (a, b) -> (cal a p) +. (cal b p)
  | SUB (a, b) -> (cal a p) -. (cal b p)
  | MUL (a, b) -> (cal a p) *. (cal b p)
  | DIV (a, b) -> (cal a p) /. (cal b p)
  | SIGMA (a, b, c) -> calculate x
  | INTEGRAL (a, b, c) -> calculate x
)
and calculate: exp -> float = fun x ->
  (
    match x with
    | X -> raise FreeVariable
    | INT d -> float_of_int d
    | REAL i -> i
    | ADD (a, b) -> (calculate a) +. (calculate b)
    | SUB (a, b) -> (calculate a) -. (calculate b)
    | MUL (a, b) -> (calculate a) *. (calculate b)
    | DIV (a, b) -> (calculate a) /. (calculate b)
    | SIGMA (a, b, c) ->
    (
      let rec sgm : exp -> int -> int -> float -> float = fun expre r s re->
      (
        if(r > s) then re else
        (
          (sgm expre (r + 1) s (re +. (cal expre (float_of_int r))))
        )
      ) in (sgm c (int_of_float (calculate a)) (int_of_float (calculate b)) 0.0)
    )
    | INTEGRAL (a, b, c) ->
    (
      let rec inte : exp -> float -> float -> float -> float = fun expre r s re ->
      (
        if (r > s) then (-1.0 *. (inte expre s r re)) else
        (
          if ((s -. r) < 0.1) then re else
          (
            (inte expre (r +. 0.1) s (re +. (0.1 *. (cal expre r))))
          )
        )
      ) in (inte c (calculate a) (calculate b) 0.0)
    )
  )
