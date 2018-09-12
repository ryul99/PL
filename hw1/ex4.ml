type expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr

let rec eval : formula -> bool = fun f ->
(  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f -> (not (eval f))
  | ANDALSO (a, b) -> ((eval a) && (eval b))
  | ORELSE (a, b) -> ((eval a) || (eval b))
  | IMPLY (a, b) -> ( if (eval a) then (eval b) else true)
  | LESS (a, b) ->
    (
      let rec cal : expr -> int = fun c ->
        (match c with
        | NUM d -> d
        | PLUS (p, q) -> (cal p) + (cal q)
        | MINUS (p, q) -> (cal p) - (cal q))
      in ( if (cal a) - (cal b) < 0 then true else false)
    )
)
