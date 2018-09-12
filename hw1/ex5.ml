type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a, b) ->
  match a with
  | ZERO -> b
  | SUCC i -> natadd (i, SUCC b)

let rec natmul : nat * nat -> nat = fun (c, d) ->
  match c with
  | ZERO -> ZERO
  | SUCC i ->
    (match i with
    | ZERO -> d
    | i -> natadd (d, natmul (i, d)))
