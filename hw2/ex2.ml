type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff : ae * string -> ae = fun (a, b) ->
(
  match a with
  | CONST _ -> CONST 0
  | VAR i -> if (i = b) then (CONST 1) else (CONST 0)
  | POWER (p, q) -> if (q = 1) then (CONST 1) else (TIMES [(CONST q); POWER (p, (q-1))])
  | TIMES i -> if (List.length i = 0) then (raise InvalidArgument) else
  (
    if ((diff ((List.hd i), b)) != CONST 0) then (TIMES ((diff ((List.hd i), b))::(List.tl i))) else
    (
      if ((List.length (List.tl i)) = 0) then (CONST 0) else
        (
          TIMES ((List.hd i)::(diff ((TIMES (List.tl i)), b))::[])
        )
    )
  )
  | SUM i -> if (List.length i = 0) then (raise InvalidArgument) else
  (
    if ((List.length (List.tl i)) = 0) then (diff ((List.hd i), b)) else
      (
        SUM ((diff ((List.hd i), b))::(diff ((SUM (List.tl i)), b))::[])
      )
  )
)
