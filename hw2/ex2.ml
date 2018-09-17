type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec caltidiff : (ae list) * int * (ae list) * string -> ae = fun (i, n, h ,x) ->
  (
    if (n = 0) then (TIMES (h@((diff ((List.hd i), x))::[])@(List.tl i))) else
    (
      caltidiff ((List.tl i), (n - 1), (h@((List.hd i)::[])), x)
    )
  )

and tidiff : (ae list) * int * string * (ae list) -> ae = fun (a, n, x, con) ->
(
  if ((List.length a - 1)= n) then SUM (con@((caltidiff (a, n, [], x))::[])) else
  (
    tidiff (a, (n + 1), x, (con@((caltidiff (a, n, [], x))::[])))
  )
)

and diff : ae * string -> ae = fun (a, b) ->
(
  match a with
  | CONST _ -> CONST 0
  | VAR i -> if (i = b) then (CONST 1) else (CONST 0)
  | POWER (p, q) -> TIMES [(CONST q); POWER (p, (q-1)); (diff (VAR p, b))]
  | TIMES i -> if (List.length i = 0) then (raise InvalidArgument) else
  (
    tidiff (i, 0, b, [])
  )
  | SUM i -> if (List.length i = 0) then (raise InvalidArgument) else
  (
    if ((List.length (List.tl i)) = 0) then (diff ((List.hd i), b)) else
    (
      SUM ((diff ((List.hd i), b))::(diff ((SUM (List.tl i)), b))::[])
    )
  )
)
