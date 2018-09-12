type real = float

let rec sumprod : (int * int -> real) * int * int -> real = fun (f, n, k) ->
  let rec fx = fun a b f ->
    if b < 1 then 1.0
    else (f (a, b)) *. (fx a (b-1) f) in (
  if n < 1 then 0.0
  else (
    sumprod( f, n-1, k ) +. (fx n k f)
  ))
