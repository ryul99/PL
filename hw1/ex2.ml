type real = float

let sumprod : (int * int -> real) * int * int -> real = fun (f, n, k) ->
  let rec prod = fun a b r fx ->
    (if b < 1 then r
    else prod a (b-1) ((fx (a, b)) *. r) fx) in (
    let rec recsumprod = fun p q r fy ->
        if p < 1 then r
        else recsumprod (p-1) q (r +. (prod p q 1.0 fy)) fy
    in recsumprod n k 0.0 f
  )
