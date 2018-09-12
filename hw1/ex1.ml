let sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
  let rec recsig = fun p q r fx ->
  (  if p > q then r
    else recsig (p + 1) q (r + (fx p)) fx) in
  recsig a b 0 f
