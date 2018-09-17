type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
  | EMPTY -> -1
  | NODE(r,_,_,_) -> r

let shake (x,lh,rh) =
  if (rank lh) >= (rank rh)
  then NODE(rank rh+1, x, lh, rh)
  else NODE(rank lh+1, x, rh, lh)

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let rec merge : heap * heap -> heap = fun (a, b) ->
(
  match a with
  | EMPTY ->
  (
    match b with
    | EMPTY -> EMPTY
    | NODE _ -> b
  )
  | NODE (_, _, la, ra) ->
  (
    match b with
    | EMPTY -> a
    | NODE (_, _, lb, rb) ->
    (
      if ((findMin a) > (findMin b)) then
      (
        if ((rank a) > (rank lb)) then
        (
          shake ((findMin b), a, (merge (lb, rb)))
        )
        else
        (
          shake ((findMin b), lb, (merge (a, rb)))
        )
      )
      else
      (
        if ((rank b) > (rank la)) then
        (
          shake ((findMin a), b, (merge (la, ra)))
        )
        else
        (
          shake ((findMin a), la, (merge (b, ra)))
        )
      )
    )
  )
)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge (lh,rh)
