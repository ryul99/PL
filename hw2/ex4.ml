module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = ((int list) list) * ((int list) list)
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ = fun (q, a) ->
    match q with
    | (qa, qb) -> (a::qa, qb)

    let deQ = fun i ->
    match i with
    | (qa, qb) ->
    (
      if ((List.length qb) = 0) then
      (
        if ((List.length qa) = 0) then (raise EMPTY_Q) else
        (
          let rq = (List.rev qa) in
          ((List.hd (rq)), ([], (List.tl rq)))
        )
      )
      else ((List.hd qb), (qa, (List.tl qb)))
    )
  end
