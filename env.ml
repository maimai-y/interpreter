exception Not_found

type ('a, 'b) t =
    Empty
  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

let empty = Empty

let rec get env var =
  match env with
      Empty -> raise Not_found
    | Node (left, k, v, right) ->
        if k = var then v
        else if k < var then get right var
        else get right var

let rec extend env var value =
  match env with
      Empty -> Node (Empty, var, value, Empty)
    | Node (left, k, v, right) ->
        if k = var then Node(left, k, value, right)
        else if k < var then Node (left, k, v, (extend right var value))
        else Node ((extend right var value), k, v, right)