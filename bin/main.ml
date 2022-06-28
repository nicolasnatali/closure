let rec find_match (a, b) pairs all_pairs =
  match pairs with
  | [] -> None
  | (b', c) :: pairs -> (
      match b = b' && not (List.mem (a, c) all_pairs) with
      | true -> Some c
      | false -> find_match (a, b) pairs all_pairs)

let rec find_new_pair pairs all_pairs =
  match pairs with
  | [] -> None
  | (a, b) :: pairs -> (
      match find_match (a, b) all_pairs all_pairs with
      | Some c -> Some (a, c)
      | None -> find_new_pair pairs all_pairs)

let rec closure pairs =
  match find_new_pair pairs pairs with
  | Some pair -> closure (pair :: pairs)
  | None -> pairs
;;

closure [ ('A', 'B'); ('B', 'C'); ('C', 'D') ]
