let bottom = 248345
let top = 746315

let rec n_to_list n = 
  if n == 0 then []
  else
  let h = n mod 10 in
  let t = n / 10 in 
  h::(n_to_list t)

let all_combinations = 
  (n_to_list bottom) :: List.init (top - bottom) (fun x -> n_to_list (x + bottom + 1))

type group = G of int * int;;

let rec get_groups acc : int list -> group list = function
  | [] -> acc
  | n::t -> match acc with
            | [] -> let acc' = [G (n, 1)] in get_groups acc' t
            | (G (n', m))::t' -> if n < n' 
                               then get_groups ((G (n, 1))::acc) t 
                               else get_groups ((G (n, m + 1))::t') t;;

let is_correct_group (G (_, m)) = m > 1

let is_valid_groups groups =
  let group_size (G (_, m)) = m in
  let group_sizes = List.map group_size groups in
  match group_sizes with
  | [6] | [5] | [4] | [3] | [3;3] -> false
  | _ -> true

let check_groups n =
  let groups = get_groups [] n |> List.filter is_correct_group in
  is_valid_groups groups

let check n = 
  let n' = List.mapi (fun idx elt -> (idx, elt)) n in
  let p = List.exists (fun (idx, elt) -> 
    match List.nth_opt n' (idx + 1) with
    | Some (_, elt') -> elt == elt'
    | None -> false
  ) n' in
  let q = List.for_all (fun (idx, elt) ->
    match List.nth_opt n' (idx + 1) with
    | Some (_, elt') -> elt >= elt'
    | None -> true
  ) n' in
  p && q;;

let () =
  all_combinations
  |> List.filter check
  |> List.filter check_groups
  |> List.length 
  |> print_int;
  print_newline ()
