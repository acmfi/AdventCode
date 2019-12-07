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
  p && q

let () =
  all_combinations
  |> List.filter check
  |> List.length 
  |> print_int;
  print_newline ()
