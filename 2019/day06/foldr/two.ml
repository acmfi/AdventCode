module Nodes = Set.Make(String)

type graph = Nodes.t * (string * string) list

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let input = read_lines "input.txt"

let empty_graph = Nodes.empty, []

let rec build_graph = function
  | [] -> empty_graph
  | h::t ->
      let (nodes, edges) = build_graph t in
      match String.split_on_char ')' h with
      | [x;y] -> Nodes.of_list [x;y] |> Nodes.union nodes, (x, y)::edges
      | _ -> raise (Failure "Bad format")

let (in_nodes, in_edges) = build_graph input
let start = "YOU"
let goal = "SAN"

let fst (x,_) = x
let snd (_,y) = y

let node_eq node (_, y) = compare node y == 0
let node_neq node (_, y) = compare node y != 0

let orbit edges node = List.filter (node_eq node) edges |> List.map fst |> Nodes.of_list

let rec min acc : int list -> int = function
  | [] -> acc
  | h::t -> min (if h < acc then h else acc) t

let disjoint s1 s2 =
  Nodes.inter s1 s2 |> Nodes.equal Nodes.empty

let jumps edges node =
  List.concat [
    List.filter (node_eq node) edges |> List.map fst;
    List.filter (fun (x, _) -> compare x node == 0) edges |> List.map snd
  ]

let rec steps_to_santa edges node visited : int =
  if disjoint (orbit edges goal) (orbit edges node) 
  then 
    List.map (fun p -> 
      1 + (steps_to_santa edges p (Nodes.add node visited))
    ) (jumps edges node |> List.filter (fun x -> not (Nodes.mem x visited))) |> min (List.length edges + 1)
  else 0

let () =
  (steps_to_santa in_edges start Nodes.empty) - 2 |> print_int;
  print_newline ()

