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

let node_eq node (_, y) = compare node y == 0

let rec orbits_of_node edges node =
  let direct : (string * string) list = List.filter (node_eq node) edges in
  match direct with
  | _::_ -> List.concat [direct;List.map (fun (x, _) -> orbits_of_node edges x) direct |> List.concat]
  | [] -> []
  
let get_orbit_count (nodes, edges) =
  Nodes.fold (fun node acc ->
    let n = orbits_of_node edges node |> List.length in acc + n) nodes 0

let () =
  get_orbit_count (in_nodes, in_edges) |> print_int;
  print_newline ()

