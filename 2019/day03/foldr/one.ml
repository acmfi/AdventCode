module StepsSet = Set.Make(struct
  type t = int * int

  let compare = compare
end)

type dir = 
  | R of int
  | L of int
  | U of int
  | D of int

let steps = function
| R x | L x | U x | D x -> x

let steps_from_dir s =
  String.sub s 1 ((String.length s) - 1) |> int_of_string

let parse_direction s =
  let steps = steps_from_dir s in
  match s.[0] with
  | 'R' -> R steps
  | 'L' -> L steps
  | 'U' -> U steps
  | 'D' -> D steps
  | _ -> raise (Failure "unk direction")
let parse_wire s = String.split_on_char ',' s |> List.map parse_direction

let expand (i,j) dir =
  List.init (steps dir) (fun idx ->
    match dir with
    | R _ -> (i + idx + 1, j)
    | L _ -> (i - idx - 1, j)
    | U _ -> (i, j + idx + 1)
    | D _ -> (i, j - idx - 1)
  ) |> List.rev

let explode_path directions =
  List.fold_left 
    (fun (last::path) dir -> 
      List.concat [expand last dir;last::path]
    )
    [(0,0)] directions

let wire1 = read_line () |> parse_wire |> explode_path
let wire2 = read_line () |> parse_wire |> explode_path

let manhattan (x,y) = (abs x) + (abs y)

let remove_zero = Seq.filter (fun x -> x != 0)
let min (h::t) = List.fold_left (fun x y -> if x > y then y else x) h t


let cross_points = StepsSet.inter (StepsSet.of_list wire1) (StepsSet.of_list wire2)

let () =
  StepsSet.to_seq cross_points
    |> Seq.map manhattan 
    |> remove_zero
    |> List.of_seq
    |> min
    |> (Printf.printf "%d\n")


