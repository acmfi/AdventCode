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

let explode_path =
  let aux acc dir =
    match acc with
    | [] -> expand (0,0) dir
    | last::path -> List.concat [expand last dir;last::path]
  in List.fold_left aux []

let wire1 = read_line () |> parse_wire |> explode_path |> List.filter (fun (x,y) -> (x,y) != (0,0))
let wire2 = read_line () |> parse_wire |> explode_path |> List.filter (fun x -> x != (0,0))

let manhattan (x,y) = (abs x) + (abs y)

let remove_zero = Seq.filter (fun x -> x != 0)
(* f x y == true if x < y conceptually *)
let min f = function
  | [] -> raise (Failure "min of empty list")
  | h::t -> List.fold_left (fun x y -> if f x y then x else y) h t

let cross_points = StepsSet.inter (StepsSet.of_list wire1) (StepsSet.of_list wire2)
let rec steps_to_cross_points wire cp =
 match wire with
 | []   -> []
 | h::t -> if StepsSet.mem h cp 
           then (h, (List.length t + 1))::steps_to_cross_points t cp
           else steps_to_cross_points t cp;;

let rec zip_steps ((x,y), d) = function
  | [] -> []
  | ((x',y'), d')::t -> if x == x' && y == y'
                   then ((x,y), d + d')::zip_steps ((x,y), d) t
                   else zip_steps ((x,y), d) t

let get_matches  =
  let wire1' = steps_to_cross_points wire1 cross_points in
  let wire2' = steps_to_cross_points wire2 cross_points in
  List.map (fun pd -> zip_steps pd wire2') wire1' |> List.concat

let snd (_, x) = x

let () = 
  get_matches 
  |> min (fun (p,d) (p',d') -> d < d')
  |> snd
  |> (Printf.printf "%d\n")

