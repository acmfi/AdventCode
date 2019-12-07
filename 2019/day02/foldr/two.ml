let program = read_line ()

let parse program = String.split_on_char ',' program |> List.map int_of_string
let deref program i = program.(program.(i))

let op program pc f = 
  let op1 = deref program (pc + 1)
  and op2 = deref program (pc + 2)
  and out = program.(pc + 3) in
  program.(out) <- f op1 op2

let add x y = x + y
let mul x y = x * y

let rec run pc program =
  match program.(pc) with
  | 1 -> op program pc add; run (pc + 4) program
  | 2 -> op program pc mul; run (pc + 4) program
  | 99 -> program
  | _ -> raise (Failure "Unk opcode")

let run_at_start = run 0

let print_program program =
  Array.to_list program |> List.map string_of_int |> List.iter (Printf.printf "%s,")

let one program =
  program.(1) <- 12;
  program.(2) <- 2;
  program

let two x y program =
  let program_arr = Array.of_list program in
  program_arr.(1) <- x;
  program_arr.(2) <- y;
  program_arr

(* OCaml does not have a range operator in Stdlib and I'm too lazy to setup Batteries or Core for this shit *)
let possible_values = List.init 100 (fun x -> x)

let possible_combinations =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) possible_values) possible_values)

let () = 
  let p = parse program in
  List.iter (fun (x,y) ->
    let instance = two x y p in
    run_at_start instance;
    if instance.(0) == 19690720 then
      string_of_int (100 * x + y) |> print_endline
    else
      ()
  ) possible_combinations

