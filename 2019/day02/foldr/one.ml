let program = read_line ()

let parse program = String.split_on_char ',' program |> List.map int_of_string |> Array.of_list

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

let () = parse program |> one |> run_at_start |> print_program
