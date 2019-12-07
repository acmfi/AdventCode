let program = 
  let fd = open_in "input.txt" in
  let p = input_line fd in
  close_in fd;
  p

let parse program = String.split_on_char ',' program |> List.map int_of_string |> Array.of_list

let deref_rd program i = program.(program.(i))
let deref_wr program i v = program.(program.(i)) <- v

let add x y = x + y
let mul x y = x * y
let less_than x y = if x < y then 1 else 0
let equals x y = if x == y then 1 else 0

type mode = I | P

let rd_access program pos = function
  | I -> program.(pos)
  | P -> deref_rd program pos
let wr_access program pos value = function
  | I -> raise (Failure "Runtime error: Inmediate mode write attempted")
  | P -> deref_wr program pos value

type opcode = 
  | Halt
  | Op of mode * mode * mode * (int -> int -> int)
  | Jump of mode * mode * (int -> bool)
  | In of mode
  | Out of mode

let op program pc f m1 m2 m3 = 
  let op1 = rd_access program (pc + 1) m1
  and op2 = rd_access program (pc + 2) m2 in
  wr_access program (pc + 3) (f op1 op2) m3

let rec pow m = function
  | 0 -> 1
  | n -> m * (pow m (n - 1))

let nth_mode n i : mode =
  match n / (pow 10 i) mod 10 with
  | 0 -> P
  | 1 -> I
  | _ -> raise (Failure "Unk mode")

let parse_opcode n : opcode = 
  match (n mod 100) with
  | 99 -> Halt
  | 1 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, add)
  | 2 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, mul)
  | 3 -> In (nth_mode n 2)
  | 4 -> Out (nth_mode n 2)
  | 5 -> Jump (nth_mode n 2, nth_mode n 3, fun n -> n != 0)
  | 6 -> Jump (nth_mode n 2, nth_mode n 3, fun n -> n == 0)
  | 7 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, less_than)
  | 8 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, equals)  
  | _ -> raise (Failure "Unk opcode")

let rec run pc program =
  match parse_opcode program.(pc) with
  | Op (m1, m2, m3, f) -> op program pc f m1 m2 m3;
                        run (pc + 4) program
  | In m -> print_string "input> ";
          wr_access program (pc + 1) (read_int ()) m;
          run (pc + 2) program
  | Out m -> Printf.printf "%d: %d\n" program.(pc + 1) (rd_access program (pc + 1) m);
            run (pc + 2) program
  | Jump (m1, m2, p) -> if p (rd_access program (pc + 1) m1) 
                        then run (rd_access program (pc + 2) m2) program
                        else run (pc + 3) program
  | Halt -> ()

let run_at_start = run 0

let print_program program =
  Array.to_list program |> List.map string_of_int |> List.iter (Printf.printf "%s,")

let () = parse program |> run_at_start 
