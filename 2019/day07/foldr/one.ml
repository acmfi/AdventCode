(* I'm using OOP and FP here. Fuck you, you are not my mom. *)

let program = 
  let fd = open_in "input.txt" in
  let p = input_line fd in
  close_in fd;
  p

let parse program = String.split_on_char ',' program |> List.map int_of_string 

let instantiate = Array.of_list

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
  | Op of mode * mode * mode * (int -> int -> int) * string
  | Jump of mode * mode * (int -> bool) * string
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
  | 1 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, add, "add")
  | 2 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, mul, "mul")
  | 3 -> In (nth_mode n 2)
  | 4 -> Out (nth_mode n 2)
  | 5 -> Jump (nth_mode n 2, nth_mode n 3, (fun n -> n != 0), "jump-if-true")
  | 6 -> Jump (nth_mode n 2, nth_mode n 3, (fun n -> n == 0), "jump-if-false")
  | 7 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, less_than, "less-than")
  | 8 -> Op (nth_mode n 2, nth_mode n 3, nth_mode n 4, equals, "equals")  
  | _ -> raise (Failure "Unk opcode")

type state = {
  pc : int;
  fin : unit -> int;
  fout : int -> unit;
}

let step { pc; fin; fout } n = { pc = pc + n; fin; fout }
let set_pc { pc; fin; fout } n = { pc =  n; fin; fout }

let rec run state program =
  match parse_opcode program.(state.pc) with
  | Op (m1, m2, m3, f, name) -> 
      Printf.printf "[%s instruction]\n" name;
      op program state.pc f m1 m2 m3;
      run (step state 4) program
  | In m -> 
      print_endline "[input instruction]"; 
      let n = state.fin () in
      wr_access program (state.pc + 1) n m;
      run (step state 2) program
  | Out m -> 
      print_endline "[output instruction]";
      state.fout (rd_access program (state.pc + 1) m);
      run (step state 2) program
  | Jump (m1, m2, p, name) -> 
      Printf.printf "[%s instruction]\n" name;
      if p (rd_access program (state.pc + 1) m1) 
      then begin
        print_endline "  [jumped]";
        run (set_pc state (rd_access program (state.pc + 2) m2)) program
      end
      else begin
        print_endline "  [didn't jumped]";
        run (step state 3) program
      end
  | Halt -> print_endline "[machine halted]"

let rec prompt f =
  print_string "input> ";
  read_int ()

let rec stdout =
  Printf.printf "%d\n"

let run_at_start = run { pc = 0; fin = prompt; fout = stdout }

let print_program program =
  Array.to_list program |> List.map string_of_int |> List.iter (Printf.printf "%s,")

class amplifier program phase (delegate : amplifier option) =
  object (self)
    val instance = instantiate program
    val mutable input_state = true
    val mutable last_output = None

    method feed_input () =
      if input_state then
        begin
          Printf.printf "input1=%d\n" phase;
          input_state <- false;
          phase 
        end
      else
        begin
          match delegate with
          | Some d -> begin 
            match d#run_and_get_output with
            | Some n -> Printf.printf "input2=%d\n" n; n
            | None -> raise (Failure "The delegate failed to provide the required output")
          end
          | None -> 0
        end
        
    method set_output n =
      last_output <- Some n

    method run_and_get_output =
      let state = { 
        pc = 0;
        fin = self#feed_input;
        fout = self#set_output
      } in
      run state instance;
      last_output
  end

let build_amplifier_chain program = function
  | [] -> raise (Failure "Provide some phases")
  | phases -> List.fold_left (fun amp phase ->
      Some (new amplifier program phase amp)) None phases

let run_chain = function
  | Some chain -> begin
    print_endline "========== Running chain ===========";
    match chain#run_and_get_output with
    | Some n -> n
    | None -> raise (Failure "The chain yielded no output")
  end
  | None -> raise (Failure "No chain provided")

let print lst =
  List.iter (fun l ->
      List.map string_of_int l
      |> String.concat ","
      |> print_endline
    ) lst

let rec permutations result other = function
  | [] -> [result]
  | hd :: tl ->
    let r = permutations (hd :: result) [] (other @ tl) in
    if tl <> [] then
      r @ permutations result (hd :: other) tl
    else
      r

let rec max acc : int list -> int = function
  | [] -> acc
  | h::t -> max (if h > acc then h else acc) t

let max_thruster_signal program =
  let results = permutations [] [] [0; 1; 2; 3; 4]
  |> List.map (fun c ->
      print [c];
      let chain = build_amplifier_chain (parse program) c in run_chain chain)
  in begin
    Printf.printf "Got %d results\n" (List.length results);
    results |> max 0
  end

let test1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let test2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
let test3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

let () = if max_thruster_signal test1 == 43210 
  then print_endline "Test 1 passed"
  else print_endline "Test 1 failed"
(*
let () = if max_thruster_signal test2 == 54321 
  then print_endline "Test 2 passed"
  else print_endline "Test 2 failed"

let () = if max_thruster_signal test3 == 65210 
  then print_endline "Test 3 passed"
  else print_endline "Test 3 failed"

let () =
  Printf.printf "I calculated %d permutations\n" (
  permutations [] [] [0; 1; 2; 3; 4]
  |> List.length
  )
*)

let () = 
  let max_signal = max_thruster_signal program in
  print_endline "Result of input:";
  print_int max_signal;
  print_newline ()


