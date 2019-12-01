
let read_next () = try
        let n = read_int () in Some n
        with End_of_file -> None
;;

let convert mass = mass / 3 - 2;;

let rec one x = 
        match read_next () with
        | Some n -> one (x  + (convert n))
        | None   -> x
;;

let () = one 0 |> string_of_int |> print_endline
