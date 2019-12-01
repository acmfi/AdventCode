
let read_next () = try
        let n = read_int () in Some n
        with End_of_file -> None
;;

let convert mass = mass / 3 - 2;;

let rec two x =
        let fuel = convert x in
        if fuel <= 0 
        then x
        else x + two fuel

let rec one acc = 
        match read_next () with
        | Some mass -> let fuel = convert mass |> two in
                       one (acc + fuel)
        | None      -> acc
;;


let () = one 0 |> string_of_int |> print_endline
