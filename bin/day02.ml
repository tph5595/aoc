open Core

let check_dir x y dir = 
    let d = x-y in 
    if dir = 0 then Some d
    else if d = 0 then Some dir
    else if dir < 0 && d < 0 then Some d
    else if dir > 0 && d > 0 then Some d
    else None

let is_valid x y = 
    let diff = Int.abs (x-y) in
    if diff >= 1 && diff <= 3 then true else false

let full_check x y dir =
                let d = check_dir x y dir in 
                match d with
                    | Some d' ->
                            if is_valid x y then
                                Some d'
                            else
                                None
                    | None -> None

let check_line_1 values = 
    let rec check_line' values dir = 
        match values with 
        | [] | _::[] -> true
        | x::y::z-> match full_check x y dir with
                    | Some d' -> check_line' (y::z) d'
                    | None -> false
    in
    check_line' values 0

let check_line_21 values = 
    let rec check_line' values dir skipped = 
        match values with 
        | [] | _::[] -> true
        | x::y::z -> match full_check x y dir with
                    | Some d' -> 
                            (*no skip*)
                            check_line' (y::z) d' skipped ||
                            (*y,z bad; skip y*)
                            (not skipped && check_line' (x::z) d' true)
                    (*x, y bad*)
                    | None -> (
                        (*make sure we have not used our skip*)
                            if skipped then false else
                            (*skip x*)
                            check_line' (y::z) dir true ||
                            (*skip y*)
                            check_line' (x::z) dir true
                            )
    in
    check_line' values 0 false

let part1 data = 
    List.map data ~f:(fun x -> check_line_1 x)
    |> List.filter ~f:(Fun.id)
    |> List.length

let part2 data = 
    List.map data ~f:(fun x -> check_line_21 x)
    |> List.filter ~f:(Fun.id)
    |> List.length

let parse data = 
    String.split_on_chars data ~on:['\n']
    |> List.map ~f:(fun x ->
            Str.split (Str.regexp "[ ]+") x
            |> List.map ~f:int_of_string
            )

let () = 
    let data = "./test02.all.txt"
    |> In_channel.read_all 
    |> String.rstrip 
    |> parse in
    Printf.printf "%d\n" (part1 data);
    Printf.printf "%d\n" (part2 data);
