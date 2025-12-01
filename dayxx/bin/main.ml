open Core

let part1 file = 
    let lists = file
        |> List.map ~f:(String.split_on_chars ~on:[' '; '\t'; '\n'; '\r'])
        |> List.map ~f:(List.map ~f:(int_of_string)) in 
    let first  = List.map ~f:(fun l -> List.nth_exn l 0) lists 
        |> List.sort ~compare:Int.compare in 
    let second = List.map ~f:(fun l -> List.nth_exn l 1) lists 
            |> List.sort ~compare:Int.compare in 
    List.fold2_exn ~init:0 ~f:(fun acc f s -> acc + f - s) first second

let part2 _file = 
    0;;


let read_file (filename : string) : string list =
  In_channel.read_lines filename

let file = "input"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result = part2(read_file file ) in 
    Printf.printf "Part 2: %d\n" result;
