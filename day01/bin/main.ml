open Core

module IntSet = Set.Make(Int)

let part1 input = 
    let count = 50 in 
    let straight = input 
        |> List.map ~f:(fun x -> 
            let s = String.to_list x in 
            match s with 
            | hd :: tl when phys_equal hd 'R' -> tl |> String.of_list |> Int.of_string
            | hd :: tl when phys_equal hd 'L' -> (tl |> String.of_list |> Int.of_string ) * -1
            | _ -> 0
        )
        |> List.fold_left ~init:[count] ~f:(fun l x -> [x + List.hd_exn l]@l)
        |> List.map ~f:(fun x -> if Int.abs x > 99 then x%100 else x )
        |> List.map ~f:(fun x -> if x < 0 then x + 100 else x)
    in 
    let seen = IntSet.of_list(straight) in 
    List.fold_left ~init:0 ~f:()

let part2 _ = 0

let read_file (filename : string) : string list =
  In_channel.read_lines filename

let file = "test"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result = part2(read_file file ) in 
    Printf.printf "Part 2: %d\n" result;
