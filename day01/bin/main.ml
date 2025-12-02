open Core

let part1 input = 
    let count = 50 in 
    let size = 100 in 
    (* let straight = *) 
    input 
    |> List.map ~f:(fun x -> 
        let s = String.to_list x in 
        match s with 
        | hd :: tl when phys_equal hd 'R' -> tl |> String.of_list |> Int.of_string
        | hd :: tl when phys_equal hd 'L' -> (tl |> String.of_list |> Int.of_string ) * -1
        | _ -> 0
    )
    |> List.fold_left ~init:[count] ~f:(fun l x -> [x + List.hd_exn l]@l)
    |> List.map ~f:(fun x -> x % size)
    |> List.map ~f:(fun x -> if x < 0 then x + size else x)
    |> List.filter ~f:(fun x -> phys_equal x 0)
    |> List.length
    (* let seen = Hashtbl.create (module Int) in *)
    (* List.fold_left ~init:0 ~f:(fun l x -> *) 
    (*     let prev = Hashtbl.find seen x in *) 
    (*     let times_seen = match prev with *) 
    (*     | Some x -> x + 1 *)
    (*     | None -> 1 in *) 
    (*     Hashtbl.set seen ~key:x ~data:times_seen; *) 
    (*     let Hashtbl.find seen (size-l)%size *)
    (*     l + (Hashtbl.find)) straight *)

let part2 _ = 0

let read_file (filename : string) : string list =
  In_channel.read_lines filename

let file = "input"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result = part2(read_file file ) in 
    Printf.printf "Part 2: %d\n" result;
