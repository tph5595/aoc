open Core

let eq_sign x y = 
    (x > 0 && y > 0) || 
    (x < 0 && y < 0) || 
    (phys_equal x 0 && phys_equal y 0)

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

let part_no input = 
    let count = 50 in 
    let size = 100 in 
    let straight = input 
    |> List.map ~f:(fun x -> 
        let s = String.to_list x in 
        match s with 
        | hd :: tl when phys_equal hd 'R' -> tl |> String.of_list |> Int.of_string
        | hd :: tl when phys_equal hd 'L' -> (tl |> String.of_list |> Int.of_string ) * -1
        | _ -> 0
    ) in

    let mid1 = straight
    |> List.map ~f:(fun x -> Int.abs (x / size))
    |> List.fold_left ~init:0 ~f:(+) in 

    let s2 = straight
    |> List.fold_left ~init:[count] ~f:(fun l x -> [x + List.hd_exn l]@l)
    |> List.map ~f:(fun x -> x % size) in 

    let mid2 = s2
    |> List.map ~f:(fun x -> if x < 0 then 1 else 0)
    |> List.fold_left ~init:0 ~f:(+) in 
    let s3 = s2
    |> List.map ~f:(fun x -> if x < 0 then x + size else x)
    |> List.filter ~f:(fun x -> phys_equal x 0)
    |> List.length in 
    s3, mid1, mid2

let part2 input = 
    let start = 50 in 
    let size = 100 in 
    let turns = input 
    |> List.map ~f:(fun x -> 
        let s = String.to_list x in 
        match s with 
        | hd :: tl when phys_equal hd 'R' -> tl |> String.of_list |> Int.of_string
        | hd :: tl when phys_equal hd 'L' -> (tl |> String.of_list |> Int.of_string ) * -1
        | _ -> 0
    ) in 
    let end_positions = turns
    |> List.fold_left ~init:[start] ~f:(fun l x -> [x + List.hd_exn l]@l)
    |> List.map ~f:(fun x -> x % size)
    |> List.map ~f:(fun x -> if x < 0 then x + size else x)
    |> List.filter ~f:(fun x -> phys_equal x 0)
    |> List.length
    in 
    let internal = turns 
    |> List.fold_left ~init:0 ~f:(fun c x -> c + Int.abs (x/100)) in 
    let (_, between) = turns
    |> List.fold_left ~init:[start] ~f:(fun l x -> [x + List.hd_exn l]@l)
    (* |> List.fold_left ~init:[start] ~f:(fun l x -> [x + List.hd_exn l]@l) *)

    (* |> List.fold_left ~init:(0, start) ~f:(fun (count, l) x -> *) 
    (*     let n = (Int.abs (l-x))/100 in *) 
    (*     (count + n, x + l)) *)

    |> List.fold_left ~init:(start,0) ~f:(fun (last, count) x -> 
            if ((phys_equal (last/size) (x/size)) 
                && eq_sign last x)
                || (phys_equal (x%100) 0)
            then (x, count)
            else (x, count + 1) 
    )
    in 
    internal, between, end_positions

let read_file (filename : string) : string list =
  In_channel.read_lines filename

let file = "input"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result, r2, r3 = part2(read_file file ) in 
    Printf.printf "Part 2: %d\t%d\t%d\n" result r2 r3;
    let result, r2, r3 = part_no(read_file file ) in 
    Printf.printf "Part 2: %d\t%d\t%d\n" result r2 r3;
