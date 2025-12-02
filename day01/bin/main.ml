open Core

let eq_sign x y = 
    (x > 0 && y > 0) || 
    (x < 0 && y < 0) || 
    (phys_equal x 0 && phys_equal y 0)

let part1 input = 
    let count = 50 in 
    let size = 100 in 
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

    (*

    When to count:
        - Assume start at positive number
        - add number
        - If end positive
            - every time over 100
            - if land at zero
        - If end negative
            - +1 for negative
            - everytime under -100
            - if land at zero
        - Modulo and make positive before next iteration

     *)

let part_no file = 
    let start = 50 in 
    let straight = file 
    |> List.map ~f:(fun x -> 
        let s = String.to_list x in 
        match s with 
        | hd :: tl when phys_equal hd 'R' -> tl |> String.of_list |> Int.of_string
        | hd :: tl when phys_equal hd 'L' -> (tl |> String.of_list |> Int.of_string ) * -1
        | _ -> 0
    ) in
    let ans = straight
    |> List.fold_left ~init:[(0, start)] ~f:(fun l x -> 
            let (c, last) = List.hd_exn (List.rev l) in 
            let new_pos = last + x in 
            (*Exact hit on 0*)
            if phys_equal new_pos 0 then
                l@[(c+1, new_pos)]
            (* Positive end *)
            else if new_pos > 0 then
                l@[(c + (new_pos/100), new_pos%100)]
            (* Negative end *)
            else
                (* If started at zero then no auto +1 for being negative *)
                let dumb = if phys_equal last 0 then 0 else 1 in 
                l@[(c + (new_pos/100) + dumb, (new_pos%100))]
    ) in 
    ans
    |> List.rev 
    |> List.hd_exn
    |> fst

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
    |> List.rev
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

let file = "test2"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result, r2, r3 = part2(read_file file ) in 
    Printf.printf "Part 2: %d\t%d\t%d\n" result r2 r3;
    let result = part_no(read_file file ) in 
    Printf.printf "Part no: %d\n" result;
