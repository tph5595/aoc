open Core

(* let print_int_list lst = *)
(*   Printf.printf "["; *)
(*   List.iter ~f:(fun (x,y) -> Printf.printf "(%d, %d); " x y) lst; *)
(*   Printf.printf "]\n"; *)
(*   lst *)

(* let eq_sign x y = *) 
(*     (x > 0 && y > 0) || *) 
(*     (x < 0 && y < 0) || *) 
(*     (phys_equal x 0 && phys_equal y 0) *)

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

exception Invalid_state;;

let part2 file = 
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
            if start < 0 || start >= 100 then 
                raise Invalid_state
            else
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
                    l@[(c + (Int.abs (new_pos/100)) + dumb, (new_pos%100))]
    ) in 
    ans
    (* |> print_int_list *)
    |> List.rev 
    |> List.hd_exn
    |> fst


let read_file (filename : string) : string list =
  In_channel.read_lines filename

let file = "input"

let () = 
    let result = part1 (read_file file ) in 
    Printf.printf "Part 1: %d\n" result;
    let result = part2(read_file file ) in 
    Printf.printf "Part no: %d\n" result;
