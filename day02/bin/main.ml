open Core

exception Invalid_state;;

let remove_newlines_regex (s : string) : string =
  Str.global_replace (Str.regexp "\\n") "" s

let explode s = List.init (String.length s) ~f:(String.get s)

let pop_first t =
    (List.tl_exn (fst t), List.tl_exn (snd t))

let string_to_lists l =
    let chars = explode l  |> List.map ~f:(Char.get_digit_exn) in 
    List.split_n chars (List.length chars)


let  get_invalid start_num end_num = 
    (* Assume same length and even length *)
    let start_lists = string_to_lists start_num in 
    let end_lists = string_to_lists end_num in 

    let rec count start_lists end_lists = 

        let next_start = List.hd_exn (fst start_lists) in
        let next_end = List.hd_exn (fst end_lists) in
        if next_start < next_end then 
            match List.tl (fst start_lists) with
            | None -> 1
            | Some l -> 
                    let first_diff = (List.hd_exn (fst end_lists)) - (List.hd_exn (fst start_lists)) in 
                    let tail_length = (List.length l) in 
                    first_diff * (Int.pow 10 tail_length)

        else if phys_equal next_start next_end then 
            match List.length (fst start_lists) with
            | 1 | 0 -> 1
            | _ -> count (pop_first start_lists) (pop_first end_lists)

        else
            (* Next digit cannot go high enough*)
            0 
    in 
    count start_lists end_lists

let part1 data = 
    data 
    |> List.map ~f:(fun x -> match x with
        | x::y::[] -> get_invalid x y
        | _ -> raise Invalid_state)
    |> List.fold ~init:0 ~f:(+)


let part2 _data = 
    0


let () = 
    let data = "test"
    |> In_channel.read_all
    |> remove_newlines_regex
    |> String.split ~on:','
    |> List.map ~f:(String.split ~on:'-')
    (* |> List.map ~f:(List.map ~f:(Int.of_string)) *)
    in 
    let result = part1 data in 
    Printf.printf "Part 1: %d\n" result;
    let result = part2 data in 
    Printf.printf "Part no: %d\n" result;
