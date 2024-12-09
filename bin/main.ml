open Core

(* let rec print_list = function *) 
(*     [] -> Printf.printf "\n"; () *)
(*     | e::l -> Printf.printf "%d|" e; print_list l *)

let part1 (l, r) = 
    let l = List.sort ~compare:(-) l in
    let r = List.sort ~compare:(-) r in 
    List.map2_exn l r ~f:(fun x y -> 
        x-y 
        |> Int.abs
    )
    |> List.fold ~init:0 ~f:(+)

let part2 (l, r) = 
    let r' = Hashtbl.create (module Int) in 
    List.iter r ~f:(fun x -> 
        match Hashtbl.find r' x with
        | Some y -> Hashtbl.set r' ~key:x ~data:(y+1)
        | None -> ignore (Hashtbl.add r' ~key:x ~data:1)
    );
    List.fold l ~init: 0 ~f:(fun acc x -> 
        match Hashtbl.find r' x with
        | Some y -> acc + x * y
        | None -> acc
    )

let parse data = 
    String.split_on_chars data ~on:['\n']
    |> List.map ~f:(fun x ->
            Str.split (Str.regexp "[ ]+") x
            |> List.map ~f:int_of_string
            )
    |> List.fold ~init:([], []) ~f:(fun acc x -> 
            match acc, x with
            | (l, r), x1::x2::[] -> x1::l, x2::r
            | _ -> acc
            )

let () = 
    let data = "./01.txt"
    |> In_channel.read_all 
    |> String.rstrip 
    |> parse in
    Printf.printf "%d\n" (part1 data);
    Printf.printf "%d\n" (part2 data);
