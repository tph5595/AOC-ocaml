open Core;;

let is_vowel c = 
    if String.contains "aeiou" c then 1 else 0

let is_bad a b = 
    let bad = ["ab"; "cd"; "pq"; "xy"] in 
    let chs = Printf.sprintf "%c%c" a b in
    if List.mem ~equal:String.equal bad chs then true else false

let part_1 input = 
    let rec aux last_char num_vowels twice input = 
        match input with
        | c :: _ when (is_bad last_char c ) -> 0
        | c :: rest -> aux c (num_vowels + (is_vowel c)) (twice || (Char.equal c last_char)) rest
        | _ -> if num_vowels > 2 && twice then 1 else 0
    in 
    List.map ~f:(fun x -> aux '9' 0 false (String.to_list x)) input
    |> List.fold_left ~f:(+) ~init:0

let part_2 input = 
    let set = Set.of_list (module String) [] in
    let rec aux good_last pairs good_pairs input = 
        match input with
        | a :: b :: rest -> 
                let new_good_last = match rest with
                | c :: _ when Char.equal a c -> true
                | _ -> good_last in 
                let p = Printf.sprintf "%c%c" a b in 
                let new_good_pairs = Set.mem pairs p in 
                aux new_good_last (Set.add pairs p) new_good_pairs (b :: rest)
        | _ -> if good_last && good_pairs then 1 else 0
    in 
    List.map ~f:(fun x -> aux false set false (String.to_list x)) input
    |> List.fold_left ~f:(+) ~init:0

let () = 
    let data = "t"
    |> In_channel.read_lines in 
    Printf.printf "%d\n" (part_1 data);
    Printf.printf "%d\n" (part_2 data);
;;
