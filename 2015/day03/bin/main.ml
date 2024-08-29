open Core;;

module Point = struct
    module T = struct
        type t = { x: int; y: int }
        [@@deriving compare, sexp_of]
    end
    include T 
    include Comparator.Make(T)
end;;

let solve_part_1 input = 
    let open Point.T in 
    let rec aux input cur hash_set = 
        match input with 
        | c :: tail ->
            let n = match c with 
                | '>' -> {cur with x = cur.x + 1}
                | '<' -> {cur with x = cur.x - 1}
                | '^' -> {cur with y = cur.y + 1}
                | 'v' -> {cur with y = cur.y - 1}
                | _ -> cur
            in 
            aux tail n (Set.add hash_set n)
        | _ -> hash_set
    in 
    let pos = {x = 0; y = 0} in 
    let set = aux (String.to_list input) pos (Set.of_list (module Point) [ pos ]) in
    Set.length set
;;

let solve_part_2 input = 
    let open Point.T in 
    let rec aux input cur1 cur2 hash_set = 
        match input with 
        | c :: tail ->
            let n = match c with 
                | '>' -> {cur1 with x = cur1.x + 1}
                | '<' -> {cur1 with x = cur1.x - 1}
                | '^' -> {cur1 with y = cur1.y + 1}
                | 'v' -> {cur1 with y = cur1.y - 1}
                | _ -> cur1
            in 
            aux tail cur2 n (Set.add hash_set n)
        | _ -> hash_set
    in 
    let pos1 = {x = 0; y = 0} in 
    let pos2 = {x = 0; y = 0} in 
    let set = aux (String.to_list input) pos1 pos2 (Set.of_list (module Point) [ pos1 ]) in
    Set.length set
;;

let () = 
    let data = "input.txt"
    |> In_channel.read_all
    in 
    let part1 = solve_part_1 data in 
    let part2 = solve_part_2 data in 
    Printf.printf "%d\n%d\n" part1 part2
