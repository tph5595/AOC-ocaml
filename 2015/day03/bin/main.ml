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
    let rec aux input cur  hash_set = 
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

let () = 
    let data = "input.txt"
    |> In_channel.read_all
    in 
    let part1 = solve_part_1 data in 
    Printf.printf "%d\n" part1
