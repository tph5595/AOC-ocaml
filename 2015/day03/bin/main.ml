open Core;;

module Point = struct
    module T = struct
        type t = { x: int; y: int }

        let compare t1 t2 = 
            let xs = Int.compare t1.x t2.x in
            if xs <> 0 then xs
            else Int.compare t1.y t2.y
        
        let sexp_of_t t: Sexp.t = 
            List [Atom (string_of_int t.x); Atom (string_of_int t.y)]
    end
    include T 
    include Comparator.Make(T)
end;;

let s = Set.of_list (module Point) [{x = 0; y = 0}; {x = 1; y = 0}];;

let () = 
    let data = "input.txt"
    |> In_channel.read_all
    in 
    print_endline data
