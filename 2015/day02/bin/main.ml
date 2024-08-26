open Core;;

(* type box = { *)
(*     l: int; *)
(*     w: int; *)
(*     h: int; *)
(* };; *)

(* let make_box dims = *)
(*     match dims with *)
(*     | [l; w; h;] -> {l=l; w=w; h=h;} *)
(*     | _ -> {l=0; w=0; h=0;} *)
(* ;; *)


let parse_box file_contents = 
    file_contents
    (* Get lines *)
    |> String.split_on_chars ~on:[ '\n' ; '\r' ]
    |> List.filter ~f:(fun s -> not (String.equal s ""))
    (* Get dims *)
    |> String.split_on_chars ~on:[ 'x' ]
    |> List.filter ~f:(fun s -> not (String.equal s ""))
    (* Create records *)
    |> List.map ~f:(fun dims -> make_box dims)
;;

"input.txt"
|> In_channel.read_all
|> parse_box
(* |> print_string; *)
