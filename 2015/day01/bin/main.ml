let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let rec diff s = 
    match s with
    | [] -> 0
    | '(':: t -> 1 + diff t
    | ')':: t -> -1 + diff t
    | _h :: t -> 0 + diff t

let read_file file =
  In_channel.with_open_bin file In_channel.input_all;;

let file = "01.txt"

let () = 
    let result = diff (explode (read_file file )) in 
    print_int result;
