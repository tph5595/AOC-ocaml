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

let rec basement n pos s = 
    if n < 0 then
        pos
    else
        begin 
            match s with
            | [] -> 0
            | '(':: t -> basement (n+1) (pos+1) t
            | ')':: t -> basement (n-1) (pos+1) t
            | _h :: t -> basement n (pos+1) t
        end

let read_file file =
  In_channel.with_open_bin file In_channel.input_all;;

let file = "01.txt"

let () = 
    let result = diff (explode (read_file file )) in 
    Printf.printf "%d\n" result;
    let result = basement 0 0 (explode (read_file file )) in 
    Printf.printf "%d\n" result;
