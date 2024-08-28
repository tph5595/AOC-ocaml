open Core;;

let sum_box lis = 
    match lis with
    | l :: w :: h :: [] ->
        let sides = [ l*w; w*h; h*l ] in 
        let sum = List.fold_left ~f:(+) ~init:0 sides in 
        let extra = BatList.min sides in 
        2*sum + extra
    | _ -> 0
;;

let get_boxes str = 
    str
    |> String.split_on_chars ~on:[ 'x' ]
    |> List.map ~f:Int.of_string
;;

let sum_ribbon lis = 
    match lis with
    | l :: w :: h :: [] ->
        let sides = BatList.min [ l+w; w+h; h+l ] in 
        let bow = l*w*h in 
        2*sides + bow
    | _ -> 0
;;

let read_lines file = 
    file
    |> In_channel.read_all 
    |> String.split_on_chars ~on:[ '\n' ; '\r' ]
    |> List.filter ~f:(fun s -> not (String.equal s "")) 
;;

let () = 
    let data = List.map ~f:get_boxes (read_lines "input.txt" ) in 

    let paper_total = List.fold_left ~f:(fun acc x -> acc + sum_box x) ~init:0 data in 
    Printf.printf "%d\n" paper_total;

    let ribbon_total = List.fold_left ~f:(fun acc x -> acc + sum_ribbon x) ~init:0 data in 
    Printf.printf "%d\n" ribbon_total;
;;
