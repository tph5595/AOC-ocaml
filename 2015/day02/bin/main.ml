open Core;;

type box = {
    l: int;
    w: int;
    h: int;
};;

let make_box dims =
    (* List.iter (Printf.printf "%d ") dims; *)
    let lis = List.map ~f:Int.of_string dims in 
    match lis with
    | [l; w; h;] -> {l; w; h;}
    | _ -> {l=0; w=0; h=0;}
;;

let sum_box { l; w; h } = 
    let sides = [ l*w; w*h; h*l ] in 
    let sum = List.fold_left ~f:(+) ~init:0 sides in 
    let extra = BatList.min sides in 
    2*sum + extra
;;

let get_dims str = 
    str
    |> String.split_on_chars ~on:[ 'x' ]
    |> List.filter ~f:(fun s -> not (String.equal s ""))
;;

let split_boxes str = 
    str
    |> String.split_on_chars ~on:[ '\n' ; '\r' ]
    |> List.filter ~f:(fun s -> not (String.equal s "")) 
;;

let parse_box file_contents = 
    let lines = split_boxes file_contents in 
    let dims = List.map ~f:get_dims lines in 
    let boxes = List.map ~f:(fun dim -> make_box dim ) dims in 
    boxes
;;


let () = 
    let result = "input.txt"
    |> In_channel.read_all
    |> parse_box 
    |> List.map ~f:sum_box
    |> List.fold_left ~f:(+) ~init:0 
    in 
    Printf.printf "%d\n" result
