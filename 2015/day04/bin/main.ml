let rec part_1 start num = 
    let str = Digest.to_hex (Digest.string (start ^ Int.to_string num))in 
    if  String.sub str 0 5 = "00000" then
        num
    else
        part_1 start (num + 1)

let rec part_2 start num = 
    let str = Digest.to_hex (Digest.string (start ^ Int.to_string num))in 
    if  String.sub str 0 6 = "000000" then
        num
    else
        part_2 start (num + 1)
let () = 
    Printf.printf "%d\n" (part_1 "yzbqklnj" 0);
    Printf.printf "%d\n" (part_2 "yzbqklnj" 0)
