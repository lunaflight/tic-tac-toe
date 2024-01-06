type mark =
    | O
    | X
    | Empty;;

type board = mark list list;;

let empty_board = [[Empty; Empty; Empty]; [Empty; Empty; Empty]; [Empty; Empty; Empty]];;

let print_board board =
let mark_to_string mark = match mark with
    | O -> "O"
    | X -> "X"
    | Empty -> "." in
let row_to_string row = String.concat " " (List.map mark_to_string row) in
let board_to_string board = String.concat "\n" (List.map row_to_string board) in
print_endline (board_to_string board);;

let put_mark mark r c board = 
    List.mapi (fun i x -> if i <> r then x
    else List.mapi (fun j y -> if j <> c then y else mark) x) board;;
