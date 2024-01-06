type mark =
    | O
    | X
    | Empty;;

type board = mark list list;;

let empty_board = [[Empty; Empty; Empty]; [Empty; Empty; Empty]; [Empty; Empty; Empty]];;
let dimension = 3;;
let enum_dimensions = List.init dimension (Fun.id)

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

let has_win board =
    let all_same list = List.for_all (fun x -> x = List.hd list) list && List.hd list <> Empty in
    let check_row r board =
        let row = List.nth board r in
        all_same row in
    let check_col c board =
        let col_list = List.map (fun row -> List.nth row c) board in
        all_same col_list in
    let check_down_right_diag board =
        let list = List.mapi (fun i row -> List.nth row i) board in
        all_same list in
    let check_down_left_diag board = 
        let reversed_board = List.map (fun row -> List.rev row) board in
        check_down_right_diag reversed_board in
    
    let perform_check f board = List.exists (fun i -> f i board) enum_dimensions in
    let check_all_rows board = perform_check check_row board in
    let check_all_cols board = perform_check check_col board in
    check_all_rows board || check_all_cols board || check_down_left_diag board || check_down_right_diag board;;
