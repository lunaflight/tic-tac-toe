module Board = Lib.Board;;

let rec new_game () =
    let end_game msg = 
        print_endline msg;
        print_endline "Play again?";
        let resp = read_line () in
        if String.lowercase_ascii resp = "y" then new_game ()
        else print_endline "Thanks for playing!"; in


    let rec new_turn mark msg board = 
        Printf.printf msg(Board.mark_to_string mark);
        let row = read_int () in
        let col = read_int () in
        let new_board = Board.put_mark mark row col board in
        if Board.has_win new_board then
            end_game "Winner!"
        else if Board.is_full new_board then
            end_game "Tie."
        else (
            Board.print_board new_board;
            new_turn (Board.negate mark) "It's %s's turn.\n" new_board
        ) in


    print_endline("Starting a new game...");
    let board = Board.empty_board in
    Board.print_board board;
    new_turn Board.X "It's %s's turn.\n" board;;


let () = new_game ();
