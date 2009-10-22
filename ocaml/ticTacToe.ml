
open List
open String
open Printf

(******************)
(* Representation *)
(******************)

(* Individual TTT square.  Also represents the player. *)
type square = X | O | Empty of int 

(* Game Result *)
type result = Continue of square | Win of square | Draw 

(* Initial Empty Board *)
let starting_grid = [[ Empty 1; Empty 2; Empty 3];
                     [ Empty 4; Empty 5; Empty 6];
                     [ Empty 7; Empty 8; Empty 9]] 
;;

(* X always goes first *)
let starting_player = X

(* Return new square if current matches the target slot *)
let update_square replacement target_slot_no current =
    match current with
    | Empty slot_no when slot_no = target_slot_no -> replacement
    | current -> current
;;

(* Replace the given slot in the grid with the new square *)
let update_grid grid replacement target_slot_no = 
    map (map (update_square replacement target_slot_no)) grid
;;

(* Toggle X and O *)
let swap square = match square with X -> O | O -> X | square -> square

(* Determine win/lose/draw for a grid. *)
let result_of_grid current_player grid =

    let is_empty square =
        match square with Empty _ -> true | square -> false in

    let not_full grid =
        exists is_empty (flatten grid) in

    let next_player grid =
        if ((List.length (filter is_empty (flatten grid))) mod 2) = 1 then
            starting_player
        else
            swap starting_player in

    match grid with

    | [[a; _; _];  (* Diagonals *)
       [_; b; _];
       [_; _; c]] when a = b && b = c -> Win a
    | [[_; _; a];
       [_; b; _];
       [c; _; _]] when a = b && b = c -> Win a

    | [[a; b; c];  (* Horizontals *)
       [_; _; _];
       [_; _; _]] when a = b && b = c -> Win a
    | [[_; _; _];
       [a; b; c];
       [_; _; _]] when a = b && b = c -> Win a
    | [[_; _; _];
       [_; _; _];
       [a; b; c]] when a = b && b = c -> Win a

    | [[a; _; _];  (* Verticals *)
       [b; _; _];
       [c; _; _]] when a = b && b = c -> Win a
    | [[_; a; _];
       [_; b; _];
       [_; c; _]] when a = b && b = c -> Win a
    | [[_; _; a];
       [_; _; b];
       [_; _; c]] when a = b && b = c -> Win a

    (* If there's no winner, but empty squares remain, keep playing. *)
    | grid when not_full grid -> Continue (next_player grid)

    (* Otherwise, it must be a draw. *)
    | grid -> Draw

    (* XXX There's probably a better way to do this. XXX *)
;;

(**********)
(* Output *)
(**********)

(* String Representations *)
let x_mark_str	= "X"
let o_mark_str	= "O"
let wall_str	= "|"
let floor_str	= "\n---+---+---\n"

(* Convert a square type to its string representation *)
let string_of_square square =
    match square with 
    | Empty n -> string_of_int n
    | X -> x_mark_str
    | O -> o_mark_str
;;

(* Output the concrete grid representation of a square *)
let concrete square =
    match square with 
    | Empty n -> sprintf "(%d)" n
    | square -> sprintf " %s " (string_of_square square)
;;

(* Convert a whole grid to its string representation *)
let concrete_grid grid = map (map concrete) grid

(* Print the converted grid *)
let print_grid grid = 
    let rows = map (concat wall_str) (concrete_grid grid) in
    printf "\n%s\n" (concat floor_str rows)
;;

(* String representation of result type *)
let string_of_result result =
    match result with
    | Continue player -> sprintf "Select a square, %s: "  (string_of_square player);
    | Win player -> sprintf "%s Wins!" (string_of_square player)
    | Draw -> "It's a Draw!"
;;

(*********)
(* Input *)
(*********)

(* Read a number from standard input *)
let read_slot_no result =
    printf "\n%s"  (string_of_result result);
    try int_of_string (read_line())
    with Failure("int_of_string") -> -1
;;

(* Main Game Loop *)
let rec game_loop current_player grid =

    let result = result_of_grid current_player grid in
    let next_grid player = update_grid grid player (read_slot_no result) in

    print_grid grid;

    match result with
    | Continue player -> game_loop player (next_grid player)
    | result -> result

;;


