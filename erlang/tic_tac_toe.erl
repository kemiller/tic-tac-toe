
-module(tic_tac_toe).
-export([main/0]).

%% Move-Making
place(Head, Space, {continue, CurrentPlayer, [Space|Rest], NextPlayer}) ->
	Board = Head ++ [CurrentPlayer|Rest],
	{result(Board), NextPlayer, Board, CurrentPlayer};

place(Head, Space, {continue, CurrentPlayer, [X|Rest], NextPlayer}) ->
	place(Head ++ [X], Space, {continue, CurrentPlayer, Rest, NextPlayer});

place(Head, _Space, {continue, CurrentPlayer, [], NextPlayer}) ->
	{continue, CurrentPlayer, Head, NextPlayer}.

place(Space, GameState) ->
	place([], Space, GameState).

%% Result-Checking
result([P, P, P, _, _, _, _, _, _]) -> win; 
result([_, _, _, P, P, P, _, _, _]) -> win; 
result([_, _, _, _, _, _, P, P, P]) -> win; 
result([P, _, _, P, _, _, P, _, _]) -> win; 
result([_, P, _, _, P, _, _, P, _]) -> win; 
result([_, _, P, _, _, P, _, _, P]) -> win; 
result([P, _, _, _, P, _, _, _, P]) -> win; 
result([_, _, P, _, P, _, P, _, _]) -> win;

result([H|_]) when is_integer(H) -> continue;
result([_|T])                    -> result(T);
result([])                       -> tie.

%% Output
fmt() -> "\n~s|~s|~s\n---+---+---\n~s|~s|~s\n---+---+---\n~s|~s|~s\n\n".

cell_repr(X) when is_integer(X) -> io_lib:format("(~p)",[X]);
cell_repr(X)                    -> io_lib:format(" ~s ",[X]).

print(Xs) 	            -> io:format(fmt(), [cell_repr(X) || X <- Xs]).

check_status({continue, CurP, Board, NextP}) ->
	Prompt = io_lib:format("Select a square, ~s: ", [CurP]),
	{Space, _} = string:to_integer(io:get_line(Prompt)),
	place(Space, {continue, CurP, Board, NextP});

check_status({tie, _, _, _}) ->
	io:format("It's a Draw!\n"),
	init:stop();

check_status({win, _, _, Winner}) ->
	io:format("~s Wins!\n", [Winner]),
	init:stop().

%% Game Loop
play(GameState) ->
	{_, _, Board, _} = GameState,
	print(Board),
	play(check_status(GameState)).

main() ->
	play({continue, "X", [1,2,3,4,5,6,7,8,9], "O"}).



