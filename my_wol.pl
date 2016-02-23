%prolog


test_strategy(N, Strat1, Strat2) :-

  test_strategy_helper(N, N, Strat1, Strat2, 0, 0, 0, 0, 0).

test_strategy_helper(1, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest) :-
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  
  FinTotalMoves is TotalMoves + NumMoves,
  format("ftm = ~w, winner is: ~w\n", [FinTotalMoves, WinningPlayer]),  

  FinBlueWins is BlueWins, 
  FinRedWins is RedWins, 
  FinHighest is Highest, 
  FinLowest is Lowest,
  format("fbw = ~w\n", [FinBlueWins]),

  ( WinningPlayer == 'b' ->
    format("w = ~w\n", [WinningPlayer]),

    format("fbw = ~w\n", [FinBlueWins]),
    %FinBlueWins = FinBlueWins + 1,
    FinBlueWins is BlueWins + 1,
    format("fbw = ~w\n", [FinBlueWins])

  ; (WinningPlayer == 'r' ->
      FinRedWins is FinRedWins + 1
    ;
      Y = 2
    )  
  ),
  format("HI", []),
  ( NumMoves > Highest ->
    FinHighest =  NumMoves
  ; (NumMoves < Lowest ->
    FinLowest = NumMoves 
  ;
    X = NumMoves
    )
  ),
  %format("BRUH", []),
  AverageMoves = InitN / FinTotalMoves, 
  Draws = InitN - BlueWins - RedWins,
  format("Highest: ~w\n, Lowest: ~w\n, AverageMoves: ~w\n, BlueWins: ~w\n, RedWins: ~w\n, Draws: ~w\n", [FinHighest, FinLowest, AverageMoves, FinBlueWins, FinRedWins, FinDraws]).

/*
test_strategy_helper(N, InitN, Strat1, Strat2, 0, 0, 0, 0, 0) :-
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  TotalMoves is NumMoves, 
  BlueWins is 0,
  RedWins is 0, 
  Draws is 0,
  ( WinningPlayer = 'b' ->
    BlueWins is 1
  ; (WinningPlayer = 'r' ->
      RedWins is 1
      ;
      Draws is 1
    )  
  ),
  Lowest is NumMoves,
  Highest is NumMoves,
  NewN is N -1,
  test_strategy_helper(NewN, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest).

*/

%test_strategy_helper(N, Strat1, Strat2, NumMoves, WinningPlayer) :-
  

  % Loop N times playing the game
  %  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),


  % Print the number of draws
  
  
  % Print the number of wins for each player


  % Print the maximum number of moves a game took
  
  % Print minimum number of moves a game took
  
  % Average moves in a game
  
  % Average time taken
  %  format("Number of moves was ~w\n", [NumMoves]),



  %GameMoveCounter = NumMoves,
  %Average =  GameMoveCounter / N,
  %format("Average move number was ~w\n", [Average]).


