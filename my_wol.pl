%prolog


test_strategy(N, Strat1, Strat2) :-
  test_strategy_helper(N, N, Strat1, Strat2, 0, 0, 0, 0, 250).

test_strategy_helper(1, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest) :-
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),

  %format("Should be a drawn when: ~w\n", [WinningPlayer]),  
  
  FinTotalMoves is TotalMoves + NumMoves,
  (WinningPlayer == 'b' ->
    FinBlueWins is BlueWins + 1,
    FinRedWins is RedWins,
    Draws is InitN - FinBlueWins - RedWins
  ; (WinningPlayer == 'r' ->
      FinBlueWins is BlueWins,
      FinRedWins is RedWins + 1,
      Draws is InitN - BlueWins - FinRedWins
    ;
      FinBlueWins is BlueWins,
      FinRedWins is RedWins,
      Draws is InitN - BlueWins - FinRedWins
    )  
  ),
  
  ( NumMoves > Highest ->
    FinHighest is NumMoves
  ;
    FinHighest is Highest
  ),
  ( NumMoves < Lowest ->
    FinLowest is NumMoves
  ;
    FinLowest is Lowest
  ),

  AverageMoves is FinTotalMoves / InitN, 
  format("Highest: ~w\nLowest: ~w\nAverageMoves: ~w\nBlueWins: ~w\nRedWins: ~w\nDraws: ~w\n", [FinHighest, FinLowest, AverageMoves, FinBlueWins, FinRedWins, Draws]).


test_strategy_helper(N, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest) :-
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),

  NewTotalMoves is TotalMoves + NumMoves,

  (WinningPlayer == 'b' ->
    NewBlueWins is BlueWins + 1,
    NewRedWins is RedWins   
  ; (WinningPlayer == 'r' ->
      NewBlueWins is BlueWins,
      NewRedWins is RedWins + 1
    ;
      NewBlueWins is BlueWins,
      NewRedWins is RedWins
    )  
  ),
  
  ( NumMoves > Highest ->
    NewHighest is NumMoves
  ;
    NewHighest is Highest
  ),
  ( NumMoves < Lowest ->
    NewLowest is NumMoves
  ;
    NewLowest is Lowest
  ),
  
  NewN is N - 1,
  test_strategy_helper(NewN, InitN, Strat1, Strat2, NewTotalMoves, NewBlueWins, NewRedWins, NewHighest, NewLowest). 

  


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


