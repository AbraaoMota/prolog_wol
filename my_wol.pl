%prolog


test_strategy(N, Strat1, Strat2) :-
  test_strategy_helper(N, N, Strat1, Strat2, 0, 0, 0, 0, 250, 0).

test_strategy_helper(1, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest, TimeInGame) :-
  
  statistics(walltime, _),
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  statistics(walltime, [_, Y]),
  
  GameTime is Y,

  FinTotalMoves is TotalMoves + NumMoves,
  FinGameTime is TimeInGame + GameTime,
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
  AverageTime is FinGameTime / InitN,
  format("Highest: ~w\nLowest: ~w\nAverageMoves: ~w\nBlueWins: ~w\nRedWins: ~w\nDraws: ~w\nAverageTimePerGame: ~w", [FinHighest, FinLowest, AverageMoves, FinBlueWins, FinRedWins, Draws, AverageTime]).


test_strategy_helper(N, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest, TimeInGame) :-
  statistics(walltime, _),
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  statistics(walltime, [_, Y]),
  GameTime is Y,
  NewTotalTime is TimeInGame + GameTime,
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
  test_strategy_helper(NewN, InitN, Strat1, Strat2, NewTotalMoves, NewBlueWins, NewRedWins, NewHighest, NewLowest, NewTotalTime). 

bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  bloodlust_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).
  %format("MOVE BEING PICKED IS: ~w\n", [Move]).
  
bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  bloodlust_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).
  %format("MOVE BEING PICKED IS: ~w\n", [Move]).

bloodlust_move(Alive, OtherPlayerAlive, Move, Player) :-
  
  % Find all the possible moves given a player colour and a current board state
  findall([A,B,MA,MB],(member([A,B], Alive),
                       neighbour_position(A,B,[MA,MB]),
                       \+member([MA,MB],Alive),
                       \+member([MA,MB],OtherPlayerAlive)),  
          PossMoves),
  
  (Player ==  'b' ->
   State = [Alive, OtherPlayerAlive]
  ;
   State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  bloodlust_move_helper(PossMoves, 65, Move, State, Player).

bloodlust_move_helper([X], Min, Move, State, Player) :-
  move_and_crank(X, State, [NewBlues, NewReds]),
  
  % Check the length of the opposing players alive list, if smaller than min, return as move
  (Player == 'b' ->
    length(NewReds, RLen),
    % format("Length of NewReds is: ~w\n", [RLen]),
    (RLen < Min ->
      NewMin is RLen,
      Move = X
    ;
      NewMin is Min
    )
  ;
    length(NewBlues, BLen),
    (BLen < Min ->
      NewMin is BLen,
      Move = X
    ;
      NewMin is Min
  )).
  

bloodlust_move_helper([X|Xs], Min, Move, State, Player) :-
  move_and_crank(X, State, [NewBlues, NewReds]),
  
  % Check the length of the opposing players alive list, if smaller than min, return as move
  (Player == 'b' ->
    length(NewReds, RLen),
    (RLen < Min ->
      NewMin is RLen,
      Move = X
    ;
      NewMin is Min
    )
  ;
    length(NewBlues, BLen),
    (BLen < Min ->
      NewMin is BLen,
      Move = X
    ;
      NewMin is Min
  )),

  bloodlust_move_helper(Xs, NewMin, Move, State, Player).



move_and_crank(Move, [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds]) :-
  alter_board(Move, AliveBlues, IntAliveBlues),
  alter_board(Move, AliveReds, IntAliveReds),
  next_generation([IntAliveBlues,IntAliveReds], [NewAliveBlues,NewAliveReds]).


