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

move_and_crank(Move, [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds], Player) :-
    (Player == 'b' ->
      alter_board(Move, AliveBlues, IntAliveBlues),
      IntAliveReds = AliveReds
    ;
      alter_board(Move, AliveReds, IntAliveReds),
      IntAliveBlues = AliveBlues
    ),
    next_generation([IntAliveBlues,IntAliveReds], [NewAliveBlues,NewAliveReds]).

bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  bloodlust_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  bloodlust_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

bloodlust_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  findall([A,B,MA,MB],(member([A,B], Alive),
                       neighbour_position(A,B,[MA,MB]),
                       \+member([MA,MB],Alive),
                       \+member([MA,MB],OtherPlayerAlive)),
          [P1|PossMoves]),

  (Player ==  'b' ->
   State = [Alive, OtherPlayerAlive]
  ;
   State = [OtherPlayerAlive, Alive]
  ),
  % Given a list of possible moves, run move and crank for each, return the lowest
  bloodlust_move_helper([P1|PossMoves], 65, P1, Move, State, Player).

bloodlust_move_helper([X], Min, SuggestedMove, FinalMove, State, Player) :-
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),
  % Check the length of the opposing players alive list, if smaller than min, return as move
  (Player == 'b' ->
    length(NewReds, RLen),
    (RLen < Min ->
      FinalMove = X
    ;
      FinalMove = SuggestedMove
    )
  ;
    length(NewBlues, BLen),
    (BLen < Min ->
      FinalMove = X
    ;
      FinalMove = SuggestedMove
    )
  ).

bloodlust_move_helper([X|Xs], Min, SuggestedMove, Move, State, Player) :-
  move_and_crank(X, State, [NewBlues, NewReds], Player),
  % Check the length of the opposing players alive list, if smaller than min, return as move
  (Player == 'b' ->
    length(NewReds, RLen),
    (RLen < Min ->
      NewMin = RLen,
      NewSuggestedMove = X
    ;
      NewMin = Min,
      NewSuggestedMove = SuggestedMove
    )
  ;
    length(NewBlues, BLen),
    (BLen < Min ->

      NewMin = BLen,
      NewSuggestedMove = X
    ;
      NewMin = Min,
      NewSuggestedMove = SuggestedMove
    )
  ),
  bloodlust_move_helper(Xs, NewMin, NewSuggestedMove, Move, State, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% self_preservation strategy%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
    self_preservation_move(AliveBlues, AliveReds, Move, 'b'),
    alter_board(Move, AliveBlues, NewAliveBlues).

  self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
    self_preservation_move(AliveReds, AliveBlues, Move, 'r'),
    alter_board(Move, AliveReds, NewAliveReds).

  self_preservation_move(Alive, OtherPlayerAlive, Move, Player) :-

    % Find all the possible moves given a player colour and a current board state
    findall([A,B,MA,MB],(member([A,B], Alive),
                         neighbour_position(A,B,[MA,MB]),
                         \+member([MA,MB],Alive),
                         \+member([MA,MB],OtherPlayerAlive)),
            [P1|PossMoves]),

    (Player ==  'b' ->
     State = [Alive, OtherPlayerAlive]
    ;
     State = [OtherPlayerAlive, Alive]
    ),
    % Given a list of possible moves, run move and crank for each, return the lowest
    self_preservation_move_helper([P1|PossMoves], 0, P1, Move, State, Player).

  self_preservation_move_helper([X], Max, SuggestedMove, FinalMove, State, Player) :-
    % Make this move and run conways crank to analyse the resulting state
    move_and_crank(X, State, [NewBlues, NewReds], Player),
    % Check the length of the opposing players alive list, if smaller than min, return as move
    (Player == 'b' ->
      length(NewBlues, BLen),
      (BLen > Max->
        FinalMove = X
      ;
        FinalMove = SuggestedMove
      )
    ;
      length(NewReds, RLen),
      (RLen > Max ->
        FinalMove = X
      ;
        FinalMove = SuggestedMove
      )
    ).

  self_preservation_move_helper([X|Xs], Max, SuggestedMove, Move, State, Player) :-
    move_and_crank(X, State, [NewBlues, NewReds], Player),
    % Check the length of the opposing players alive list, if smaller than min, return as move
    (Player == 'b' ->
      length(NewBlues, BLen),
      (BLen > Max ->
        NewMax = BLen,
        NewSuggestedMove = X
      ;
        NewMax = Max,
        NewSuggestedMove = SuggestedMove
      )
    ;
      length(NewReds, RLen),
      (RLen > Max ->
        NewMax = RLen,
        NewSuggestedMove = X
      ;
        NewMax = Max,
        NewSuggestedMove = SuggestedMove
      )
    ),
    self_preservation_move_helper(Xs, NewMax, NewSuggestedMove, Move, State, Player).
