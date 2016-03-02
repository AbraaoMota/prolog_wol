%prolog


test_strategy(N, Strat1, Strat2) :-
  test_strategy_helper(N, N, Strat1, Strat2, 0, 0, 0, 0, 250, 0).

% Base case of test_strategy helper (1 game)
test_strategy_helper(1, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest, TimeInGame) :-

  % Grab the game time whilst playing the game
  statistics(walltime, _),
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  statistics(walltime, [_, Y]),
  GameTime is Y,


  % Grab the total moves over all the games
  FinTotalMoves is TotalMoves + NumMoves,
  % Get total time spent in games
  FinGameTime is TimeInGame + GameTime,

  % Decide final number of wins for each player as well as draws
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

  % Decide if this last game had a higher number of moves than our previous highest
  % game
  ( NumMoves > Highest ->
    FinHighest is NumMoves
  ;
    FinHighest is Highest
  ),

  % Same logic, just for lowest number of moves
  ( NumMoves < Lowest ->
    FinLowest is NumMoves
  ;
    FinLowest is Lowest
  ),

  % Calculate average moves and average time
  AverageMoves is FinTotalMoves / InitN,
  AverageTime is FinGameTime / InitN,
  
  % Print results
  format("\n\nNumber of wins for player 1 (blue): ~w\n", [FinBlueWins]),
  format("Number of wins for player 2 (red): ~w\n", [FinRedWins]),
  format("Number of Draws: ~w\n", [Draws]),
  format("Longest non-exhaustive game: ~w\n", [FinHighest]),
  format("Quickest game: ~w\n", [FinLowest]),
  format("Average Moves per game: ~w\n", [AverageMoves]),
  format("Average Time per game: ~w\n\n", [AverageTime]).


% Recursive case for test_strategy_helper (>1 games to play)
test_strategy_helper(N, InitN, Strat1, Strat2, TotalMoves, BlueWins, RedWins, Highest, Lowest, TimeInGame) :-
  
  % Grab game time statistics and play the game
  statistics(walltime, _),
  play(quiet, Strat1, Strat2, NumMoves, WinningPlayer),
  statistics(walltime, [_, Y]),
  GameTime is Y,

  % Update the total time and moves in games
  NewTotalTime is TimeInGame + GameTime,
  NewTotalMoves is TotalMoves + NumMoves,


  % Update wins for each player (draws calculated only at the base case)
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

  % Calculate if this game had a new highest record of moves
  ( NumMoves > Highest ->
    NewHighest is NumMoves
  ;
    NewHighest is Highest
  ),

  % Same logic, check for lowest record of moves
  ( NumMoves < Lowest ->
    NewLowest is NumMoves
  ;
    NewLowest is Lowest
  ),

  % Call the function recursively, with 1 less game to play
  NewN is N - 1,
  test_strategy_helper(NewN, InitN, Strat1, Strat2, NewTotalMoves, NewBlueWins, NewRedWins, NewHighest, NewLowest, NewTotalTime).


%%%%%%%%%%%%%%%%%%%%%%%
% Utilities           %
%%%%%%%%%%%%%%%%%%%%%%%

% Performs a move and runs conways crank on it,
% Returns a new state when given a move, a state and a player
move_and_crank(Move, [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds], Player) :-
    (Player == 'b' ->
      alter_board(Move, AliveBlues, IntAliveBlues),
      IntAliveReds = AliveReds
    ;
      alter_board(Move, AliveReds, IntAliveReds),
      IntAliveBlues = AliveBlues
    ),
    next_generation([IntAliveBlues,IntAliveReds], [NewAliveBlues,NewAliveReds]).


% Returns the other player given a player
other_player('b') :- 'r'.
other_player('r') :- 'b'.


% Find all possible moves for a player with a corresponding set of Alive pieces. Requires
% also the set of OtherPlayerAlive pieces for the other player. Returns also the 
% FirstMove of the Moves set, which is useful in being the first suggestedMove in
% strategies later
find_poss_move(Alive, OtherPlayerAlive, Moves, FirstMove) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                       neighbour_position(A,B,[MA,MB]),
                       \+member([MA,MB],Alive),
                       \+member([MA,MB],OtherPlayerAlive)),
          [FirstMove|PossMoves]),
          Moves = [FirstMove|PossMoves].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bloodlust strategy             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  bloodlust_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  bloodlust_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

bloodlust_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves, P1),

  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  bloodlust_move_helper(Moves, 65, P1, Move, State, Player).

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
  
  % Make this move and run conways crank to analyse the resulting state
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

  % Recursively call the function with one less move to consider
  bloodlust_move_helper(Xs, NewMin, NewSuggestedMove, Move, State, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% self_preservation strategy     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  self_preservation_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  self_preservation_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

self_preservation_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves, P1),

  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  self_preservation_move_helper(Moves, 0, P1, Move, State, Player).

self_preservation_move_helper([X], Max, SuggestedMove, FinalMove, State, Player) :-
  
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),

  % Check the length of the players alive list, if greater than max, return as move
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
  
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),

  % Check the length of the player's alive list, if greater than max, return as move
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

  % Recursively call the self_preservation function
  self_preservation_move_helper(Xs, NewMax, NewSuggestedMove, Move, State, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% land_grab strategy             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  land_grab_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  land_grab_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

land_grab_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves, P1),
  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  land_grab_move_helper(Moves, 0, P1, Move, State, Player).

land_grab_move_helper([X], Max, SuggestedMove, FinalMove, State, Player) :-
  
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),

  % Check the length of the opposing players alive list, if smaller than min, return as move
  length(NewBlues, BLen),
  length(NewReds, RLen),
  (Player == 'b' ->
    (BLen - RLen > Max->
      FinalMove = X
    ;
      FinalMove = SuggestedMove
    )
  ;
    (RLen - BLen > Max ->
      FinalMove = X
    ;
      FinalMove = SuggestedMove
    )
  ).

land_grab_move_helper([X|Xs], Max, SuggestedMove, Move, State, Player) :-
  
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),

  % Check the length of the opposing players alive list, if smaller than min, return as move
  length(NewBlues, BLen),
  length(NewReds, RLen),
  (Player == 'b' ->
    (BLen - RLen > Max ->
      NewMax is BLen - RLen,
      NewSuggestedMove = X
    ;
      NewMax = Max,
      NewSuggestedMove = SuggestedMove
    )
  ;
    (RLen - BLen > Max ->
      NewMax is RLen - BLen,
      NewSuggestedMove = X
    ;
      NewMax = Max,
      NewSuggestedMove = SuggestedMove
    )
  ),

  % Recursively call the function with one less move
  land_grab_move_helper(Xs, NewMax, NewSuggestedMove, Move, State, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% minimax strategy               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minimax('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  minimax_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  minimax_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

minimax_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves, P1),

  (Player ==  'b' ->
   State = [Alive, OtherPlayerAlive]
  ;
   State = [OtherPlayerAlive, Alive]
  ),
  % Given a list of possible moves, run move and crank for each, return the lowest
  minimax_move_helper(Moves, 0, P1, Move, State, Player).



% Given a list of moves (of the enemy), 
% we minimise our utility because 
% we assume they are smart and thus 
% they'll make the worst move for us (based on 
% our own heuristic)

get_enemy_min([M1], Min, State, Player, FinalMin) :-
  
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(M1, State, [NewBlues, NewReds], other_player(Player)),
  length(NewBlues, BLen),
  length(NewReds, RLen),

  % The enemy wants to minimise the utility function for the current player
  (Player == 'b' ->
    (BLen - RLen < Min ->
      FinalMin = BLen - RLen
    ;
      FinalMin = Min
    )
  ;
    (RLen - BLen < Min ->
      FinalMin = RLen - BLen
    ;  
      FinalMin = Min
    )
  ).

get_enemy_min([M1|Moves], Min, State, Player, FinalMin) :-

  move_and_crank(M1, State, [NewBlues, NewReds], other_player(Player)),
  length(NewBlues, BLen),
  length(NewReds, RLen),

  % You want to minimise utility for the current player
  (Player == 'b' ->
    (BLen - RLen < Min ->
      NewMin is BLen - RLen
    ;
      NewMin = Min
    )
  ;
    (RLen - BLen < Min ->
      NewMin is RLen - BLen
    ;
      NewMin = Min
    )
  ),
  get_enemy_min(Moves, NewMin, State, Player, FinalMin).

minimax_move_helper([X], Max, SuggestedMove, FinalMove, State, Player) :-
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),
  % First compute the enemies possible moveset under the new state
  (Player == 'r' ->
    Alive = NewBlues,
    OtherPlayerAlive = NewReds
  ;
    Alive = NewReds,
    OtherPlayerAlive = NewBlues
  ),
  
  find_poss_move(Alive, OtherPlayerAlive, EnemyPossMoves, _),
  
  % Then give this set to get_enemy_move
  get_enemy_min(EnemyPossMoves, 65, [NewBlues, NewReds], Player, EnemyMin),
  % We now have the Enemy Move to be made given we have made move X
  % We now compare the returned EnemyMin with our current Max
  % If enemyMin is larger, we want to choose move X because it 
  % gives us a higher lowest possible outcome
  % Otherwise, ignore it and keep suggested move from before
  (EnemyMin > Max ->
    FinalMove = X
  ;
    FinalMove = SuggestedMove
  ).

minimax_move_helper([X|Xs], Max, SuggestedMove, Move, State, Player) :-
  % Make this move and run conways crank to analyse the resulting state
  move_and_crank(X, State, [NewBlues, NewReds], Player),
  
  % We now get the worst enemy move resulting from this move and crank
  % First compute the enemies possible moveset
  (Player == 'r' ->
    Alive = NewBlues,
    OtherPlayerAlive = NewReds
  ;
    Alive = NewReds,
    OtherPlayerAlive = NewBlues
  ),

  find_poss_move(Alive, OtherPlayerAlive, EnemyPossMoves, _),
  

  % Then give this set to get_enemy_move
  get_enemy_min(EnemyPossMoves, 65, [NewBlues, NewReds], Player, EnemyMin),
  % We now have the Enemy Move to be made given we have made move X
  % We now compare the returned EnemyMin with our current Max
  % If enemyMin is larger, we want to choose move X because it 
  % gives us a higher lowest possible outcome
  % Otherwise, ignore it and keep suggestedmove from before
  (EnemyMin > Max ->
    NewSuggestedMove = X,
    NewMax = EnemyMin
  ;
    NewSuggestedMove = SuggestedMove,
    NewMax = Max
  ),
  minimax_move_helper(Xs, NewMax, NewSuggestedMove, Move, State, Player).



