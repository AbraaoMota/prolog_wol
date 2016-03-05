%prolog


% Wrapper for function called in war_of_life
test_strategy(N, Strat1, Strat2) :-
  test_strategy_helper(N, N, Strat1, Strat2, 0, 0, 0, 0, 250, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Base case of test_strategy helper (1 game)
%
% 
% Args: (1)    - Variable number of games used to recurse on
%       (2)    - Initial Number of games played (used for statistics)
%       (3, 4) - Player 1 and 2 Strategies
%       (5)    - Counter keeping total moves in all the games
%       (6, 7) - Number of BlueWins and RedWins
%       (8, 9) - Highest/ Lowest number of moves in any game ran in this recursion
%       (10)   - Total time spent in games, measured in ms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Recursive case of test_strategy helper
%
% Args: See above in base case
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%
% Utilities           
% 
%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Performs a move and runs conways crank on it,
%
% Args: (1) - Move given to perform and analyse
%       (2) - A state of [B, R], corresponding to the states defined in war_of_life
%       (3) - The new state [NB, NR], after having executed move given on state given,
%             and called conways crank on it
%       (4) - The player making the move
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_and_crank(Move, [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds], Player) :-
    (Player == 'b' ->
      alter_board(Move, AliveBlues, IntAliveBlues),
      IntAliveReds = AliveReds
    ;
      alter_board(Move, AliveReds, IntAliveReds),
      IntAliveBlues = AliveBlues
    ),
    next_generation([IntAliveBlues,IntAliveReds], [NewAliveBlues,NewAliveReds]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Returns the other player given a player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
other_player('b', 'r').
other_player('r', 'b').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find all possible moves for a player given his 
% piece's state and the opponent's state, in
% that order
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_poss_move(Alive, OtherPlayerAlive, Moves) :-
  findall([A,B,MA,MB],(member([A,B], Alive),
                       neighbour_position(A,B,[MA,MB]),
                       \+member([MA,MB],Alive),
                       \+member([MA,MB],OtherPlayerAlive)),
          Moves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%         STRATEGIES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Bloodlust strategy             
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Wrappers for bloodlust depending on what player is called
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  bloodlust_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  bloodlust_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Wrapper for the move_helper. This method prepares a list of moves 
% and prepares the state to give to the helper
%
% Args: (1) - Current player's alive pieces
%       (2) - Opponents alive pieces
%       (3) - Final Move (Returned from below)
%       (4) - Current player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bloodlust_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves),
  Moves = [P1|_],

  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  bloodlust_move_helper(Moves, 65, P1, Move, State, Player).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run the actual bloodlust strategy
%
% Args: (1) - List of moves a player has available to him
%       (2) - The running minimum to compare against
%       (3) - Currently suggested Move
%       (4) - Final move - RETURNED
%       (5) - State the player is in when they make the move
%       (6) - Current player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bloodlust_move_helper([], _, SuggestedMove, FinalMove, _, _) :-
  FinalMove = SuggestedMove.

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
%
% Self_preservation strategy    
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%
% Wrappers
%
%%%%%%%%%%%%%%%
self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  self_preservation_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  self_preservation_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%
%
% Prepare the moveset and states as described in bloodlust
%
%%%%%%%%%%%%%%%
self_preservation_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves),
  Moves = [P1|_],

  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  self_preservation_move_helper(Moves, 0, P1, Move, State, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run the actual self_preservation strategy
%
% Args: (1) - List of moves a player has available to him
%       (2) - The running maximum to compare against
%       (3) - Currently suggested Move
%       (4) - Final move - RETURNED
%       (5) - State the player is in when they make the move
%       (6) - Current player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
self_preservation_move_helper([], _, SuggestedMove, FinalMove, _, _) :-
  FinalMove = SuggestedMove.

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
%
% Land Grab strategy    
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%
% Wrappers
%%%%%%%%%%%%
land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  land_grab_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  land_grab_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%
%
% Prepare moveset and states
%
%%%%%%%%%%%
land_grab_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves),
  Moves = [P1|_],
  (Player ==  'b' ->
    State = [Alive, OtherPlayerAlive]
  ;
    State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  land_grab_move_helper(Moves, 0, P1, Move, State, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run the actual landgrab strategy
%
% Args: (1) - List of moves a player has available to him
%       (2) - The running maximum to compare against
%       (3) - Currently suggested Move
%       (4) - Final move - RETURNED
%       (5) - State the player is in when they make the move
%       (6) - Current player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
land_grab_move_helper([], _, SuggestedMove, FinalMove, _, _) :-
  FinalMove = SuggestedMove.

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
%
% Minimax strategy    
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%
% Wrappers
%%%%%%%%%%%%
minimax('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
  minimax_move(AliveBlues, AliveReds, Move, 'b'),
  alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
  minimax_move(AliveReds, AliveBlues, Move, 'r'),
  alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%
%
% Prepare moveset and states
%
%%%%%%%%%%%
minimax_move(Alive, OtherPlayerAlive, Move, Player) :-

  % Find all the possible moves given a player colour and a current board state
  find_poss_move(Alive, OtherPlayerAlive, Moves),
  Moves = [M1|_],

  (Player ==  'b' ->
   State = [Alive, OtherPlayerAlive]
  ;
   State = [OtherPlayerAlive, Alive]
  ),

  % Given a list of possible moves, run move and crank for each, return the lowest
  minimax_move_helper(Moves, 0, State, Player, M1, Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Calculates the most damage an enemy move can do to us
%
% Args: (1) - Moveset that the enemy has available
%       (2) - State the enemy has to start from
%       (3) - Current player (not the enemy himself)
%       (4) - Currently running minimum, changes on loops
%       (5) - FinalMinimum (Returned to minimax_move_helper)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_enemy_min([], _, _, Min, NewMin) :-
  NewMin = Min. 

get_enemy_min([M1|Moves], State, Player, Min, FinalMin) :-
  other_player(Player, Opponent),
  move_and_crank(M1, State, [NewBlues, NewReds], Opponent),
  length(NewBlues, BLen),
  length(NewReds, RLen),
 
  % Minimise the initial player's strategy (NOT Maximise ours as if we had land grab)
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

  % Recurse
  get_enemy_min(Moves, State, Player, NewMin, FinalMin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run the actual minimax strategy
%
% Args: (1) - List of moves a player has available to him
%       (2) - The running maximum to compare against
%       (3) - Current state
%       (4) - Current Player
%       (5) - Currently Suggested move
%       (6) - Final move - RETURNED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
minimax_move_helper([], _, _, _, SuggestedMove, FinalMove) :-
  FinalMove = SuggestedMove.

minimax_move_helper([M1|RemMoves], Max, State, Player, SuggestedMove, FinalMove) :-

  move_and_crank(M1, State, [NewBlues, NewReds], Player),

  % Depending on who the current player is, prepares the state of the enemy
  % to be called for the find_poss_move function
  (Player == 'r' ->
    Alive = NewBlues,
    OtherPlayerAlive = NewReds
  ;
    Alive = NewReds,
    OtherPlayerAlive = NewBlues
  ),
 
  % Find the enemy's moveset given we have made move M1
  find_poss_move(Alive, OtherPlayerAlive, Moves),

  % Get the most damaging enemy move from this set
  get_enemy_min(Moves, [NewBlues, NewReds], Player, 65, Min),

  % If the new worst from this move is better than our previous worst, pick this new worst
  (Min > Max ->
    NewSuggestedMove = M1,
    NewMax = Min
  ;
    NewSuggestedMove = SuggestedMove,
    NewMax = Max
  ),

  % Recurse
  minimax_move_helper(RemMoves, NewMax, State, Player, NewSuggestedMove, FinalMove).

