/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Gustav Hansson 
%    Student user id  : gushan-6 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
% :- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
:- ensure_loaded('rndBoard.pl').
:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(B, 1) :- initBoard(B).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
winner(State, Plyr) :-
	terminal(State),
	score(State, 1, S1),
	score(State, 2, S2),
	winner(S1, S2, Plyr).


winner(S1, S2, Plyr) :-
	S1 < S2, !,
	Plyr is 1.

winner(S1, S2, Plyr) :-
	S2 < S1,!,
	Plyr is 2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),
	score(State, 1, S1),
	score(State, 2, S2),
	S1 =:= S2, !.

score([], _, 0).
score([Row | Rows], Player, Score) :-
	score(Row, Player, S1),
	score(Rows, Player, S2),
	Score is S1 + S2.

score([Point | Row], Player, Score) :-
	Point = Player,
	score(Row, Player, S1),
	Score is 1 + S1.

score([Point| Row], Player, Score) :-
	Point \= Player,
	score(Row, Player, Score).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
	moves(1, State, M1),
	moves(2, State, M2),
	M1 == [n],
	M2 == [n].

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, State, MvList) :-
	setof([X, Y], genMoves(Plyr, State, [X,Y]), M),
	length(M, L),
	L \= 0,
	sort(0, @<, M, MvList).

moves(Plyr, State, [n]) :-
	findall([X, Y], genMoves(Plyr, State, [X,Y]), M),
	length(M, L),
	L =:= 0.

genMoves(Plyr, State, [X, Y]) :-
	get(State, [X, Y], Value),
	Value = '.',
	(((shouldFlip(Plyr, State, [X, Y], 1, 0));
	( shouldFlip(Plyr, State, [X, Y], -1, 0));
	( shouldFlip(Plyr, State, [X, Y], 0, 1));
	( shouldFlip(Plyr, State, [X, Y], 0, -1)))).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, n, State, State, NextPlyr) :- nextPlayer(Plyr, NextPlyr), !.

nextState(Plyr, Move, State, S8, NextPlyr) :-
	validmove(Plyr, State, Move), !,
	nextPlayer(Plyr, NextPlyr),
	set(State, S, Move, Plyr),

	flipControl(Plyr, S, Move, 1, 0, S1),
	flipControl(Plyr, S1, Move, -1, 0, S2),
	flipControl(Plyr, S2, Move, 0, 1, S3),
	flipControl(Plyr, S3, Move, 0, -1, S4),
	flipControl(Plyr, S4, Move, 1, 1, S5),
	flipControl(Plyr, S5, Move, 1, -1, S6),
	flipControl(Plyr, S6, Move, -1, 1, S7),
	flipControl(Plyr, S7, Move, -1, -1, S8).


nextPlayer(1,2).
nextPlayer(2,1).

flipControl(Plyr, State, [X, Y], XDiff, YDiff, NewState) :-
	shouldFlip(Plyr, State, [X, Y], XDiff, YDiff) -> 
		flip(Plyr, State, [X, Y], XDiff, YDiff, NewState);
	NewState = State.

shouldFlip(Plyr, State, [X,Y],  XDiff, YDiff) :-
	NX is X + XDiff,
	NY is Y + YDiff,
	NX2 is X + 2* XDiff,
	NY2 is Y + 2* YDiff,
	get(State, [NX,NY], V1),
	get(State, [NX2,NY2], V2),
	enemy(Plyr, V1),
	Plyr = V2.

shouldFlip(Plyr, State, [X,Y], XDiff, YDiff) :-
	NX is X + XDiff,
	NY is Y + YDiff,
	NX2 is X + 2* XDiff,
	NY2 is Y + 2* YDiff,
	get(State, [NX,NY], V1),
	get(State, [NX2,NY2], V2),
	enemy(Plyr, V1),
	enemy(Plyr, V2),
	shouldFlip(Plyr, State, [NX,NY], XDiff, YDiff).

flip(Plyr, State, [X,Y],  XDiff, YDiff, State) :-
	NX is X + XDiff,
	NY is Y + YDiff,
	get(State, [NX,NY], Value),
	Plyr == Value.

flip(Plyr, State, [X,Y], XDiff, YDiff, NewState) :-
	NX is X + XDiff,
	NY is Y + YDiff,
	get(State, [NX,NY], Value),
	enemy(Plyr, Value),
	set(State, TmpState, [NX, NY], Plyr),
	flip(Plyr, TmpState, [NX, NY], XDiff, YDiff, NewState).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
validmove(Plyr, State, Move) :-
	moves(Plyr, State, MvList),
	member(Move, MvList).


enemy(Plyr, Val) :-
	Val \= Plyr,
	Val \= '.'.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State, -100) :-
	winner(State, 1), !.

h(State, 100) :-
	winner(State, 2), !.

h(State, 0) :-
	tie(State), !.

h(State, Val) :-
	score(State, 1, S1),
	score(State, 2, S2),
	Val is S2 - S1.
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
