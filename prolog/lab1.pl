% Gustav Hansson

action(state(robot(r1, Items), S, B, P),
    move(r2), 
    state(robot(r2, Items), S, B, P)) :-
        member(steel, Items).

action(state(robot(r2, Items), S, B, P),
    move(r1), 
    state(robot(r1, Items), S, B, P)) :-
        member(steel, Items).

action(state(robot(r1, Items), S, B, P),
    move(r3), 
    state(robot(r3, Items), S, B, P)) :-
        member(brass, Items).

action(state(robot(r3, Items), S, B, P),
    move(r1), 
    state(robot(r1, Items), S, B, P)) :-
        member(brass, Items).

action(state(robot(Pos, Items), S, B, P),
    pickup(Item),
    state(robot(Pos, [Item | Items]), NS, NB, NP)) :-
        member(item(Item, Pos), [S, B, P]),
        length(Items, L),
        L < 2,
        select(item(Item, _), [S, B, P], item(Item, -1), [NS, NB, NP]).  

action(state(robot(Pos, Items), S, B, P),
    drop(Item),
    state(robot(Pos, NewItems), NS, NB, NP)) :- 
        member(Item, Items),
        delete(Items, Item, NewItems),
        select(item(Item, _), [S, B, P], item(Item, Pos), [NS, NB, NP]).  

solverR(state(robot(r2, Items), _,_,_), _, [done | []]) :-
    member(package, Items).

solverR(State, N, [Action | Trace]) :-
    N > 0,
    action(State, Action, NewState),
    solverR(NewState, N-1, Trace).

solve(Trace):-
    BeginState = state(robot(r1, []), item(steel, r1), item(brass, r2), item(package, r3)),
    solverR(BeginState, 11, Trace).

solve(Trace, N):-
    BeginState = state(robot(r1, []), item(steel, r1), item(brass, r2), item(package, r3)),
    solverR(BeginState, N, Trace).