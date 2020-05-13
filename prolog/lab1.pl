% Gustav Hansson

action(state(robot(r1, OwnedItems), Items),
    move(r2), 
    state(robot(r2, OwnedItems), Items)) :-
        member(steel, OwnedItems).

action(state(robot(r2, OwnedItems), Items),
    move(r1), 
    state(robot(r1, OwnedItems), Items)) :-
        member(steel, OwnedItems).

action(state(robot(r1, OwnedItems), Items),
    move(r3), 
    state(robot(r3, OwnedItems), Items)) :-
        member(brass, OwnedItems).

action(state(robot(r3, OwnedItems), Items),
    move(r1), 
    state(robot(r1, OwnedItems), Items)) :-
        member(brass, OwnedItems).

action(state(robot(Pos, OwnedItems), Items),
    pickup(Item),
    state(robot(Pos, [Item | OwnedItems]), UpdatedItems)) :-
        member(item(Item, Pos), Items),                                 % if the item exists in the current room
        length(OwnedItems, L),                                          % check so that the robot only carries a max of 2 items
        L < 2,
        select(item(Item, _), Items, item(Item, -1), UpdatedItems).     %  update the position of the item to be outside any room

action(state(robot(Pos, OwnedItems), Items),
    drop(Item),
    state(robot(Pos, NewItems), UpdatedItems)) :- 
        member(Item, OwnedItems),                                       % check so that the item that wants to be dropped is owned
        delete(OwnedItems, Item, NewItems),                             % delete the item from the owned items and give a new list of owned items
        select(item(Item, _), Items, item(Item, Pos), UpdatedItems).    % update the position of the dropped item to be the current position

solverR(state(robot(r2, OwnedItems), _), _, [done | []]) :-
    member(package, OwnedItems).

solverR(State, N, [Action | Trace]) :-
    N > 0,
    M is N-1,
    action(State, Action, NewState),
    solverR(NewState, M, Trace).

solve(Trace):-
    BeginState = state(
        robot(r1, []),
        [item(steel, r1),
        item(brass, r2),
        item(package, r3)]),

    solverR(BeginState, 11, Trace).

solve(Trace, N):-
    BeginState = state(
        robot(r1, []),
        [item(steel, r1),
        item(brass, r2),
        item(package, r3)]),
    solverR(BeginState, N, Trace).