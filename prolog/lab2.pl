% Gustav Hansson


init([_], []).
init([Head | Tail], [Head | O]):-
    init(Tail, O).

generatePrefix([], _, []).
generatePrefix([Input | Tail], I, [[Size, I, J, [Input | Tail]] | XS]) :-
    init([Input | Tail], Init),
    sum([Input | Tail], Size),
    generatePrefix(Init, I, XS),
    length([Input | Tail], L),
    J is I + L - 1.


generateSublists([], _, []).
generateSublists([Input | Tail], I, [Prefix | Xs]) :-
    J is I+1,
    generatePrefix([Input | Tail], I, Prefix),
    generateSublists(Tail, J, Xs).

sum(List, Sum) :-
    sum_list(List, Sum).

sortSublists(Input, Output) :-
    sort(1, @=<, Input, Output).


smallestK(0, _, []).

smallestK(K, [List | Lists], [List | Xs]) :-
    L is K-1,
    smallestK(L, Lists, Xs).

formatOutput([S, I, J, X], Output) :-
    string_concat(S, "\t", S1),
    string_concat(S1, I, S2),
    string_concat(S2, "\t", S3),
    string_concat(S3, J, S4),
    string_concat(S4, "\t", S5),
    atomics_to_string(X, ",", SX),
    string_concat(S5, SX, S6),
    string_concat(S6, "\n", Output).

printLists([]).
printLists([X | XS]) :-
    formatOutput(X, Out),
    write(Out),
    printLists(XS).

testCase1Func(X, Y) :- Y is X*(-1)^X.
testCase1(15, Y) :-
    range(1, 101, Ints),
    maplist(testCase1Func, Ints, Y).

range(X, X, []).
range(S, E, [S|Ls]) :-
    S < E,
    NextS is S+1,
    range(NextS, E, Ls).

g(K) :-
    % testCase1(K, List),
    List = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],
    generateSublists(List, 1, X),
    append(X, Y),

    sortSublists(Y, SortedLists),
    smallestK(K, SortedLists, Out),

    write("Entire List": List),
    nl,
    write("K": K),
    nl,
    write("Size \t Start \t End \t List\n"),
    printLists(Out).

