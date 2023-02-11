% swipl -s ./src/day_06/day_06_part_1_sample.pl -g 'find_solution(Solution).'
:- module(day_06_part_1_sample, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

sublist(List, N, SL, _) :-
    length(List, L),
    N > L, !,
    SL = List.

sublist(L, N, SL, _) :-
    N is 0, !,
    SL = L.

sublist([X|_], N, SL, _) :-
    N is 1, !,
    SL = [X].

sublist([X|L1], N, SL, R) :-
    N > 1,
    N1 is N - 1,
    sublist(L1, N1, SL2, _),
    append([SL2, R], L1),
    append([[X], SL2], SL).

codes_string(Codes, String) :-
    string_codes(String, [Codes]).

found_marker(SL, FoundIt) :-
    [First|R1] = SL,
    [Second|R2] = R1,
    [Third|R3] = R2,
    [Fourth|_] = R3,

    list_to_ord_set([First, Second, Third, Fourth], UniqueCharacters),
    maplist(codes_string, [First, Second, Third, Fourth], Substring),
    format("Sublist: ~q~n~n", [Substring]),

    string_length(UniqueCharacters, Length),
    %format("How many unique characters? ~q~n", [Length]),
    (
        Length is 4 ->
        FoundIt is 1;
        FoundIt is 0
    ).

peak_at_rest([Head|_], PeekedAtChar) :-
    codes_string(Head, PeekedAtChar).

next_sublist_until_marker([Head|Rest], Iter, Position) :-
    sublist([Head|Rest], 4, SL, R),

    maplist(codes_string, [Head|Rest], OriginalList),
    maplist(codes_string, R, RSubstring),
    maplist(codes_string, SL, SLSubstring),

    format("Liste: ~q~n", [OriginalList]),
    format("Sous-liste: ~q~n", [SLSubstring]),
    format("Reste: ~q~n", [RSubstring]),

    peak_at_rest(R, PeekedAtChar),
    format("Peeked At Char: ~s~n", [PeekedAtChar]),

    found_marker(SL, FoundIt),
    (
        FoundIt is 0 ->
        next_sublist_until_marker(Rest, Iter + 1, Position);
        Position is 4 + Iter
    ).

read_until_eos(Stream, Sol, Solution) :-
    not_eos(Stream) ->
    (
        read_line_to_string(Stream, Line),
        string_to_list(Line, Input),
        next_sublist_until_marker(Input, 0, PrevSolution),
        read_until_eos(Stream, PrevSolution, Solution),
        format("Solution is ~q~n~n", [PrevSolution])
    );
    Solution is Sol,
    format('Solution is ~d~n', [Solution]),
    close(Stream).

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_06/sample.txt'], AbsoluteFilePath),

    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_eos(Stream, _, Solution).
