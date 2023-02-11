% swipl -s ./src/day_06/day_06_part_2_sample.pl -g 'find_solution(Solution).'
:- module(day_06_part_2_sample, [find_solution/1]).

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
    [Fourth|R4] = R3,
    [Fifth|R5] = R4,
    [Sixth|R6] = R5,
    [Seventh|R7] = R6,
    [Eighth|R8] = R7,
    [Ninth|R9] = R8,
    [Tenth|R10] = R9,
    [Eleventh|R11] = R10,
    [Twelfth|R12] = R11,
    [Thirtheen|R13] = R12,
    [Fourteenth|_] = R13,

    list_to_ord_set([
        First,
        Second,
        Third,
        Fourth,
        Fifth,
        Sixth,
        Seventh,
        Eighth,
        Ninth,
        Tenth,
        Eleventh,
        Twelfth,
        Thirtheen,
        Fourteenth
    ], UniqueCharacters),

    string_length(UniqueCharacters, Length),
    (
        Length is 14 ->
        FoundIt is 1;
        FoundIt is 0
    ).

peak_at_rest([Head|_], PeekedAtChar) :-
    codes_string(Head, PeekedAtChar).

next_sublist_until_marker([Head|Rest], Iter, Position) :-
    sublist([Head|Rest], 14, SL, _),

    found_marker(SL, FoundIt),
    (
        FoundIt is 0 ->
        next_sublist_until_marker(Rest, Iter + 1, Position);
        Position is 14 + Iter
    ).

read_until_eos(Stream, Sol, Solution) :-
    not_eos(Stream) ->
    (
        read_line_to_string(Stream, Line),
        string_to_list(Line, Input),
        next_sublist_until_marker(Input, 0, PrevSolution),
        read_until_eos(Stream, PrevSolution, Solution)
    );
    Solution is Sol,
    close(Stream),
    format('~nSolution is ~d~n~n', Solution),
    halt.

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_06/sample.txt'], AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),
    read_until_eos(Stream, _, Solution).