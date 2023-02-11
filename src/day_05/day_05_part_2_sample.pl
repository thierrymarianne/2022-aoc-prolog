% swipl -s ./src/day_05/day_05_part_1.pl -g 'find_solution(Solution).'
:- module(day_05_part_2_sample, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

nth_crate_item(N, Matches, Stacks, StackOut) :-
    Key is N * 2 - 1,
    get_dict(Key, Matches, Crate),

    string_length(Crate, L),
    (
        L > 0 ->
        (
            nth1(N, Stacks, Stack),
            append(Stack, [Crate], NthCol),
            StackOut = [NthCol]
        );
        nth1(N, Stacks, NthCol),
        StackOut = [NthCol]
    ).

arrangement(Matches, Stacks, StackOut) :-
    nth_crate_item(1, Matches, Stacks, FirstStack),
    nth_crate_item(2, Matches, Stacks, SecondStack),
    nth_crate_item(3, Matches, Stacks, ThirdStack),

    append([
        FirstStack,
        SecondStack,
        ThirdStack
    ], StackOut).

end_of_arrangement(Stream, Line) :-

    CratesIndicesPattern = "\s([1-9])\s(?:\s)?",
    atomic_list_concat([
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern
    ], AllIndicesPattern),

    re_matchsub(AllIndicesPattern, Line, Matches),

    dict_size(Matches, Size),
    Size > 0,

    % Consume empty line
    read_line_to_string(Stream, _).

parse_crates_arrangement(Stream, PreviousArrangement, Arrangement) :-
    (
        read_line_to_string(Stream, Line),
        % format("~s~n~n", Line),

        \+end_of_arrangement(Stream, Line),

        SingleCratePattern = "(?:\\[([A-Z])\\]|(\s\s\s))\s?",
        atomic_list_concat([
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern
        ], AllCratesPattern),

        re_matchsub(AllCratesPattern, Line, Matches),

        arrangement(Matches, PreviousArrangement, StackOut),
        parse_crates_arrangement(Stream, StackOut, Arrangement)
    );
    Arrangement = PreviousArrangement.
    % format('How are the crates arranged at first? ~q.~n~n', [Arrangement]).

move_crate([Crate|SourceStackRest], DestinationStack, PostMoveSourceStack, PostMoveDestinationStack) :-
    PostMoveSourceStack = SourceStackRest,
    append([Crate], DestinationStack, PostMoveDestinationStack).

replace_stack_at_index(Index, Replacement, [_|Rest], StacksOut) :-
    Index is 1,
    StacksOut = [Replacement|Rest].

replace_stack_at_index(Index, Replacement, [Head|Rest], StacksOut) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_stack_at_index(NextIndex, Replacement, Rest, Out),
    append([[Head], Out], StacksOut).

move_once(Matches, [DestIdx, DestArr], PreviousArrangement, PostMoveToDestinationArrangement) :-
        get_dict('source', Matches, SourceStack),
        number_string(SourceStackIndex, SourceStack),
        nth1(SourceStackIndex, PreviousArrangement, SourceStackFromArrangement),

        move_crate(SourceStackFromArrangement, DestArr, PostMoveSourceStack, PostMoveDestinationStack),

        replace_stack_at_index(SourceStackIndex, PostMoveSourceStack, PreviousArrangement, PostMoveFromSourceArrangement),
        replace_stack_at_index(DestIdx, PostMoveDestinationStack, PostMoveFromSourceArrangement, PostMoveToDestinationArrangement).

sublist(List, N, SL, R) :-
    length(List, L),
    N > L, !,
    SL = List,
    R = [].

sublist(L, N, SL, R) :-
    N is 0, !,
    SL = L,
    R = [].

sublist([X|L], N, SL, R) :-
    N is 1, !,
    SL = [X],
    R = L.

sublist([X|L1], N, SL, R) :-
    N > 1,
    N1 is N - 1,
    sublist(L1, N1, SL2, R),
    append([SL2, R], L1),
    append([[X], SL2], SL).

loop(HowManyTimes, Matches, PreviousArrangement, PostMoveToDestinationArrangement) :-
    (
%        % format('Matches ~q~n~n', [Matches]),
        get_dict('destination', Matches, DestinationStack),
        number_string(DestinationStackIndex, DestinationStack),
        nth1(DestinationStackIndex, PreviousArrangement, DestinationStackFromArrangement)
    ),
    HowManyTimes > 0 -> (
        move_once(Matches, [DestinationStackIndex, DestinationStackFromArrangement], PreviousArrangement, IntermediaryPostMoveToDestinationArrangement),
        loop(HowManyTimes - 1, Matches, IntermediaryPostMoveToDestinationArrangement, PostMoveToDestinationArrangement)
    );
    (
        get_dict('how_many_crates', Matches, CratesCount),
        number_string(CratesToMove, CratesCount),

        get_dict('destination', Matches, DstStck),
        number_string(DstStckIdx, DstStck),
        nth1(DstStckIdx, PreviousArrangement, NewDestinationStackFromArrangement),

        sublist(NewDestinationStackFromArrangement, CratesToMove, DestinationSubstack, R),
        reverse(DestinationSubstack, ReversedDestinationSubstack),

        append(ReversedDestinationSubstack, R, DestinationStack),

        replace_stack_at_index(DstStckIdx, DestinationStack, PreviousArrangement, PostMoveToDestinationArrangement)
    ).

list_head(L, Head) :-
    reverse(L, RL),
    last(RL, Head).

parse_moves(Stream, PreviousArrangement, Arrangement, CratesAtTheTop) :-
    (
        read_line_to_string(Stream, Line),
        \+end_of_arrangement(Stream, Line),

        MovePattern = "move (?<how_many_crates>[0-9]+) from (?<source>[0-9]+) to (?<destination>[0-9]+)\s?",
        re_matchsub(MovePattern, Line, Matches),

        get_dict('how_many_crates', Matches, HowManyCrates),
        number_string(TotalCratesToMove, HowManyCrates),

        loop(TotalCratesToMove, Matches, PreviousArrangement, ArrangementOut),
        parse_moves(Stream, ArrangementOut, _, CratesAtTheTop)
    ); (
        Arrangement = PreviousArrangement,
        maplist(list_head, Arrangement, TopCratesSeq),
        atomic_list_concat(TopCratesSeq, CratesAtTheTop)
    ).

read_until_eos(Stream, PreviousArrangement, Solution) :-
    not_eos(Stream) -> (
        \+ground(PreviousArrangement) -> (
            parse_crates_arrangement(Stream, [[], [], []], ArrangementOut),
            read_until_eos(Stream, ArrangementOut, Solution)
        )
        ;
        (
            parse_moves(Stream, PreviousArrangement, _, Solution),
            format('What crates end up at the top? ~q.~n~n', [Solution])
        ),
        close(Stream),
        halt
    ).

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_05/sample.txt'], AbsoluteFilePath),

    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_eos(Stream, _PreviousArrangement, Solution).
