% swipl -s ./src/day_04/day_04_part_1.pl -g 'find_solution(Solution).'
:- module(day_04_part_1, [find_solution/1]).

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
%    format("Stack out: ~p~n", [StackOut]).

arrangement(Matches, Stacks, StackOut) :-
    nth_crate_item(1, Matches, Stacks, FirstStack),
    nth_crate_item(2, Matches, Stacks, SecondStack),
    nth_crate_item(3, Matches, Stacks, ThirdStack),
    nth_crate_item(4, Matches, Stacks, FourthStack),
    nth_crate_item(5, Matches, Stacks, FifthStack),
    nth_crate_item(6, Matches, Stacks, SixthStack),
    nth_crate_item(7, Matches, Stacks, SeventhStack),
    nth_crate_item(8, Matches, Stacks, EighthStack),
    nth_crate_item(9, Matches, Stacks, NinthStack),

    append([
        FirstStack,
        SecondStack,
        ThirdStack,
        FourthStack,
        FifthStack,
        SixthStack,
        SeventhStack,
        EighthStack,
        NinthStack
    ], StackOut).

end_of_arrangement(Stream, Line) :-

    CratesIndicesPattern = "\s([1-9])\s(?:\s)?",
    atomic_list_concat([
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern,
        CratesIndicesPattern
    ], AllIndicesPattern),

    re_matchsub(AllIndicesPattern, Line, Matches),

    dict_size(Matches, Size),
    Size > 0,

    format("Consuming empty line~n~n", []),
    % Consume empty line
    read_line_to_string(Stream, _).

parse_crates_arrangement(Stream, PreviousArrangement, Arrangement) :-
    (
        read_line_to_string(Stream, Line),
        format("~s~n~n", Line),

        \+end_of_arrangement(Stream, Line),

        SingleCratePattern = "(?:\\[([A-Z])\\]|(\s\s\s))\s?",
        atomic_list_concat([
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern,
            SingleCratePattern
        ], AllCratesPattern),

        re_matchsub(AllCratesPattern, Line, Matches),

        arrangement(Matches, PreviousArrangement, StackOut),
        parse_crates_arrangement(Stream, StackOut, Arrangement)
    );
    Arrangement = PreviousArrangement,
    format('How are the crates arranged at first? ~q.~n~n', [Arrangement]).

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

move_once(Matches, PreviousArrangement, PostMoveToDestinationArrangement) :-
%        get_dict('how_many_crates', Matches, HowManyCrates),
%        number_string(TotalCratesToMove, HowManyCrates),

        get_dict('source', Matches, SourceStack),
        number_string(SourceStackIndex, SourceStack),
        % format("Source stack index is ~d~n", SourceStackIndex),

        get_dict('destination', Matches, DestinationStack),
        number_string(DestinationStackIndex, DestinationStack),
        % format("Destination stack index is ~d~n~n", DestinationStackIndex),

        %format("Moving ~d from #~d stack to #~d stack~n", [TotalCratesToMove, SourceStackIndex, DestinationStackIndex]),

        nth1(SourceStackIndex, PreviousArrangement, SourceStackFromArrangement),
        % format("Source stack is ~q~n~n", [SourceStackFromArrangement]),

        nth1(DestinationStackIndex, PreviousArrangement, DestinationStackFromArrangement),
        % format("Destination stack is ~q~n~n", [DestinationStackFromArrangement]),

        move_crate(SourceStackFromArrangement, DestinationStackFromArrangement, PostMoveSourceStack, PostMoveDestinationStack),
        % format("Post-move source stack: ~q~n~n", [PostMoveSourceStack]),
        % format("Post-move destination stack: ~q~n~n", [PostMoveDestinationStack]),

        replace_stack_at_index(SourceStackIndex, PostMoveSourceStack, PreviousArrangement, PostMoveFromSourceArrangement),
        replace_stack_at_index(DestinationStackIndex, PostMoveDestinationStack, PostMoveFromSourceArrangement, PostMoveToDestinationArrangement).
        %format("New arrangement: ~q~n~n", [PostMoveToDestinationArrangement]).

loop(HowManyTimes, Matches, PreviousArrangement, PostMoveToDestinationArrangement) :-
    HowManyTimes > 0 -> (
        move_once(Matches, PreviousArrangement, IntermediaryPostMoveToDestinationArrangement),
        loop(HowManyTimes - 1, Matches, IntermediaryPostMoveToDestinationArrangement, PostMoveToDestinationArrangement)
    );
    PostMoveToDestinationArrangement = PreviousArrangement.

list_head(L, Head) :-
    reverse(L, RL),
    last(RL, Head).
%        maplist(list_head(L), Arrangement, TopCrates),
%        format('What is the latest crates arrangement? ~q.~n~n', [TopCrates])
parse_moves(Stream, PreviousArrangement, Arrangement, CratesAtTheTop) :-
    (
        read_line_to_string(Stream, Line),
%        format("~s~n~n", Line),
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
        atomic_list_concat(TopCratesSeq, CratesAtTheTop),
        format('What are the top crates? ~s.~n~n', [CratesAtTheTop]),
        format('What is the latest crates arrangement? ~q.~n~n', [Arrangement])
    ).

read_until_eos(Stream, PreviousArrangement, Solution) :-
    not_eos(Stream) -> (
        \+ground(PreviousArrangement) -> (
            parse_crates_arrangement(Stream, [[], [], [], [], [], [], [], [], []], ArrangementOut),
            read_until_eos(Stream, ArrangementOut, Solution)
        )
        ;
        (
            parse_moves(Stream, PreviousArrangement, _, Solution),
            format('What crates end up at the top? ~q.~n~n', [Solution])
        ),
        close(Stream),
        format('Closed stream.~n~n', []),
        halt
    ).

find_solution(Solution) :-
    working_directory(_d, _d),
    format("~n[ Current dir is ~q ]", _d),

    atomic_list_concat([_d, 'src/day_05/input.txt'], AbsoluteFilePath),

    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_eos(Stream, _PreviousArrangement, Solution).
