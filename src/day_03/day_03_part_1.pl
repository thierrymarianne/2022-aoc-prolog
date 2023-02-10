% swipl -s ./src/day_03/day_03_part_1.pl -g 'find_solution(Solution).'
:- module(day_03_part_1, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

list_integers(Max, Acc, Ints) :-
    Max is 1,
    append(Acc, [], Ints).

list_integers(Max, Acc, Ints) :-
    Max > 1,
    N is Max - 1,
    append([N], Acc, N_Ints),
    list_integers(N, N_Ints, Ints).

in_compartment(Line, IntList, Compartment) :-
    string_to_list(Line, L),
    findall(Letter, (
        member(Index, IntList),
        nth1(Index, L, CharCode),
        string_codes(Letter, [CharCode])
    ), Compartment).

% a-z : 97-122
% A-Z : 65-90
what_priority(Letter, Priority) :-
    string_codes(Letter, [CharCode]),
    (
        CharCode =< 122,
        CharCode >= 97
    )
    ->
        Priority is CharCode - 97 + 1
    ;
        string_codes(Letter, [CharCode]),
        Priority is CharCode - 65 + 27
    .

read_until_next_whitespace_shows_up(Stream, PreviousSum, SummedPriorities) :-
    (
        not_eos(Stream) ->
            read_line_to_string(Stream, Line),
            format("Line is \"~s\"~n~n", [Line]),

            string_length(Line, Length),
            format("Line length is of ~d characters~n~n", [Length]),

            CompartmentCapacity is Length // 2,

            list_integers(CompartmentCapacity, [CompartmentCapacity], IntList),
            format("Integers list is \"~p\"~n~n", [IntList]),

            in_compartment(Line, IntList, FirstCompartment),
            format("Item in first compartment is \"~q\"~n~n", [FirstCompartment]),

            maplist(plus(CompartmentCapacity), IntList, SecondCompartmentIntList),
            format("Second compartment indices are \"~q\"~n~n", [SecondCompartmentIntList]),
            in_compartment(Line, SecondCompartmentIntList, SecondCompartment),
            format("Item in second compartment is \"~q\"~n~n", [SecondCompartment]),

            findall(
                Priority,
                (
                    member(Letter, SecondCompartment),
                    member(Letter, FirstCompartment),
                    what_priority(Letter, Priority)
                ),
                Priorities
            ),

            nth1(1, Priorities, CommonItemPriority),
            length(Priorities, HowManyItems),
            format("Found ~d items of priority ~d~n~n", [HowManyItems, CommonItemPriority]),

            format("Current priority sum is ~d~n~n", [PreviousSum + (CommonItemPriority)]),

            read_until_next_whitespace_shows_up(Stream, PreviousSum + (CommonItemPriority), SummedPriorities)
        ;
            SummedPriorities is PreviousSum,
            format('Summed priorities is ~d.~n~n', [SummedPriorities]),
            close(Stream),
            format('Closed stream.~n~n', []),
            halt
    ).

find_solution(Solution) :-
    working_directory(_d, _d),
    format("Current dir is ~q~n~n", _d),

    atomic_list_concat([_d, 'src/day_03/input.txt'], AbsoluteFilePath),

    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_next_whitespace_shows_up(Stream, 0, Solution).
