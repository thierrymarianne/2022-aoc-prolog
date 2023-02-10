% swipl -s ./src/day_04/day_04_part_1.pl -g 'find_solution(Solution).'
:- module(day_04_part_1, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

is_there_overlap(section(FirstRangeStart, FirstRangeEnd), section(SecondRangeStart, SecondRangeEnd), DoesOneRangeOverlapTheOther) :-
    (
        (
                FirstRangeStart >= SecondRangeStart,
                FirstRangeEnd =< SecondRangeEnd
        );
        (
                SecondRangeStart >= FirstRangeStart,
                SecondRangeEnd =< FirstRangeEnd
        )
    ) ->
        DoesOneRangeOverlapTheOther is 1
    ;
        DoesOneRangeOverlapTheOther is 0
    .

read_until_eos(Stream, Overlaps, HowManyOverlaps) :-
    (
        not_eos(Stream) ->
            read_line_to_string(Stream, Line),

            re_matchsub("([^\\-]*)-([^\\-,]*),([^\\-,]*)-([^\\-]*)", Line, Matches),

            get_dict(1, Matches, FirstPairStart),
            get_dict(2, Matches, FirstPairEnd),
            get_dict(3, Matches, SecondPairStart),
            get_dict(4, Matches, SecondPairEnd),

            number_string(FirstSectionsStart, FirstPairStart),
            number_string(FirstSectionsEnd, FirstPairEnd),

            number_string(SecondSectionsStart, SecondPairStart),
            number_string(SecondSectionsEnd, SecondPairEnd),

            is_there_overlap(
                section(FirstSectionsStart, FirstSectionsEnd),
                section(SecondSectionsStart, SecondSectionsEnd),
                OneMoreOverlapOrNot
            ),
            % format("Is there overlap? ~p~n~n", OneMoreOverlapOrNot),

            read_until_eos(Stream, Overlaps + OneMoreOverlapOrNot, HowManyOverlaps)
        ;
            HowManyOverlaps is Overlaps,
            format('How many sections are overlapping? ~q.~n~n', [HowManyOverlaps]),
            close(Stream),
            format('Closed stream.~n~n', []),
            halt
    ).

find_solution(Solution) :-
    working_directory(_d, _d),
    format("Current dir is ~q~n~n", _d),

    atomic_list_concat([_d, 'src/day_04/input.txt'], AbsoluteFilePath),

    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_eos(Stream, 0, Solution).
