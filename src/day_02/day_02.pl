% swipl -s ./day_02.pl
:- module(day_02, [day_02/1]).

:- use_module(library(readutil)).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).
    
print_choice_rock(Choice, Translation) :-
    (
        Choice = "A";
        Choice = "X"
    ),
    Translation = "Rock".

print_choice_paper(Choice, Translation) :-
    (
        Choice = "B";
        Choice = "Y"
    ),
    Translation = "Paper".

print_choice_scissors(Choice, Translation) :-
    (
        Choice = "C";
        Choice = "Z"
    ),
    Translation = "Scissors".

translate_choice(Choice, Translation) :-
    print_choice_rock(Choice, Translation);
    print_choice_paper(Choice, Translation);
    print_choice_scissors(Choice, Translation).

% Rock
how_many_points_for_rock(Choice, Points) :-
    (
        Choice = "A";
        Choice = "X";
        Choice = "Rock"
    ),
    Points is 1.

% Paper
how_many_points_for_paper(Choice, Points) :-
    (
        Choice = "B";
        Choice = "Y";
        Choice = "Paper"
    ),
    Points is 2.

% Scissors
how_many_points_for_scissors(Choice, Points) :-
    (
        Choice = "C";
        Choice = "Z";
        Choice = "Scissors"
    ),
    Points is 3.

how_many_points(Choice, Points) :-
    how_many_points_for_scissors(Choice, Points);
    how_many_points_for_paper(Choice, Points);
    how_many_points_for_rock(Choice, Points).

what_strategy_for_scissors(OpponentChoice, Tip, StrategicChoice) :-
    OpponentChoice = "Scissors",
    (
        (
            Tip = "X",
            StrategicChoice = "Paper"
        );
        (
            Tip = "Y",
            StrategicChoice = "Scissors"
        );
        (
            Tip = "Z",
            StrategicChoice = "Rock"
        )
    ).

what_strategy_for_rock(OpponentChoice, Tip, StrategicChoice) :-
    OpponentChoice = "Rock",
    (
        (
            Tip = "X",
            StrategicChoice = "Scissors"
        );
        (
            Tip = "Y",
            StrategicChoice = "Rock"
        );
        (
            Tip = "Z",
            StrategicChoice = "Paper"
        )
    ).

what_strategy_for_paper(OpponentChoice, Tip, StrategicChoice) :-
    OpponentChoice = "Paper",
    (
        (
            Tip = "X",
            StrategicChoice = "Rock"
        );
        (
            Tip = "Y",
            StrategicChoice = "Paper"
        );
        (
            Tip = "Z",
            StrategicChoice = "Scissors"
        )
    ).

what_strategy_for(OpponentChoice, Tip, StrategicChoice) :-
    what_strategy_for_paper(OpponentChoice, Tip, StrategicChoice);
    what_strategy_for_rock(OpponentChoice, Tip, StrategicChoice);
    what_strategy_for_scissors(OpponentChoice, Tip, StrategicChoice).

how_many_points_for_winner(PlayerAChoice, PlayerBChoice, Points) :-
    (
        PlayerAChoice = "Scissors",
        PlayerBChoice = "Paper"
    ;
        PlayerAChoice = "Rock",
        PlayerBChoice = "Scissors"
    ;
        PlayerAChoice = "Paper",
        PlayerBChoice = "Rock"
    ),
    Points is 6.

how_many_points_for_both(PlayerAChoice, PlayerBChoice, Points) :-
    (
        PlayerAChoice = "Scissors",
        PlayerBChoice = "Scissors"
    ;
        PlayerAChoice = "Rock",
        PlayerBChoice = "Rock"
    ;
        PlayerAChoice = "Paper",
        PlayerBChoice = "Paper"
    ),
    Points is 3.

how_many_points_for_loser(PlayerAChoice, PlayerBChoice, Points) :-
    (
        PlayerAChoice = "Paper",
        PlayerBChoice = "Scissors"
    ;
        PlayerAChoice = "Scissors",
        PlayerBChoice = "Rock"
    ;
        PlayerAChoice = "Rock",
        PlayerBChoice = "Paper"
    ),
    Points is 0.

how_many_points_given_match_outcome(PlayerAChoice, PlayerBChoice, Points) :-
    how_many_points_for_both(PlayerAChoice, PlayerBChoice, Points);
    how_many_points_for_loser(PlayerAChoice, PlayerBChoice, Points);
    how_many_points_for_winner(PlayerAChoice, PlayerBChoice, Points).

read_until_next_whitespace_shows_up(Stream, PreviousPoints, TotalPoints) :-
    (
        not_eos(Stream) ->
            read_line_to_codes(Stream, Line),
            re_matchsub("(.+) (.+)", Line, Matches),
            get_dict(1, Matches, OpponentChoice),
            how_many_points(OpponentChoice, OpponentPoints),

            translate_choice(OpponentChoice, OpponentChoiceTranslation),

            get_dict(2, Matches, Tip),
            what_strategy_for(OpponentChoiceTranslation, Tip, StrategicChoice),
            format("What strategic choice to make against ~s: ~s (~s)~n", [OpponentChoiceTranslation, StrategicChoice, Tip]),
            how_many_points(StrategicChoice, OurPoints),

            how_many_points_given_match_outcome(OpponentChoiceTranslation, StrategicChoice, OpponentsPointsForMatchOutcome),
            how_many_points_given_match_outcome(StrategicChoice, OpponentChoiceTranslation, OurPointsForMatchOutcome),

            format("Opponent choice: ~s (~d pts)~n", [OpponentChoiceTranslation, OpponentPoints]),
            format("Our choice ~s (~d pts)~n", [StrategicChoice, OurPoints]),
            format("Match outcome points (opponent): ~d pts~n", [OpponentsPointsForMatchOutcome]),
            format("Match outcome points (us): ~d pts~n", [OurPointsForMatchOutcome]),
            format("Total points so far for us: ~d pts~n~n", [PreviousPoints + OurPoints + OurPointsForMatchOutcome]),
            read_until_next_whitespace_shows_up(Stream, PreviousPoints + OurPoints + OurPointsForMatchOutcome, TotalPoints)
        ;
            TotalPoints is PreviousPoints,
            close(Stream),
            format('Closed stream.~n~n', []),
            halt
    ).

day_02(TotalPoints) :-
    working_directory(_d, _d),
    format("Current dir is ~q~n~n", _d),

    atomic_list_concat([_d, 'input.txt'], AbsoluteFilePath),
    
    ground(AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    format("~n~n[ Opening file ~q ]~n~n", [AbsoluteFilePath]),

    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    read_until_next_whitespace_shows_up(Stream, 0, TotalPoints).
