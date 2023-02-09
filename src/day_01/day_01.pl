% consult('./day_01').
% read_file('./input.txt', [A, B, C]).
:- module(day_01,[read_file/2]).

:- use_module(engine(basic_props)).
:- use_module(engine(messages_basic)).
:- use_module(library(assertions/native_props)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(numlists)).
:- use_module(library(stream_utils)).
:- use_module(library(terms_io)).
:- use_module(library(terms)).
:- use_module(library(strings)).
:- use_module(library(when)).
:- use_module(library(sort)).

%:- use_package(debug).
%:- use_package(trace).

not_eos(Stream) :-
    peek_byte(Stream, Byte),
    Byte =\= -1.

read_until_next_whitespace_shows_up(Stream, InitialValue, Sums, PreviousSum, Sum, _Top3) :-
    peek_code(Stream, Code),
    Code =:= 10,
    get_line(Stream, Line),
    length(Line, _Length),
    _Length =:= 0,

    if(
        Sum > PreviousSum,
        (
            _NextSum is Sum,
            _NextMax is PreviousSum
        ),
        (
            _NextSum is PreviousSum,
            _NextMax is Sum
        )
    ),

    append(Sums, [_NextMax], L),
    read_until_next_whitespace_shows_up(Stream, InitialValue, L, InitialValue, _NextSum, _Top3).

read_until_next_whitespace_shows_up(
    Stream,
    InitialValue,
    Sums,
    PreviousSum,
    MostCalories,
    Top3
) :-
    if(
        not_eos(Stream),
        (
            get_line(Stream, Line),
            length(Line, _Length),
            number_codes(_Calories, Line),
            _Length > 0,
            read_until_next_whitespace_shows_up(
                Stream,
                InitialValue,
                Sums,
                +(PreviousSum, _Calories),
                MostCalories,
                Top3
            )
        ),
        (
            _MostCalories is MostCalories,
            append(Sums, [MostCalories], _Sums),
            sort(_Sums, SortedSums),
            reverse(SortedSums, RevSortedSums),

            nth(1, RevSortedSums, _First),
            nth(2, RevSortedSums, _Second),
            nth(3, RevSortedSums, _Third),

            Top3 = [_First, _Second, _Third],

            sum_list([_First, _Second, _Third], Sum),
            format('The maximum amount of calories carried by the 3 topmost elves are ~d.~n~n', [Sum]),

            close(Stream),
            format('Closed stream.~n~n', [])
        )
    ).

read_file(File, [A, B, C]) :-
    gnd(File),
    file_exists(File),
    format("~n~n[ Opening file ~q ]~n~n", ['./input.txt']),

    open(File, read, _Stream),

    read_until_next_whitespace_shows_up(
        _Stream,
        0,
        [],
        0,
        0,
        [A, B, C|_]
    ).
