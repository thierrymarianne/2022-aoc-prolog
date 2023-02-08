% consult('./day_01').
% read_file('./input.txt').
:- module(day_01,[read_file/1]).

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

%:- use_package(debug).
%:- use_package(trace).

not_eos(Stream) :-
    peek_byte(Stream, Byte),
    Byte =\= -1.

read_until_next_whitespace_shows_up(Stream, InitialValue, PreviousSum, Sum) :-
    peek_code(Stream, Code),
    Code =:= 10,
    get_line(Stream, Line),
    length(Line, _Length),
    _Length =:= 0,
    if(
        Sum > PreviousSum,
        read_until_next_whitespace_shows_up(Stream, InitialValue, InitialValue, Sum),
        read_until_next_whitespace_shows_up(Stream, InitialValue, InitialValue, PreviousSum)
    ).

read_until_next_whitespace_shows_up(Stream, InitialValue, PreviousSum, MostCalories) :-
    if(
        not_eos(Stream),
        (
            get_line(Stream, Line),
            length(Line, _Length),
            number_codes(_Calories, Line),
            _Length > 0,
%            format('Next food item contains ~s calories.~n', [Line]),
            read_until_next_whitespace_shows_up(
                Stream,
                InitialValue,
                +(PreviousSum, _Calories),
                MostCalories
            )
        ),
        format('The maximum amount of calories carried by a single elf is ~d.~n~n', [MostCalories])
    ).

read_file(File) :-
    gnd(File),
    file_exists(File),
    format("~n~n[ Opening file ~q ]~n~n", ['./input.txt']),
    open(File, read, _Stream),
    read_until_next_whitespace_shows_up(_Stream, 0, 0, 0).

%
% Where to find usage examples?
% What are the differences between a module and a package ("hiord" package)?
% Is [repeat/2 documentation](https://ciao-lang.org/ciao/build/doc/ciao.html/ciaosearch.html#repeat/2) correct?
% Where can I find some hosted version of [PiLLoW's Web programming libraries](http://cliplab.org/logalg/slides/C_pillow/C_pillow.pdf)?
%