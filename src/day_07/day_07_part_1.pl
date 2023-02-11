% swipl -s ./src/day_07/day_07_part_1.pl -g 'find_solution(Solution).'
:- module(day_07_part_1, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

cd(Dir, IsOutermostDirectory) :-
    Dir = "/",
    IsOutermostDirectory is 1, !.

cd(_, IsOutermostDirectory) :-
    IsOutermostDirectory is 0.

change_directory(Line, Dir, HasParent) :-
    re_matchsub("cd (?<directory_name>\\/|[a-z]+)", Line, Matches),
    get_dict('directory_name', Matches, Dir),
    cd(Dir, HasParent).

list_directory(Line) :-
    Line = "$ ls".

execute_commands(Stream, Solution) :-
    not_eos(Stream) -> (
        read_line_to_string(Stream, Line),
        string_to_list(Line, Input),
        (
            change_directory(Line, Dir, HasParent),
            format('Dir is ~s~n~n', [Dir])
        );(
            list_directory(Line)
        ),
        execute_commands(Stream, Solution)
    );
    close(Stream),
    format('~nClosing stream~n~n'),
    halt.

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_07/input.txt'], AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),
    execute_commands(Stream, Solution).