% swipl -s ./src/day_07/day_07_part_1.pl -g 'find_solution(Solution).'
:- module(day_07_part_1, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

change_directory(Line, Path, NewPath, FilesIn, FilesOut) :-
    re_matchsub("\\$\\scd\\s(?<directory_name>\\.\\.|\\/|[a-z]+)", Line, Matches),
    get_dict('directory_name', Matches, Dir),
    (
        Dir = ".." -> (
            FilesOut = FilesIn,
            [_|[ParentDir|R]] = Path,
            NewPath = [ParentDir|R]
        );(
            (
                Dir = "/" ->
                string_to_atom("root", DirKey);
                string_to_atom(Dir, DirKey)
            ),
            put_dict(DirKey, FilesIn, 0, FilesOut),
            append([Dir], Path, NewPath)
        )
    ).

directory(Line, [ParentDir|_], FsBeforeListing, FsAfterListing, FilesIn, FilesOut) :-
    re_matchsub("dir\\s(?<directory_name>\\/|[a-z]+)", Line, Matches),
    get_dict('directory_name', Matches, Dir),
    string_to_atom(Dir, DirAtom),

    (
        ParentDir = "/" ->
        string_to_atom("root", ParentDirKey);
        string_to_atom(ParentDir, ParentDirKey)
    ),
    (
        (
            get_dict(ParentDirKey, FsBeforeListing, DescendantDirectoriesIn),
            ground(DescendantDirectoriesIn)
            -> append(DescendantDirectoriesIn, [DirAtom], DescendantDirectories);
            DescendantDirectories = [DirAtom]
        ),
        string_to_atom(Dir, DirKey),
        put_dict(DirKey, FilesIn, 0, FilesOut),
        put_dict(ParentDirKey, FsBeforeListing, DescendantDirectories, FsAfterListing)
    ).

file_size(Line, Filesize, _) :-
    re_matchsub("(?<filesize>[0-9]+)\\s(?<filename>[\\.a-z]+)", Line, Matches),
    get_dict('filesize', Matches, FilesizeCodes),
    number_codes(Filesize, FilesizeCodes).

list_files_in_directory(Line, ParentDirs, FsBeforeListing, NextFsAfterListing, FilesIn, FilesOut) :-
    (
        Command = "$ ls",
        Line = Command,
        NextFsAfterListing = FsBeforeListing,
        FilesOut = FilesIn
    );
    (
        NextFsAfterListing = FsBeforeListing,
        file_size(Line, Filesize, ParentDirs),
        ParentDirs = [WorkingDirectory|_],
        (
            WorkingDirectory = "/" ->
            string_to_atom("root", DirectoryKey);
            string_to_atom(WorkingDirectory, DirectoryKey)
        ),
        (
            (
                get_dict(DirectoryKey, FilesIn, FilesizeIn),
                ground(FilesizeIn)
                -> NewDirectorySize = FilesizeIn + Filesize;
                NewDirectorySize = Filesize
            ),
            put_dict(DirectoryKey, FilesIn, NewDirectorySize, FilesOut)
        )
    );
    directory(Line, ParentDirs, FsBeforeListing, NextFsAfterListing, FilesIn, FilesOut).

get_dir_filesize(Files, Dir, Filesize) :-
    get_dict(Dir, Files, Filesize).

sum_descendant_directories_filesizes(DecendantDirs, Files, SummedSizes) :-
    maplist(get_dir_filesize(Files), DecendantDirs, DescendantDirsSizes),
    sumlist(DescendantDirsSizes, SummedSizes).

update_filesizes(ParentDir, DecendantDirs, Files, FilesDirectories) :-
    sum_descendant_directories_filesizes(DecendantDirs, Files, SummedSizes),
    get_dict(ParentDir, Files, ParentFilesizes),
    put_dict(ParentDir, Files, ParentFilesizes + SummedSizes, FilesDirectories).

execute_commands(Stream, WorkingDirectory, Files, Directories, FilesOut) :-
    not_eos(Stream),
    read_line_to_string(Stream, Line) -> (
        change_directory(Line, WorkingDirectory, NextWorkingDirectory, Files, Out),
        execute_commands(Stream, NextWorkingDirectory, Out, Directories, FilesOut);
        (
            list_files_in_directory(Line, WorkingDirectory, Directories, NextDirectories, Files, FilesIn),
            execute_commands(Stream, WorkingDirectory, FilesIn, NextDirectories, FilesOut)
        )
    );
    dict_pairs(Directories, directories, DirPairs),
    pairs_keys_values(DirPairs, DescendantDirectories, Dirs),
    foldl(update_filesizes, DescendantDirectories, Dirs, Files, FilesDirectories),
    dict_pairs(FilesDirectories, files, FilesDirectoriesPairs),
    pairs_keys_values(FilesDirectoriesPairs, _, Filesizes),
    exclude(=<(100000), Filesizes, FilesizesOut),
    sumlist(FilesizesOut, FilesOut),
    format("~n~nSolution is ~q~n~n", [FilesOut]).

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_07/sample.txt'], AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    dict_create(Files, files, []),
    dict_create(Directories, directories, []),
    execute_commands(Stream, [], Files, Directories, Solution),

    close(Stream).
