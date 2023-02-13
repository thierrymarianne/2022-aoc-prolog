% swipl -s ./src/day_07/day_07_part_2.pl -g 'find_solution(Solution).'
:- module(day_07_part_2, [find_solution/1]).

not_eos(Stream) :-
    \+at_end_of_stream(Stream).

format_absolute_path(AbsolutePath, FormattedAbsolutePath) :-
    re_match("_sep_", AbsolutePath) ->
    (
        re_replace("_sep_", "/", AbsolutePath, ReplacementResult),
        re_replace("_dot_", ".", ReplacementResult, FilepathReplacement),
        format_absolute_path(FilepathReplacement, FormattedAbsolutePath)
    );
    FormattedAbsolutePath = AbsolutePath.

absolute_path(Path, AbsolutePath) :-
    length(Path, 1), !,
    AbsolutePath = '_sep_'.

absolute_path([WorkingDirectory|Rest], AbsolutePath) :-
    absolute_path(Rest, RestAbsPath),
    (
        RestAbsPath = '_sep_' ->
        atomic_list_concat([RestAbsPath, WorkingDirectory], AbsPath);
        atomic_list_concat([RestAbsPath, '_sep_', WorkingDirectory], AbsPath)
    ),
    string_to_atom(AbsPath, AbsolutePath).

change_directory(Line, Path, NewPath, FilesIn, FilesOut) :-
    re_matchsub("\\$\\scd\\s(?<directory_name>\\.\\.|\\/|[a-z]+)", Line, Matches),
    get_dict('directory_name', Matches, Dir),
    (
        Dir = ".." -> (
            [_|[ParentDir|R]] = Path,
            NewPath = [ParentDir|R],
            FilesOut = FilesIn
        );(
            append([Dir], Path, NewPath),
            absolute_path(NewPath, DirKey),
            (
                get_dict(DirKey, FilesIn, Filesize),
                Filesize =\= 0 ->
                format("[ERROR] ~s (~d)~n", ["Non-zero filesize", Filesize]);
                put_dict(DirKey, FilesIn, 0, FilesOut)
            )
        )
    ).

directory(Line, ParentPath, FsBeforeListing, FsAfterListing, FilesIn, FilesOut) :-
    re_matchsub("dir\\s(?<directory_name>\\/|[a-z]+)", Line, Matches),
    get_dict('directory_name', Matches, Dir),

    string_to_atom(Dir, DirAtom),
    append([DirAtom], ParentPath, WorkingDirectoryPath),
    absolute_path(WorkingDirectoryPath, DirKey),

    (
        absolute_path(ParentPath, ParentDirKey),

        (
            get_dict(ParentDirKey, FsBeforeListing, DescendantDirectoriesIn),
            ground(DescendantDirectoriesIn)
            -> append(DescendantDirectoriesIn, [DirKey], DescendantDirectories);
            DescendantDirectories = [DirKey]
        ),
        put_dict(ParentDirKey, FsBeforeListing, DescendantDirectories, FsAfterListing),
        put_dict(DirKey, FilesIn, 0, FilesOut)
    ).

file_size(Line, Filesize) :-
    re_matchsub("(?<filesize>[0-9]+)\\s(?<filename>[\\.a-z]+)", Line, Matches),
    get_dict('filesize', Matches, FilesizeCodes),
    number_codes(Filesize, FilesizeCodes).

file_name(Line, ParentDirs, Filepaths, FilepathsOut) :-
    re_matchsub("(?<filesize>[0-9]+)\\s(?<filename>[\\.a-z]+)", Line, Matches),
    get_dict('filename', Matches, Name),
    re_replace("\\.", "_dot_", Name, Replacement),
    string_to_atom(Replacement, Filename),
    append([Filename], ParentDirs, Filepath),
    absolute_path(Filepath, FilepathOut),
    put_dict(FilepathOut, Filepaths, FilepathOut, FilepathsOut).

list_files_in_directory(Line, ParentDirs, FsBeforeListing, NextFsAfterListing, FilesIn, FilesOut, FilepathsIn, FilepathsOut) :-
    (
        Command = "$ ls",
        Line = Command,
        NextFsAfterListing = FsBeforeListing,
        FilesOut = FilesIn,
        FilepathsOut = FilepathsIn
    );

    (
        NextFsAfterListing = FsBeforeListing,
        file_name(Line, ParentDirs, FilepathsIn, FilepathsOut),

        file_size(Line, Filesize),
        absolute_path(ParentDirs, DirectoryKey),
        get_dict(DirectoryKey, FilesIn, FilesizeIn),
        put_dict(DirectoryKey, FilesIn, FilesizeIn + Filesize, FilesOut)
    );

    FilepathsOut = FilepathsIn,
    directory(
        Line, ParentDirs,
        FsBeforeListing, NextFsAfterListing,
        FilesIn, FilesOut
    ).

get_dir_filesize(Files, Dir, Filesize) :-
    get_dict(Dir, Files, Filesize).

sum_descendant_directories_filesizes(DecendantDirs, Files, SummedSizes) :-
    maplist(get_dir_filesize(Files), DecendantDirs, DescendantDirsSizes),
    sumlist(DescendantDirsSizes, SummedSizes).

update_filesizes(ParentDir, DecendantDirs, Files, FilesDirectories) :-
    sum_descendant_directories_filesizes(DecendantDirs, Files, SummedSizes),
    get_dict(ParentDir, Files, ParentFilesizes) ->
    put_dict(ParentDir, Files, ParentFilesizes + SummedSizes, FilesDirectories);
    format("[ERROR] ~s~n", ["Cannot get filesize"]).

found_occurrence(Subject, OccOut) :-
    re_match("_sep_", Subject) -> OccOut = 1;
    OccOut = 0.

count_occurrences(Subject, OccIn, OccOut) :-
    found_occurrence(Subject, FoundOcc),
    FoundOcc = 1 -> (
        re_replace("_sep_", "/", Subject, ReplacementResult),
        found_occurrence(ReplacementResult, Recurse),
        Recurse is 1 -> count_occurrences(ReplacementResult, OccIn + 1, OccOut);
        OccOut is OccIn + 1
    );
    OccOut = 0.

compare_key(Order, LeftPair, RightPair) :-
    pairs_keys_values([LeftPair], [LeftKey], _),
    pairs_keys_values([RightPair], [RightKey], _),

    count_occurrences(LeftKey, 0, LeftSepOccurrences),
    count_occurrences(RightKey, 0, RightSepOccurrences),

    atomic_list_concat([LeftSepOccurrences, "_", LeftKey], LeftTerm),
    atomic_list_concat([RightSepOccurrences, "_", RightKey], RightTerm),

    compare(Order, RightTerm, LeftTerm).

compliant_with_size_limit(Filesize) :-
    Filesize =< 100000.

compliant_dir(Required, Pair) :-
    pairs_keys_values([Pair], _, [Filesize]),
    Filesize >= Required.

compare_filesizes(Order, LeftPair, RightPair) :-
    pairs_keys_values([LeftPair], _, [LeftMember]),
    pairs_keys_values([RightPair], _, [RightMember]),
    LeftFilesize is LeftMember,
    RightFilesize is RightMember,
    compare(Order, RightFilesize, LeftFilesize).

execute_commands(Stream, WorkingDirectory, Filesizes, Directories, FilesOut, FilepathsIn, FilepathsOut, Solution) :-
    not_eos(Stream),
    read_line_to_string(Stream, Line) -> (
        change_directory(Line, WorkingDirectory, NextWorkingDirectory, Filesizes, Out),
        execute_commands(Stream, NextWorkingDirectory, Out, Directories, FilesOut, FilepathsIn, FilepathsOut, Solution);
        (
            list_files_in_directory(Line, WorkingDirectory, Directories, NextDirectories, Filesizes, FilesIn, FilepathsIn, Out),
            execute_commands(Stream, WorkingDirectory, FilesIn, NextDirectories, FilesOut, Out, FilepathsOut, Solution)
        )
    );

    dict_pairs(Directories, directories, DirPairs),

    predsort(compare_key, DirPairs, SortedDirPairs),

    pairs_keys_values(SortedDirPairs, DescendantDirectories, DirPairsValues),
    foldl(update_filesizes, DescendantDirectories, DirPairsValues, Filesizes, FoldedFilesizes),

    dict_pairs(FoldedFilesizes, filesizes, FoldedFilesizesPairs),

    get_dict('_sep_', FoldedFilesizes, UsedSpace),
    RequiredSpace is 30000000 - (70000000 - UsedSpace),

    include(compliant_dir(RequiredSpace), FoldedFilesizesPairs, Candidates),

    predsort(compare_filesizes, Candidates, SortedCandidates),

    last(SortedCandidates, CandidateForDeletion),
    pairs_keys_values([CandidateForDeletion], _, [CandidateForDeletionFilesize]),

    Solution is CandidateForDeletionFilesize,
    format("~n~nSolution is ~d~n~n", [Solution]).

find_solution(Solution) :-
    working_directory(_d, _d),
    atomic_list_concat([_d, 'src/day_07/input.txt'], AbsoluteFilePath),
    exists_file(AbsoluteFilePath),
    open(AbsoluteFilePath, read, Stream, [buffer(line), close_on_abort(true)]),

    dict_create(Directories, directories, []),
    dict_create(Filesizes, filesizes, []),
    dict_create(Filepaths, filepaths, []),

    execute_commands(Stream, [], Filesizes, Directories, _, Filepaths, _, Solution),

    close(Stream).
