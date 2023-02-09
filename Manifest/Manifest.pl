:- bundle(advent_of_code).
version('0.1').
depends([core]).
alias_paths([
    advent_of_code = 'src'
]).
%
cmd('advent_of_code', [main='cmds/aoc']).
%
lib('src').
%
manual('advent_of_code', [main='doc/SETTINGS.pl']).
