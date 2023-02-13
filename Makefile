SHELL:=/bin/bash

.PHONY: help shell

help: doc
	@grep -E '^[-0-9a-zA-Z_]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

shell: ## Run Ciao shell
	@/bin/bash -c '( cd src/day_02 && CIAOALIASPATH="$$(pwd)" prolog )'

day_05_part_1:  ## Run day 05 part 1
	swipl -s './src/day_05/day_05_part_1.pl' -g 'find_solution(X).'

day_05_part_2:  ## Run day 05 part 2
	swipl -s './src/day_05/day_05_part_2.pl' -g 'find_solution(X).'

day_06_part_1_sample:  ## Run day 06 part 1 from sample data
	swipl -s './src/day_06/day_06_part_1_sample.pl' -g 'find_solution(X).'

day_06_part_1:  ## Run day 06 part 1
	swipl -s './src/day_06/day_06_part_1.pl' -g 'find_solution(X).'

day_06_part_2_sample:  ## Run day 06 part 2 from sample data
	swipl -s './src/day_06/day_06_part_2_sample.pl' -g 'find_solution(X).'

day_06_part_2:  ## Run day 06 part 2
	swipl -s './src/day_06/day_06_part_2.pl' -g 'find_solution(X).'

day_07_part_1_sample:  ## Run day 07 part 1 from sample data
	swipl -s './src/day_07/day_07_part_1_sample.pl' -g 'find_solution(X).'

day_07_part_1:  ## Run day 07 part 1
	swipl -s './src/day_07/day_07_part_1.pl' -g 'find_solution(X).'

day_07_part_2_sample:  ## Run day 07 part 2 from sample data
	swipl -s './src/day_07/day_07_part_2_sample.pl' -g 'find_solution(X).'

day_07_part_2:  ## Run day 07 part 2
	swipl -s './src/day_07/day_07_part_2.pl' -g 'find_solution(X).'
