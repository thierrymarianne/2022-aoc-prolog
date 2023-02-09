SHELL:=/bin/bash

.PHONY: help shell

help: doc
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

shell: ## Run Ciao shell
	@/bin/bash -c '( cd src/day_02 && CIAOALIASPATH="$$(pwd)" prolog )'
