.PHONY: sync test prune download poll coverage tree help

##@ Data 

sync: ## sync data to s3; needs AWS profiles set up
	Rscript dev/sync_data_to_s3.R
	
poll: ## Check to see if NYC published more data
	Rscript dev/data_and_data_prep/polling_nyc_for_data.R	

##@ Testing & Quality

test: ## Run testhat and shinytests2 
	Rscript -e "Sys.setenv(NOT_CRAN = 'true'); offline_data_prep <- TRUE; shinytest2::test_app()"

coverage: ## Run dedicated covr 
	Rscript dev/testing/coverage.R
	
##@ Misc	


prune: ## Clean local repo
	Rscript dev/clean_local_git.R
	
tree: ## Create tree of local files
	tree -I ".git|.Rhistory|.Rprofile|.Renviron|data-raw|renv|.Rproj.user|app_data|data"
	
help: 
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[.a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
