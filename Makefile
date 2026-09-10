.PHONY: sync test prune download poll coverage

sync:
	Rscript dev/sync_data_to_s3.R

test:
	Rscript -e "offline_data_prep <- TRUE; shinytest2::test_app()"

prune:
	Rscript dev/clean_local_git.R
	
download:
	Rscript dev/download_code_for_ai.R
	
poll:
	Rscript dev/data_and_data_prep/polling_nyc_for_data.R
	
coverage:
	Rscript dev/testing/coverage.R