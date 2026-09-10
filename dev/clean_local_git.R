# prune branches and offer to remove all local branches

cat("Fetching and pruning remote-tracking branches...\n")
system("git fetch --prune")

# Retrieve the currently checked-out branch
current_branch <- system("git branch --show-current", intern = TRUE)

# Retrieve all local branches
local_branches <- system("git branch --format='%(refname:short)'", intern = TRUE)

# Protect standard branches from accidental deletion
protected_branches <- c(current_branch, "main", "master")
branches_to_check <- setdiff(local_branches, protected_branches)

if (length(branches_to_check) == 0) {
  cat("No other local branches to review.\n")
} else {
  cat("\nFound the following local branches:\n")
  print(branches_to_check)

  # Prompt using readLines for Rscript compatibility
  cat("\nWould you like to review and delete any of these branches? (y/n): ")
  proceed <- readLines("stdin", n = 1)

  if (tolower(trimws(proceed)) %in% c("y", "yes")) {
    # Loop through each branch and ask for instructions
    for (branch in branches_to_check) {
      cat(sprintf("\nWhat would you like to do with branch '%s'?\n", branch))
      cat("1: Keep\n2: Delete (Safe)\n3: Force Delete\nChoice (1/2/3): ")

      action <- trimws(readLines("stdin", n = 1))

      if (action == "2") {
        # Safe delete: Git will block this if the branch is unmerged
        system(paste("git branch -d", branch))
      } else if (action == "3") {
        # Force delete: Git will delete the branch regardless of merge status
        system(paste("git branch -D", branch))
      }
    }
    cat("\nBranch cleanup complete.\n")
  } else {
    cat("\nCleanup cancelled.\n")
  }
}
