#!/bin/bash

echo "Fetching latest changes and pruning remote references..."
# This updates your local awareness of the remote and removes deleted remote branches
git fetch --prune

echo "Identifying local branches with deleted remote counterparts..."
# Find branches marked as '[gone]' and extract their names
GONE_BRANCHES=$(git branch -vv | grep ': gone]' | awk '{print $1}')

if [ -z "$GONE_BRANCHES" ]; then
  echo "Your local repository is clean. No branches to prune."
else
  echo "Deleting the following local branches:"
  echo "$GONE_BRANCHES"
  
  # Force delete (-D) the branches. Force is required if the PR was squash-merged on GitHub.
  echo "$GONE_BRANCHES" | xargs git branch -D
  
  echo "Cleanup complete."
fi