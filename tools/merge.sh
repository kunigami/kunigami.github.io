#!/bin/bash

# TODO: check current branch is not master
git reset $(git merge-base master $(git branch --show-current))
git add -A

echo "Creating new, clean commit"
git commit -m "new post: $(git rev-parse --abbrev-ref HEAD)$"

# Save current branch to merge later
current_branch=$(git rev-parse --abbrev-ref HEAD)

git checkout master
