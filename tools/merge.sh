#!/bin/bash

current_branch=$(git rev-parse --abbrev-ref HEAD)

if [ "$current_branch" == "master" ]; then
    echo "Currently on $current_branch branch. Skipping..."
else
    git reset $(git merge-base master $(git branch --show-current))
    git add -A
    git commit -m "new post: $(git rev-parse --abbrev-ref HEAD)$"
    git checkout master

    echo "Do you want to continue? (y/N)"
    read -r response
    if [[ "$response" == "y" || "$response" == "Y" ]]; then
        git merge $current_branch
    else
        echo "Skip running: git merge $current_branch"
    fi
fi
