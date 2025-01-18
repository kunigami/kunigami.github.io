BRANCH_NAME=$(git branch --show-current)
echo $BRANCH_NAME
git reset $(git merge-base master $BRANCH_NAME)
git add -A
git commit -m "new post: $BRANCH_NAME"
git checkout master
git merge $BRANCH_NAME
