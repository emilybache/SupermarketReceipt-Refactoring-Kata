# The Supermarket Receipt Refactoring Kata

On this branch, the exercise is to incrementally add changes, running the tests after each one. 
The end state should be the same as the branch 'with_broken_tests', you just get there in small steps.

In this branch all the tests start out passing. The idea is to merge in each feature one at a time, from the branches
named 'pick1', 'pick2' etc. Think of each pick branch as a feature branch where someone has developed one feature but 
didn't remember to update the tests.

Get the next pick:

    git merge origin/pickx

If it was a feature and you’ve fixed all the tests:

	git add .
	git commit -m"feature"

If it was a bug, revert the pick:

    git reset --hard HEAD~1

    
