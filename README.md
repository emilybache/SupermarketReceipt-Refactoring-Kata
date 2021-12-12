# The Supermarket Receipt Refactoring Kata

On the 'pick' branches, the exercise is to incrementally add changes, running the tests after each one. 
The end state should be the same as the branch 'with_broken_tests', you just get there in small steps.

In each 'pick' branch one feature has been implemented, and one or more tests are failing. The idea is to handle each feature one at a time, updating the tests as you go.

Get the next pick:

    git checkout origin/pickx

Look at the failing tests, and decide if each pick branch contains a feature or a bug. If it was a feature and you’ve fixed all the tests:

	git add .
	git commit -m"feature"

Then go to the next pick branch and merge in the previous one. For example when you've finished fixing pick1 and want to move on to pick2:

	git checkout origin/pick2
	git merge pick1

If you discover the pick branch contains a bug, revert your changes:

	git clean -df

Do not merge buggy pick branches to the next pick branch, merge the previous good branch instead. By the end you should be on the branch 'pick6', all the tests should be passing, and no bugs are present (since you didn't merge those branches).

How did this experience compare with maintaining the failing tests on the branch 'with_broken_tests'?