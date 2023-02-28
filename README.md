[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-f4981d0f882b2a3f0472912d15f9806d57e124e0fc890972558857b51b24a6f9.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=10320154)
# Midterm 2 Problems (200 points)

The main topics for Midterm 2 are:

- higher order functions (Using them to write non recursive functions,
  implementing them for other kinds of ADTs)

- Nano (and variations of it), interpreters (which could involve changes to /
  variations on Nano), closures

- Semantics, evaluation strategy

This midterm must be completed INDIVIDUALLY.

You can run the test suite with `stack test` or `make`, like the PAs.

The general points breakdown is: 80 public, 80 private, 40 graded by hand.

## Submission Instructions
To submit your code, first commit and push your repository to GitHub. To see the output of the auto-grader tests, click 'Pull Requests' and select Feedback. You can see the output of the tests under the Checks tab.

**When you have completed the assignment, submit your repo to gradescope by selecting your repository and the branch containing your completed assignment. Unless you created your own branches, the branch is called 'main'**. You can access Gradescope through the Canvas assignment page.


## Problem 1: Maps and Filters for Tries (50 public points, 50 private points)

Complete the 4 problems described in [Trie.hs](src/Trie.hs).

## Problem 2: Call-by-Name Semantics  (30 public, 30 private, 40 grader points)

First, complete the questions labeled `semantics_callbyname_applications`
and `semantics_callbyname_lets` in [CallByName.hs](src/CallByName.hs)
by either typing in the reduction rules (in the same style of the lecture 
slides) or by uploading an image file with the same name as the Haskell object.

Complete the function `reduce1` in the same file. The requirements are described in
[CallByName.hs](src/CallByName.hs).

Below the helper functions, be sure to complete the expression `e_cbn`.
