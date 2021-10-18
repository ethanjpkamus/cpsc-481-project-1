# Class number
CPSC 481 - Section 01

# Project number and name
Project 1 - Card Sorting

# Team name and members
* Ethan Kamus (ethanjpkamus@csu.fullerton.edu)

# Intro (including the algorithm used)
This project was used to demonstrate the use of state space search 
and decision making with the use of a heuristic function.

Given 3 rows of n cards (3 ≤ n ≤ 7) we were tasked with finding
the steps to successfully sorting them. The set of cards are
considered sorted when all cards are in the left most row

For example
* Given the set ((E C A) (G B) (F D))
* we are expected to find the steps to the goal state ((G F E D C B A)()())

# Contents: Files in the .zip submission
* `card-sorting-v2.el`
* `card-sorting-v2.el~`
* `README.md`

# External Requirements (None?)
None

# Setup and Installation (if any)
* make sure to have emacs downloaded
	* https://www.gnu.org/software/emacs/download.html
* open `card-sorting-v2.el` in emacs

# Sample invocation
* enter the keyboard shortcut `alt + x`
* type `eval-buffer`
* enter the keyboard shortcut `ctrl + x, ctrl + b`
	* on the right, click on the `messages` buffer
After all this:
* on the left should appear the code
* on the right should appear the output
# Features (both included and missing)
## Missing:
* unable to display move indexes
	* i.e. m(2,1) indicating move from row 2 to row 1
# Bugs (if any)
* none from my testing