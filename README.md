# cl-utter

My common lisp unit testing framework and debug logger that I use packaged with everything I make

Deftest macro is used to define tests based on comparators between functions and expected expressions (doesn't have to be a value). test-all used to run all tests. Result is a list of T for passed tests, and a tuple for failure. Comes with excellent debug logging INCLUDING error logging for when you've written invalid expressions in the deftest! That was a hard one to write. I wrote this because I wanted a useful, property-retaining, functional unit-testing framework and I like to populate my code with proper debug logs as well as throw warning where necessary.

Un-comment the example tests in the main file and run (test-all) to see the library in action.


To add: - Allow user to define and populate non-global test groups (yawn, kind of too easy, maybe when I really need to)
- Decide when to throw the user into a debugger for failed expressions to see if they want to re-write them in the debugger? 
