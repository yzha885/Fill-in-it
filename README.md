# Fill-in-it


# Sample 
?- Puzzle = [[‘#',h,'#'],[_,_,_],['#',_,'#']],WordList = [[h,a,t], [b,a,g]],puzzle_solution(Puzzle, WordList).
Puzzle = [[#, h, #], [b, a, g], [#, t, #]],
WordList = [[h, a, t], [b, a, g]] ;
false.



A fill-in Puzzle(sometimes called a fill-it-in) is like a corssword puzzle, except that instead of being give obscure clues telling us whic words go where, you are 
given a list of all the words to place in the puzzle, but not told where they go.

The puzzle consists of a grid of squares, most of which are empty, into which letters or digits are to be written, buts some of which are filled in soild, and are not to be written in. You are also given a list of words to place in the puzzle. You must place each word in the word list exactly once in the puzzle, wither left to right or top to bottom, filling a maximal squance of ecmpty squares. Also , every maximal squence of non-solid squares that is more than one square long must have one word from the wors list written in it. Many words cross one another, so many of the letters in a horizontal word will also be a letter in a vertical word.

The Prolog code to solve fillin puzzles. The Program supplies #puzzle_solution(PUzzle, WordList) that holds when Puzzzle is the representation of a solves fillin puzzle for the given list of words, #WordList.

A fillin puzzle will be represented as a list of lists, each of the same length and each representing a single row of the puzzle. Each element in each of these lists is either a: '#',denoting a solid, unfillable square; an underscore (_), representing a fillable square; or a single, lower case letter (e.g., h), denoting a pre-filled square.
For example, suppose you have a 3 by 3 puzzle with the four corners filled in solid and one pre-filled letter. This would be represented by the Puzzle argument:
?- Puzzle = [[‘#',h,'#'],[_,_,_],['#',_,'#']]

A word list will be represented as a list of lists. Each list is a list of characters, spelling a word. For the above puzzle, the accompanying word list may be:
?- WordList = [[h,a,t], [b,a,g]]. The code is only be tested with proper puzzles, which have at most one solution.Of course, if the puzzle is not solvable, the predicate should fail, and it should never succeed with a puzzle argument that is not a valid solution. For example, your program would solve the above puzzle as below:

?- Puzzle = [['#',h,'#'],[_,_,_],['#',_,'#']], WordList = [[h,a,t], [b,a,g]], puzzle_solution(Puzzle, WordList).
Puzzle = [[#, h, #], [b, a, g], [#, t, #]],
WordList = [[h, a, t], [b, a, g]] ;
false.


