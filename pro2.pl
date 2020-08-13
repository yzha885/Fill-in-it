
/*
Author: Yajing Zhang 1100453 <yajzhang@student.unimelb.edu.au>
Purpose: This program is designed to solve a fill-in puzzle(aka fill-it-in).
        puzzle_solution/2(?List,+List) is the entry of the program. The Puzzle is represented as a list of lists, each of 
        the same length and each representing a single row of the puzzle.Each element in each of these lists is either a:  '#', denoting a solid, unfillable square; an underscore (_), representing a fillable square; or a single, lower    case letter (e.g., h), denoting a pre-filled square. 

*/

/*load SWI library ensure transpose/2 word propertly*/
:- ensure_loaded(library(clpfd)).

/*Main entry of the program  puzzle_solution/2(?List,+List)*/
puzzle_solution(Puzzle, WordList):-
 validation(Puzzle),
 solution(Puzzle, WordList).
 
/*ensure the length of each rows are the same */
validation([]).
validation([Row|Rows]) :-
	maplist(samelength(Row), Rows).
samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).
 
 
 %%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%
 
/* Main logic that solove the problem. This process is achieved through following  substeps that covert underscore %character into logical variable, generate the available slots both vertical and horizontal and fill the slots with the given WordList. */ 
solution(Puzzle,WordList):-
 sort_wordList(WordList,WordListSort),
 toSlotH(Puzzle,[],HorizontalSlots), /*Make consistency with existing logical variable and horizontal slots*/ 
 transpose(Puzzle,VerticalSlots),   /* To transpose the horizontal puzzle to vertical puzzle*/
 toSlotH(VerticalSlots,HorizontalSlots,VerticalSlots),
 fill_slots(VerticalSlots, WordList).
 
 /*Covert the underscore character into logical variable and generate the horizontalSlots.*/ 
toSlotH([],Buffer, Buffer).
toSlotH([Row|Rows],Buffer,HorizontalSlots) :-
  toRowSlot(Row,[],Buffer,RowSlot),
  toSlotH(Rows,RowSlot,HorizontalSlots).
  
% subprogram used to suppporting toSlotH/3. To generate the slot for each row.
%Loop through each element of the row. Save a new slot if occurs # otherwise keep adding element.
% Note that only slots should contains at least two elements.
toRowSlot([],[],ListBuffer,ListBuffer).  
toRowSlot([],WordBuffer,ListBuffer, HorizontalSlots):-
( length(WordBuffer,Length),
Length >1 -> 
  append(ListBuffer, [WordBuffer],ListBuffer1),
  toRowSlot([],[],ListBuffer1, HorizontalSlots)
  ;
  toRowSlot([],[],ListBuffer,HorizontalSlots)
).
toRowSlot([H|T],WordBuffer,ListBuffer,HorizontalSlots):-
  ( H =='#' ->
   (length(WordBuffer,Length), Length >1 ->
    append(ListBuffer,[WordBuffer],ListBuffer1),
    toRowSlot(T,[],ListBuffer1,HorizontalSlots)
    ; toRowSlot(T,[],ListBuffer,HorizontalSlots)
   )

   %the case H is not #
   ;append(WordBuffer,[H],WordBuffer1),
    toRowSlot(T,WordBuffer1,ListBuffer,HorizontalSlots)
    ).
 
    

/*Sort WordList based on the number of same-length word in the wordList*/
/*Example  [[h, a, t], [a, p, p, l, e], [I, m], [p, e, n, c, i]] => [[p, e, n, c, i, l], [a, p, p, l, e], [h, a, t], [I, m]].*/
sort_wordList(WordList, Rev) :-
% map list to the key-value list with length as list e.g [3-[h,a,t]]
 map_list_to_pairs(length, WordList,WordListPair),
% Sort the list based on the key(length) e.g [2-[i,m]],[3-[h,a,t]]
 keysort(WordListPair,WorListPairSorted),
% Group pair by key e.g [2-[[i,m],[o,r]]].
 group_pairs_by_key(WorListPairSorted,WordListGroupByLength), 
 % Get ride of the key and only keep the value e.g [[[i,m],[o,r]]]
 pairs_values(WordListGroupByLength,WordListSort),
 % Remove the nested list and make list as a list of list of word same format as orginal WordList
 remove_nest(WordListSort,WordListSorted),
 % Reverse the list to make the list as descending order
 reverse(WordListSorted,Rev).
 
 % Remove the nested list and make list as a list of list of word same format as orginal WordList
remove_nest([], []).
remove_nest([X|Xs], Ys) :-
	remove_nest(Xs, A),
	append(X, A, Ys).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_words([Word|Words],H_V_Slots):-
	match_one_word_to_slot(Word,H_V_Slots,[],Slots_Left),
	match_words(Words,Slots_Left).
match_words([],_).

 match_one_word_to_slot(Word,[Slot|Slots],Acc,Result):-

	% when the word can unify a slot, return the rest slots
	Word = Slot,
	append(Slots,Acc,Result)
	;
	
	% if word can not unify the slot, continue on rest slots
	append(Acc,[Slot],Acc_New),	
	match_one_word_to_slot(Word,Slots,Acc_New,Result).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%start to fill the slots
fill_slots([],[]).
fill_slots(FinalSlots,Wordlist):-
    %choose the slot with the least number of possibilities in the worldlist to fill
    %in firstly
    choose_best_slot_tofill(FinalSlots,Wordlist,BestSlot),

    %generate possible words list after finding the best slots by matching the best slot
    %with the words in the words list
    exclude(\=(BestSlot), Wordlist, PossibleWords),

    %pick a word that in the PossibleWords list and fill the slot with this word
    member(Word,PossibleWords),
    BestSlot = Word,

    %delete this word in the wordlist in case repetition
    exclude(==(Word),Wordlist,WordsLeft),
    %delte this slot in the puzzle slot in case repeition
    exclude(==(BestSlot),FinalSlots,NextSlots),

    %keep looping and fill in other slots with the left words
    fill_slots(NextSlots,WordsLeft).


%by given the slots list, find the best slot which is the slot that has the 
%least number of possibilities in the worldlist to fill in firstly. To achieve this
%we need to first count the possibilities for each slots in the FinalSlots list first
choose_best_slot_tofill([Head|Tail],Wordlist,BestSlot):-
    %count the possibilities
    choose_word(Head,Wordlist,Count),
    %base on the count, find the best slot to start with
    choose_best_slot_tofill(Tail,Wordlist,Count,Head,BestSlot).

%if there is no more slots in FinalSlots, the current finding BestSlot is the 
%best slot to start with
choose_best_slot_tofill([],_,_,BestSlot,BestSlot).
%Start to loop each slots in the final slots and store the count in the Minimum variable
%if there is any slots that have possibilities nubmer that smaller than the minumum one
%update it with the new count number and update the CurrentBestSlot 
choose_best_slot_tofill([Head|Tail],Wordlist,Minimum,CurrentBestSlot,BestSlot):-
    choose_word(Head,Wordlist,Count),
    (Count<Minimum ->
    UpdateCurrentBestSlot = Head,
    UpdateMinimum = Count
    ;
    UpdateCurrentBestSlot = CurrentBestSlot,
    UpdateMinimum = Minimum),
    choose_best_slot_tofill(Tail,Wordlist,UpdateMinimum,UpdateCurrentBestSlot,BestSlot).




%check the currrent slot e.g. ['LogicalVariable','LogicalVariable'] has how many Words
%that match this slot
choose_word(Slot,Wordlist,Count):-
    compute_possibilities(Slot,Wordlist,0,Count).


%if the worldlist reaches the end, the current count is the final count
compute_possibilities(_,[],Accumulator,Accumulator).
%use the logical variable to match each words in the wordlist, if there is a
%match, count increases by 1. keep looping to find all possibilities and save 
%the count number into Count
compute_possibilities(Slot,[Head|Tail],Accumulator,Count):-
    %logic variables can help match
    (Slot \= Head ->
    UpdateAccumulator is Accumulator + 0
    ;
    UpdateAccumulator is Accumulator + 1),
    compute_possibilities(Slot,Tail,UpdateAccumulator,Count).


%------------------------fill the slots function end-----------------------------%  
    
    
    
    
    
    
    
    
    