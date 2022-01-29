##Synchronizing DFA with Haskell  
Main IO monad reads automaton from file "automaton.txt", in order for it to work file must be of given structure:  
"[0-9]+\n[A-Za-z]+\n([0-9]+[A-Za-z][0-9]+\n)+"  
where:  
first [0-9]+ designates the number of states  
[A-Za-z]+ is alphabet  
([0-9]+[A-Za-z][0-9]+\n) is transistion function from first to second number after reading character described as [A-Za-z]  
\n is an eol character  
In order for program to work properly letters in transition function need to match ones decribed earlier as alphabet. Such described automaton must be valid DFA.  
"automaton.txt" contains valid sample description of DFA.  
File "volkov-surf.pdf" contains valuable knowledge about synchronizing problem.  
Algorithm used here is suboptimal, it is possible to determine if automaton is synchronizable in deterministic polynomial time. But as the shortest reseting word is NP-hard,  there is no faster method of determining it other than creating power automaton and finding the shortest path to single state vertex in such automaton using BFS with tracking of   traversed path for every vertex in queue.  
