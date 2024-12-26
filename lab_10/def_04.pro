
:- use_module(library(clpfd)).

factorial(0, 1).
factorial(Num, Res) :- 0 #< Num, factorial_inner(Num, 1, Res).

factorial_inner(Num, Acc, Acc) :- 1 #= Num, !.
factorial_inner(Num, Acc, Res) :- TmpN #= Num - 1, AccIn #= Acc * Num,
                                  factorial_inner(TmpN, AccIn, Res).

fibonacci(0, 0) :- !.
fibonacci(1, 1).
fibonacci(Num, Result) :- 1 #< Num, fibonacci_inner(Num, 1, 0, Result).

fibonacci_inner(2, P1, P2, Result) :- Result #= P1 + P2, !.
fibonacci_inner(Depth, P1, P2, Result) :- 2 #< Depth, Tmp #= P1 + P2,
                                          TmpD #= Depth - 1,
                                          fibonacci_inner(TmpD, Tmp, P1,
                                                          Result).

