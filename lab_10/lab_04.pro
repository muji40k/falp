/*****************************************************************************

		Copyright (c) My Company

 Project:  LAB_04
 FileName: LAB_04.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/


predicates
    minus(integer, integer, integer).
    mult(integer, integer, integer).
    gtt(integer, integer).
    factorial(integer, integer).
    fibonacci(integer, integer).

    factorial_inner(integer, integer, integer).
    factorialX(integer, integer, integer).
    fibonacci_inner(integer, integer, integer, integer).

    factorial_back(integer, integer, integer).

clauses
% factorial(Num, _) :- 0 > Num, !, fail.
% factorial(Num, Res) :- factorial_inner(Num, 1, Res).
%  
% factorial_inner(Num, Acc, Acc) :- 1 = Num, !.
% factorial_inner(Num, Acc, Res) :- TmpN = Num - 1, AccIn = Acc * Num,
%                                   factorial_inner(TmpN, AccIn, Res).

    fibonacci(Num, _) :- 0 > Num, !, fail.
    fibonacci(0, 0) :- !.
    fibonacci(1, 1) :- !.
    fibonacci(Num, Result) :- fibonacci_inner(Num, 1, 0, Result).

    fibonacci_inner(2, P1, P2, Result) :- !, Result = P1 + P2.
    fibonacci_inner(Depth, P1, P2, Result) :- Tmp = P1 + P2, TmpD = Depth - 1,
                                              fibonacci_inner(TmpD, Tmp, P1,
                                                              Result).

    minus(A,B,C):- free(A), not(free(B)),not(free(C)),A = C+B.
    minus(A,B,C):- free(B), not(free(A)),not(free(C)),B = C+A.
    minus(A,B,C):- free(C), not(free(B)),not(free(A)),C = A-B.
    %plus(A,B,C):-minus(C,B,A).
    mult(A,B,C):- free(A), not(free(B)),not(free(C)),A = C/B.
    mult(A,B,C):- free(B), not(free(A)),not(free(C)),B = C/A.
    mult(A,B,C):- free(C), not(free(B)),not(free(A)),C = A*B.

    gtt(N,X):- not(free(N)), not(free(X)), N > X.
    gtt(N,X):- free(N), not(free(X)).
    gtt(N,X):- not(free(N)), free(X).
    gtt(N,X):- free(N), free(X).

    factorial(N,X):- factorialX(N,1,X).
    factorialX(0,X,X).
    factorialX(N,Acc,X):-gtt(N, 0),
    			 minus(N,1,N2), 
    			 mult(Acc,N,A2), 
    			 factorialX(N2,A2,X).
    %factorial(Num, Res) :- not(free(Num)), factorial_inner(Num, 1, Res), !.
    %factorial(Num, Res) :- factorial_back(Num, 1, Res).

    factorial_inner(Num, Acc, Acc) :- Num = 1, !.
    factorial_inner(Num, Acc, Res) :- TmpN = Num - 1, AccIn = Acc * Num,
                                      factorial_inner(TmpN, AccIn, Res).

    factorial_back(Num, Acc, Res) :- factorial_inner(Acc, 1, IRes), IRes = Res,
                                     Num = Acc, !.
    factorial_back(Num, Acc, Res) :- AccIn = Acc + 1,
                                     factorial_back(Num, AccIn, Res).

goal
% fibonacci(5, Res).
    % fibonacci(Num, 5).
    factorial(5, Res),
    factorial(X,120).
    % factorial(-1, Res).
    %mult(2,B,6).
