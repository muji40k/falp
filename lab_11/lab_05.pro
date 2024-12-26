domains
    int_list = integer*.
    exp_list = string*.

predicates
    int_to_exp_list(int_list, exp_list).

    solve(int_list, exp_list).

    place(exp_list, exp_list, exp_list).
    unary(string, exp_list, exp_list).
    binary(string, exp_list, exp_list).

    apply(string, string, string, string).
    apply_integer(string, integer, integer, integer).

    eval_unary(exp_list, string, exp_list).
    eval(exp_list, string).
    eval_sequence(exp_list, integer, integer, string, string, exp_list).

    get_op(exp_list, string, integer, exp_list).
    get_prio(string, integer).

    reverse(exp_list, exp_list).
    reverse_inner(exp_list, exp_list, exp_list).

    pow(integer, integer, integer).

clauses
    solve(Input, Res) :- int_to_exp_list(Input, CInput), place(CInput, [], RRes),
                         reverse(RRes, Res), eval(Res, "True").

    int_to_exp_list([], []).
    int_to_exp_list([H | T], [Tmp | TmpL]) :- str_int(Tmp, H),
                                              int_to_exp_list(T, TmpL).

    % Form expression

    unary(Val, Acc, [Val | Acc]).
    unary(Val, Acc, [Val | ["-" | Acc]]).

    binary(Val, Acc, ["+" | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, ["-" | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, ["*" | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, ["div" | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, ["mod" | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, ["=" | AccIn]) :- unary(Val, Acc, AccIn).

    place([], Res, Res).
    place([H | []], Acc, Res) :- !, unary(H, Acc, Res).
    place([H | T], Acc, Res) :- binary(H, Acc, AccIn), place(T, AccIn, Res).

    % Evaluate expression

    apply("=", N1, N2, "True") :- str_int(N1, IN1), str_int(N2, IN2),
                                  IN1 = IN2, !.
    apply(Op, N1, N2, Res) :- str_int(N1, IN1), str_int(N2, IN2),
                              apply_integer(Op, IN1, IN2, IRes),
                              str_int(Res, IRes).
    apply_integer("+", N1, N2, Res) :- Res = N1 + N2.
    apply_integer("-", N1, N2, Res) :- Res = N1 - N2.
    apply_integer("*", N1, N2, Res) :- Res = N1 * N2.
    apply_integer("div", N1, N2, Res) :- Res = N1 div N2.
    apply_integer("mod", N1, N2, Res) :- Res = N1 mod N2.

    % ^
    apply_integer("^", N1, N2, Res) :- pow(N1, N2, Res).
    %

    eval_unary(["-" | [Num | Rest]], Val, Rest) :- str_int(Num, INum),
                                                   IVal = - INum,
                                                   str_int(Val, IVal), !.
    eval_unary([Num | Rest], Num, Rest).

    eval(List, Res) :-
        eval_unary(List, Num, Rest1),
        get_op(Rest1, _, Prio, _),
        eval_sequence(Rest1, Prio, -1, Num, Res, _), !.

    eval_sequence([], _, _, Acc, Acc, []) :- !.

    eval_sequence(List, _, RetPrio, Acc, Acc, List) :-
        get_op(List, _, PrioIn, _),
        PrioIn <= RetPrio,
        !.

    eval_sequence(List, Prio, _, _, _, _) :-
        get_op(List, _, PrioIn, _),
        PrioIn > Prio,
        !,
        fail.

    eval_sequence(List, Prio, RetPrio, Acc, Res, Rest) :-
        get_op(List, Op, PrioIn, Rest1),
        eval_unary(Rest1, Num, Rest2),
        get_op(Rest2, _, Prio2, _),
        eval_sequence(Rest2, Prio2, PrioIn, Num, Val, Rest3),
        apply(Op, Acc, Val, ResIn),
        eval_sequence(Rest3, Prio, RetPrio, ResIn, Res, Rest).

    eval_sequence(List, Prio, RetPrio, Acc, Res, Rest) :-
        get_op(List, Op, PrioIn, Rest1),
        eval_unary(Rest1, Num, Rest2),
        apply(Op, Acc, Num, ResIn),
        eval_sequence(Rest2, PrioIn, RetPrio, ResIn, Res, Rest).


    get_op([H | T], H, Prio, T) :- get_prio(H, Prio).
    get_prio("=",   0).
    get_prio("+",   1).
    get_prio("-",   1).
    get_prio("*",   2).
    get_prio("div", 2).
    get_prio("mod", 2).

    % ^
    get_prio("^",   3).
    %

    reverse(List, Res) :- reverse_inner(List, [], Res).
    reverse_inner([], Acc, Acc) :- !.
    reverse_inner([H | T], Acc, Res) :- reverse_inner(T, [H | Acc], Res).

    pow(_, N2, 0) :- N2 < 0, !.
    pow(_, 0, 1) :- !.
    pow(N1, 1, N1) :- !.
    pow(N1, N2, Res) :- 1 = N2 mod 2, NN = N2 div 2, pow(N1, NN, ResIn),
                        Res = ResIn * ResIn * N1, !.
    pow(N1, N2, Res) :- NN = N2 div 2, pow(N1, NN, ResIn), Res = ResIn * ResIn.

goal
    % int_to_exp_list([1, 2, 4, 9, 1], Res).
    % place(["1", "2", "3"], [], Res).
    % p(["1", "2"], Res).
    % eval(["-", "3", "*", "6", "+", "2", "*", "2"], Res).
    % eval(["3", "*", "2", "=", "1", "+", "1", "*", "5"], Res).
    % eval(["4", "mod", "2", "=", "0"], Res).
    % eval(["1", "-", "-", "5", "=", "6"], Res).
    solve([1, 1, 2], Res).
    % solve([1, 5, 3, 2], Res).
    % solve([1, 1, 1], Res).
    % solve([1, 2, 3, 8], Res).
    % eval(["4", "*", "2", "^", "3"], Res).
    % eval(["2", "^", "3", "^", "5"], Res).

