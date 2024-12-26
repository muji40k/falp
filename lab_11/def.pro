domains
    int_list = integer*.
    num_t = numerical(integer).
    log_t = logical(string).
    symbol_t = symbol(string).
    op_res = numerical(integer); logical(string).
    exp_val  = numerical(integer); logical(string); symbol(string).
    exp_list = exp_val*.
    res_list = string*.

predicates
    int_to_exp_list(int_list, exp_list).
    exp_to_res_list(exp_list, res_list).

    exp_lookup(exp_val, string).

    solve(int_list, res_list).
    eval_str(res_list, op_res).

    place(exp_list, exp_list, exp_list).
    unary(exp_val, exp_list, exp_list).
    binary(exp_val, exp_list, exp_list).

    apply(symbol_t, op_res, op_res, op_res).

    eval_unary(exp_list, op_res, exp_list).
    eval(exp_list, op_res).
    eval_sequence(exp_list, integer, integer, op_res, op_res, exp_list).

    get_op(exp_list, symbol_t, integer, exp_list).
    get_prio(string, integer).

    reverse(exp_list, exp_list).
    reverse_inner(exp_list, exp_list, exp_list).

    pow(integer, integer, integer).

clauses
    solve(Input, Res) :- int_to_exp_list(Input, CInput), place(CInput, [], RRes),
                         reverse(RRes, ERes), eval(ERes, logical("True")),
                         exp_to_res_list(ERes, Res).

    eval_str(List, Res) :- exp_to_res_list(Exp, List), eval(Exp, Res).

    int_to_exp_list([], []).
    int_to_exp_list([H | T], [numerical(H) | TmpL]) :- int_to_exp_list(T, TmpL).

    exp_to_res_list([], []).
    exp_to_res_list([H | T], [Mod | TmpL]) :- exp_lookup(H, Mod), exp_to_res_list(T, TmpL).

    exp_lookup(numerical(Num), Str) :- str_int(Str, Num), !.
    exp_lookup(logical("True"), "True") :- !.
    exp_lookup(logical("False"), "False") :- !.
    exp_lookup(symbol(Str), Str).


    % Form expression

    unary(Val, Acc, [Val | Acc]).
    unary(numerical(Val), Acc, [numerical(ValIn) | Acc]) :- ValIn = -Val.

    binary(Val, Acc, [symbol("+")   | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, [symbol("-")   | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, [symbol("*")   | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, [symbol("div") | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, [symbol("mod") | AccIn]) :- unary(Val, Acc, AccIn).
    binary(Val, Acc, [symbol("=")   | AccIn]) :- unary(Val, Acc, AccIn).


    place([], Res, Res).
    place([H | []], Acc, Res) :- !, unary(H, Acc, Res).
    place([H | T], Acc, Res) :- binary(H, Acc, AccIn), place(T, AccIn, Res).


    % Evaluate expression

    apply(symbol("="),   numerical(N1), numerical(N2), logical("True"))  :- N1 = N2.
    apply(symbol("="),   numerical(N1), numerical(N2), logical("False")) :- N1 <> N2.
    apply(symbol("+"),   numerical(N1), numerical(N2), numerical(Res))   :- Res = N1 + N2.
    apply(symbol("-"),   numerical(N1), numerical(N2), numerical(Res))   :- Res = N1 - N2.
    apply(symbol("*"),   numerical(N1), numerical(N2), numerical(Res))   :- Res = N1 * N2.
    apply(symbol("div"), numerical(N1), numerical(N2), numerical(Res))   :- Res = N1 div N2.
    apply(symbol("mod"), numerical(N1), numerical(N2), numerical(Res))   :- Res = N1 mod N2.

    % ^
    apply(symbol("^"), numerical(N1), numerical(N2), numerical(Res)) :- pow(N1, N2, Res).
    %


    eval_unary([numerical(Num) | Rest], numerical(Num), Rest).


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

    eval_sequence(List, _, RetPrio, Acc, Res, Rest) :-
        get_op(List, Op, PrioIn, Rest1),
        eval_unary(Rest1, Num, Rest2),
        apply(Op, Acc, Num, ResIn),
        eval_sequence(Rest2, PrioIn, RetPrio, ResIn, Res, Rest).


    get_op([symbol(H) | T], symbol(H), Prio, T) :- get_prio(H, Prio).
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
% eval_str(["-3", "*", "6", "+", "2", "*", "2"], Res).
    % eval_str(["3", "*", "2", "=", "1", "+", "1", "*", "5"], Res).	
    % eval_str(["4", "mod", "2", "=", "0"], Res).
    % eval_str(["1", "-", "-5", "=", "6"], Res).
    % solve([1, 1, 2], Res).
    solve([1, 5, 3, 2], Res).
    % solve([1, 1, 1], Res).
    % solve([1, 2, 3, 8], Res).
    % eval_str(["4", "*", "2", "^", "3"], Res).
    % eval_str(["0", "=", "2", "^", "3", "^", "5"], Res).
    % eval_str(["1", "+", "2", "*", "3"], Res).

