:- use_module(library(func)).


lexer(Input, Output) :-
    InputAsList = exclude(remove_whitespace) of maplist(atom_string)
                                             of atom_chars $ Input,
    Tokens = [],
    tokenize(InputAsList, Tokens, Output).


remove_whitespace(CurrentChar) :-
    CurrentChar == " ";
    CurrentChar == "\t";
    CurrentChar == "\n".


tokenize([], Tokens, Tokens).

tokenize([Head | Tail], Tokens, Output) :-
    char_type(Head, digit),

    digit_token(Tail, Head, FinalDigit, Remaining),

    Token = token(digit, FinalDigit),

    append(Tokens, [Token], NewTokens),
    tokenize(Remaining, NewTokens, Output),
    !.

tokenize([Head | Tail], Tokens, Output) :-
    member(Head, ["+", "-", "*", "/"]),

    operator_token(Head, Token),

    append(Tokens, [Token], NewTokens),
    tokenize(Tail, NewTokens, Output).


operator_token(Operator, Token) :-
    Operator == "+",
    Token = token(plus, "").

operator_token(Operator, Token) :-
    Operator == "-",
    Token = token(minus, "").

operator_token(Operator, Token) :-
    Operator == "*",
    Token = token(multiply, "").

operator_token(Operator, Token) :-
    Operator == "/",
    Token = token(divide, "").


digit_token([], CurrentDigit, CurrentDigit, []).

digit_token([Char | Rest], CurrentDigit, FinalDigit, Remaining) :-
    not(char_type(Char, digit)),
    FinalDigit = CurrentDigit,
    Remaining = [Char | Rest],
    !.

digit_token([Char | Rest], CurrentDigit, FinalDigit, Remaining) :-
    char_type(Char, digit),
    string_concat(CurrentDigit, Char, NextDigit),
    digit_token(Rest, NextDigit, FinalDigit, Remaining).

