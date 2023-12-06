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


tokenize([Char | _], _, Output) :-
    ERROR_MESSAGE = "Unknown Character, ",

    string_concat(ERROR_MESSAGE, Char, ErrorMessageForOutput),
    Output = error(lexer, ErrorMessageForOutput).



operator_token(Operator, Token) :-
    Operator == "+",
    Token = token(add, Operator).


operator_token(Operator, Token) :-
    Operator == "-",
    Token = token(minus, Operator).


operator_token(Operator, Token) :-
    Operator == "*",
    Token = token(multiply, Operator).


operator_token(Operator, Token) :-
    Operator == "/",
    Token = token(divide, Operator).



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



parse_for_expression(Tokens, Error, Remaining) :-
    parse_for_term(Tokens, Error, Remaining),
    Error = error(parser, _),
    !.


parse_for_expression(Tokens, Expression, Remaining) :-
    parse_for_term(Tokens, Term, RemainingAfterTerm),

    RemainingAfterTerm = [],
    Expression = expression(Term),
    Remaining = [].


parse_for_expression(Tokens, Expression, Remaining) :-
    parse_for_term(Tokens, Term, RemainingAfterTerm),

    RemainingAfterTerm = [Token | _],
    Token = token(Type, _),
    
    not(member(Type, [add, minus])),

    Expression = expression(Term),
    Remaining = RemainingAfterTerm.


parse_for_expression(Tokens, Error, Remaining) :-
    parse_for_term(Tokens, _, RemainingAfterFirstTerm),

    RemainingAfterFirstTerm = [Token | RemainingAfterOperator],
    Token = token(Type, _),

    member(Type, [add, minus]),

    parse_for_term(RemainingAfterOperator, Error, Remaining),
    Error = error(parser, _).


parse_for_expression(Tokens, Expression, Remaining) :-
    parse_for_term(Tokens, FirstTerm, RemainingAfterFirstTerm),

    RemainingAfterFirstTerm = [Token | RemainingAfterOperator],
    Token = token(Type, _),

    member(Type, [add, minus]),

    parse_for_term(RemainingAfterOperator, SecondTerm, Remaining),

    Expression = expression(FirstTerm, Type, SecondTerm).



% This clause must come first before the other terms for error handling
parse_for_term(Tokens, Error, Remaining) :-
    parse_for_factor(Tokens, Error, Remaining),
    Error = error(parser, _),
    !.


parse_for_term(Tokens, Term, Remaining) :-
    parse_for_factor(Tokens, Factor, RemainingAfterFactor),

    RemainingAfterFactor = [],
    Term = term(Factor),
    Remaining = [].


parse_for_term(Tokens, Term, Remaining) :-
    parse_for_factor(Tokens, Factor, RemainingAfterFactor),

    RemainingAfterFactor = [Token | _],
    Token = token(Type, _),
    
    not(member(Type, [multiply, divide])),

    Term = term(Factor),
    Remaining = RemainingAfterFactor.


parse_for_term(Tokens, Error, Remaining) :-
    parse_for_factor(Tokens, _, RemainingAfterFirstFactor),

    RemainingAfterFirstFactor = [Token | RemainingAfterOperator],
    Token = token(Type, _),

    member(Type, [multiply, divide]),

    parse_for_factor(RemainingAfterOperator, Error, Remaining),

    Error = error(parser, _).


parse_for_term(Tokens, Term, Remaining) :-
    parse_for_factor(Tokens, FirstFactor, RemainingAfterFirstFactor),

    RemainingAfterFirstFactor = [Token | RemainingAfterOperator],
    Token = token(Type, _),

    member(Type, [multiply, divide]),

    parse_for_factor(RemainingAfterOperator, SecondFactor, Remaining),

    Term = term(FirstFactor, Type, SecondFactor).



parse_for_factor([Token | Rest], Factor, Remaining) :-
    Token = token(Type, Value),
    Type == digit,

    Factor = factor(Value),
    Remaining = Rest.


parse_for_factor([Token | _], Error, []) :-
    Token = token(Type, _),
    Type \= digit,

    ERROR_MESSAGE = "Expected a digit token",
    Error = error(parser, ERROR_MESSAGE).



test_parser(UserInput) :-
    lexer(UserInput, Tokens),

    parse_for_expression(Tokens, Expression, Remaining),
    !,
    (Expression = error(parser, Message) ->
        writeln(Message)
    ;
        writeln(Expression),
        write("Remaining Tokens: "), writeln(Remaining)
    ).



test_interpreter(UserInput) :-
    lexer(UserInput, Tokens),

    parse_for_expression(Tokens, Expression, Remaining),
    writeln(Remaining),

    interpreter(Expression, Output),
    !,
    writeln(Output).



interpreter(Node, Output) :-
    Node = expression(SingleTerm),
    interpreter(SingleTerm, TermOutput),
    Output = TermOutput.


interpreter(Node, Output) :-
    Node = term(Factor),
    interpreter(Factor, FactorOutput),
    Output = FactorOutput.


interpreter(Node, Output) :-
    Node = term(FirstFactor, Operator, SecondFactor),
    interpreter(FirstFactor, FirstOutput),
    interpreter(SecondFactor, SecondOutput),

    Operator == multiply,

    Output is FirstOutput * SecondOutput.


interpreter(Node, Output) :-
    Node = factor(NumberAsString),
    atom_number(NumberAsString, Output).

