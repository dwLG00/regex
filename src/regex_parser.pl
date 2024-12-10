:- module(regex_parser, [
    re_match/2, re_match_any/2, re_cut/4, re_split/3, re_compile/2,
    parse_and/3, parse_or/3, parse_star/2, parse_maybe/2, parse_char/2, parse_end/3, parse_nothing/3,
    parse_and_aux/5, parse_or_aux/5, parse_star_aux/4, parse_maybe_aux/4, parse_char_aux/4
]).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).

% matching regex
re_match(Regex, Xs) :- call(Regex, Xs, _, []).

re_match_any(Regex, Xs) :- % matches any portion of the text
    append(_, Rest, Xs),
    call(Regex, Rest, _, _).

re_cut(Regex, Xs, Before, After) :-
    append(Before, Rest, Xs),
    call(Regex, Rest, _, After).

re_split(Regex, Xs, SplitValues) :-
    re_cut(Regex, Xs, Before, After) -> (
        re_split(Regex, After, SplitValues2),
        SplitValues = [Before|SplitValues2]
    ); (
        SplitValues = [Xs]
    ).
    

% compile regex

re_compile([], parse_nothing).
re_compile(Xs, Regex) :-
    compile_aux(Xs, RevStack, [], []),
    combine_terms(RevStack, Regex).

compile_aux([], [], _, []).
compile_aux([')'|Rest], [], _, Rest).
compile_aux([X|Xs], RevStack, Stack, Rest) :- 
    X \= ')', (
        X = '*' -> (
            compile_aux(Xs, RevStackRest, [], Rest),
            RevStackRest = [Head|_],
            Head \= '*', Head \= '+', Head \= '?',
            RevStack = ['*'|RevStackRest]
        );
        X = '+' -> (
            compile_aux(Xs, RevStackRest, [], Rest),
            RevStackRest = [Head|_],
            Head \= '*', Head \= '+', Head \= '?',
            RevStack = ['+'|RevStackRest]
        );
        X = '?' -> (
            compile_aux(Xs, RevStackRest, [], Rest),
            RevStackRest = [Head|_],
            Head \= '*', Head \= '+', Head \= '?',
            RevStack = ['?'|RevStackRest]
        );
        X = '|' -> (
            compile_aux(Xs, RevStackRest, [], Rest),
            RevStack = ['|'|RevStackRest]
        );
        (
            (X = '('; X = '.'; plain_atom(X)) -> (
                (
                    X = '(' -> (
                        compile_aux(Xs, ParenParse, [], Rest1),
                        combine_terms(ParenParse, Parse),
                        compile_aux(Rest1, RevStackRest, [], Rest)
                    );
                    X = '.' -> (
                        Parse = parse_any,
                        compile_aux(Xs, RevStackRest, [Parse|Stack], Rest)
                    );
                    plain_atom(X) -> (
                        parse_char(X, Parse),
                        compile_aux(Xs, RevStackRest, [Parse|Stack], Rest)
                    ); false
                ),
                length(RevStackRest, RevStackLen),
                (
                    RevStackLen #> 0 -> (
                        RevStackRest = [RevStackRestHead|RevStackRestTail],
                        (
                            RevStackRestHead = '*' -> (
                                parse_star(Parse, AugParse),
                                RevStack = [AugParse|RevStackRestTail]
                            );
                            RevStackRestHead = '+' -> (
                                parse_plus(Parse, AugParse),
                                RevStack = [AugParse|RevStackRestTail]
                            );
                            RevStackRestHead = '?' -> (
                                parse_maybe(Parse, AugParse),
                                RevStack = [AugParse|RevStackRestTail]
                            ); RevStack = [Parse|RevStackRest]
                        )
                    ); RevStack = [Parse]
                ); false
            ); false
        );
        false
    ).


combine_terms([], parse_nothing).
combine_terms(Terms, Parsed) :-
    split(Terms, '|', AndParts),
    maplist(and_chain, AndParts, AndchainedParts),
    or_chain(AndchainedParts, Parsed).

and_chain([], parse_nothing).
and_chain([X], X).
and_chain([X|Xs], Parsed) :-
    and_chain(Xs, Parsed1),
    parse_and(X, Parsed1, Parsed).

or_chain([], parse_nothing).
or_chain([X], X).
or_chain([X|Xs], Parsed) :-
    length(Xs, N),
    N #> 0,
    or_chain(Xs, Parsed1),
    parse_or(X, Parsed1, Parsed).

% non-escape characters
plain_atom(X) :-
    char_type(X, alnum);
    char_type(X, quote);
    X = '/';
    X = '_';
    X = ',';
    X = '!';
    X = '@';
    X = '#';
    X = '%';
    X = '&';
    X = '=';
    X = '<';
    X = '>';
    X = ' ';
    X = '\n';
    X = '\t'.

% parser combinator

parse_and(Parse1, Parse2, parse_and_aux(Parse1, Parse2)).
parse_and_aux(Parse1, Parse2, Xs, Capture, Rest) :-
    append(Segment1, Segment2, Xs),
    call(Parse1, Segment1, Capture1, []),
    call(Parse2, Segment2, Capture2, Rest),
    append(Capture1, Capture2, Capture).
    %call(Parse1, Xs, Capture1, Rest1),
    %call(Parse2, Rest1, Capture2, Rest),
    %append(Capture1, Capture2, Capture).

parse_or(Parse1, Parse2, parse_or_aux(Parse1, Parse2)).
parse_or_aux(Parse1, Parse2, Xs, Capture, Rest) :-
    call(Parse1, Xs, Capture, Rest);
    call(Parse2, Xs, Capture, Rest).

parse_star(Parse, parse_star_aux(Parse)).
parse_star_aux(_, Xs, [], Xs).
parse_star_aux(Parse, Xs, Capture, Rest) :-
    call(Parse, Xs, Capture1, Rest1),
    parse_star_aux(Parse, Rest1, Capture2, Rest),
    append(Capture1, Capture2, Capture).

parse_plus(Parse, parse_plus_aux(Parse)).
parse_plus_aux(Parse, Xs, Capture, Rest) :- call(Parse, Xs, Capture, Rest).
parse_plus_aux(Parse, Xs, Capture, Rest) :-
    call(Parse, Xs, Capture1, Rest1),
    parse_plus_aux(Parse, Rest1, Capture2, Rest),
    append(Capture1, Capture2, Capture).

parse_maybe(Parse, parse_maybe_aux(Parse)).
parse_maybe_aux(_, Xs, [], Xs).
parse_maybe_aux(Parse, Xs, Capture, Rest) :- call(Parse, Xs, Capture, Rest).

parse_char(Char, parse_char_aux(Char)).
parse_char_aux(_, [], _, _) :- false.
parse_char_aux(Char, [Char|Rest], [Char], Rest).

parse_any([X|Xs], [X], Xs).

parse_end([], [], []).

parse_nothing(Xs, [], Xs). % parse_and(Parse, parse_nothing, Parse); parse_and(parse_nothing, Parse, Parse)
