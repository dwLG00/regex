# Regex

Simple implementation of a regex parser in prolog.

Current supported symbols:
- Alphanumeric characters and symbols
- wildcards (. and +)
- ?, +, *, |
- parentheses (but not as capture groups... yet)

## Running
I will assume you have [SWI-Prolog]() installed already. This project hasn't been bundled yet, so you will need to manually include the source file into your project.

To load this project in the repl, run `[src/regex_parser].` This will import the regex API.

## Example Usage

```
?- [src/regex_parser].
true.

?- re_compile("abc", Regex). % Compile regex expressions using re_compile/2. Note that predicates ending in "_aux" are auxillary predicates that should never be called explicitly.
Regex = parse_and_aux(parse_char_aux(a), parse_and_aux(parse_char_aux(b), parse_char_aux(c))) .

?- re_compile("abc", Regex), re_match(Regex, "abc"). % re_match/2 is true whenever the Regex expression matches the entire given string.
Regex = parse_and_aux(parse_char_aux(a), parse_and_aux(parse_char_aux(b), parse_char_aux(c))) .

?- re_compile("abc", Regex), re_match(Regex, "ab").
false.

?- re_compile("ab*", Regex), re_match_any(Regex, "cabbbcc"). % Use re_match_any/2 to match any portion of the text.
Regex = parse_and_aux(parse_char_aux(a), parse_star_aux(parse_char_aux(b))) .

?- re_compile("ab*", Regex), re_cut(Regex, "cabbbcc", Before, After). % Use re_cut/4 to divide a string into two on a regex expression. This operation is lazy.
Regex = parse_and_aux(parse_char_aux(a), parse_star_aux(parse_char_aux(b))),
Before = "c",
After = "bbbcc" .

?- re_compile(",|_", Regex), re_split(Regex, "ab,cd_ef", Splits). % Split a string on a regex expression by using re_split/3.
Regex = parse_or_aux(parse_char_aux(','), parse_char_aux('_')),
Splits = ["ab", "cd", "ef"] .
```
