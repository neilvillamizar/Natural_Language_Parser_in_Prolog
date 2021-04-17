
% se uso este fragmento de codigo de wikipedia (lol):
% https://en.wikipedia.org/wiki/Definite_clause_grammar#Parsing_with_DCGs

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(D,N)) --> det(D), noun(N).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
det(d(the)) --> [the].
det(d(a)) --> [a].
noun(n(bat)) --> [bat].
noun(n(cat)) --> [cat].
verb(v(eats)) --> [eats].


main(List) :- sentence(Parse_tree, List, []), write(Parse_tree).