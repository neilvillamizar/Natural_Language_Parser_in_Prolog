% https://en.wikipedia.org/wiki/Definite_clause_grammar

sentence --> noun_phrase, verb_phrase.
noun_phrase --> det, noun.
verb_phrase --> verb, noun_phrase.
det --> [the].
det --> [a].
noun --> [cat].
noun --> [bat].
verb --> [eats].

% sentence([the,bat,eats,the,bat],[])