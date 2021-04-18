sentence(sentence(S)) --> single_sentence(S).
sentence(sentence(S, C, SS)) --> single_sentence(S), conjunction(C), sentence(SS).

single_sentence(single_sentence(S, V, C)) --> subject_tps(S), verb_tps(V), complement(C).
single_sentence(single_sentence(S, V, C)) --> subject_other(S), verb(V), complement(C).

subject_tps(subject_tps(P)) --> pronoun_tps(P).
subject_tps(subject_tps(C)) --> composed_noun(C).
subject_tps(subject_tps(A)) --> noun_phrase(A).
subject_tps(subject_tps(N, A, D)) --> noun_sing(N), article(A), description(D).
subject_tps(subject_tps(C, N, A, D)) --> composed_noun(C), noun_sing(N), article(A), description(D).
subject_tps(subject_tps(N, C)) --> noun_phrase(N), composed_noun(C).

composed_noun(composed_noun(N)) --> noun_sing(N).
composed_noun(composed_noun(N, C)) --> noun_sing(N), composed_noun(C).

noun_phrase(noun_phrase(A, N)) --> article(A), noun_sing(N).
noun_phrase(noun_phrase(A, D, N)) --> article(A), description(D), noun_sing(N).

subject_other(subject(P)) --> pronoun(P).
subject_other(subject(N)) --> noun_plural(N).
subject_other(subject(A)) --> noun_phrase_plural(A).
subject_other(subject(L, C, S)) --> noun_list(L), conjunction(C), single_subject(S).

noun_list(noun_list(S)) --> single_subject(S).
noun_list(noun_list(S, C, L)) --> single_subject(S), conjunction(C), noun_list(L).

single_subject(single_subject(S)) --> noun_plural(S).
single_subject(single_subject(S)) --> noun_phrase_plural(S).
single_subject(single_subject(S)) --> subject_tps(S).

noun_phrase_plural(noun_phrase_plural(A, P)) --> article(A), noun_plural(P).
noun_phrase_plural(noun_phrase_plural(A, D, P)) --> article(A), description(D), noun_plural(P).

subject(subject(S)) --> subject_other(S).
subject(subject(S)) --> subject_tps(S).

verb(verb(V)) --> inf_verb(V).
verb(verb(A, V)) --> aux(A), inf_verb(V).
verb(verb(V)) --> past_verb(V).

complement(complement(C)) --> subject(C).
complement(complement(S, A)) --> subject(S), adverbs(A).
complement(complement(C)) --> adverbs(C).

adverbs(adverbs(A)) --> adverb(A).
adverbs(adverbs(A, AA)) --> adverb(A), adverbs(AA).

adverb(adverb(A)) --> time(A).
adverb(adverb(A)) --> place(A).
adverb(adverb(A)) --> direction(A).
adverb(adverb(A)) --> pertenence(A).

time(time(C, T)) --> time_conjunction(C), sentence(T).
time(time(T)) --> time_tokens(T). % -- Tokens like today, tomorrow, yesterday, everyday  etc

place(place(P, N)) --> place_preposition(P), subject(N).
%PlaceTokens -- here, there

direction(direction(P, D)) --> dir_preposition(P), subject(D).

pertenence(pertenence(P, D)) --> pret_preposition(P), subject(D).

aux(aux(will)) --> [will].
conjunction(conjunction(,)) --> [,].
conjunction(conjunction(and)) --> [and].
verb_tps(verb_tps(likes)) --> [likes].
verb_tps(verb_tps(works)) --> [works].
verb_tps(verb_tps(plays)) --> [plays].
pronoun_tps(pronoun_tps(he)) --> [he].
pronoun_tps(pronoun_tps(she)) --> [she].
pronoun_tps(pronoun_tps(it)) --> [it].
noun_sing(noun_sing(comedores)) --> [comedores].
noun_sing(noun_sing(society)) --> [society].
noun_sing(noun_sing(mys)) --> [mys].
noun_sing(noun_sing(bamboo)) --> [bamboo].
noun_sing(noun_sing(postres)) --> [postres].
noun_sing(noun_sing(afternoon)) --> [afternoon].
noun_sing(noun_sing(daniel)) --> [daniel].
noun_sing(noun_sing(carlos)) --> [carlos].
noun_sing(noun_sing(alex)) --> [alex].
noun_sing(noun_sing(music)) --> [music].
noun_sing(noun_sing(class)) --> [class].
noun_sing(noun_sing(project)) --> [project].
article(article(a)) --> [a].
article(article(the)) --> [the].
description(description(new)) --> [new].
description(description(great)) --> [great].
description(description(gamer)) --> [gamer].
pronoun(pronoun(we)) --> [we].
pronoun(pronoun(we)) --> [they].
noun_plural(noun_plural(people)) --> [people].
noun_plural(noun_plural(books)) --> [books].
noun_plural(noun_plural(films)) --> [films].
inf_verb(inf_verb(like)) --> [like].
inf_verb(inf_verb(eat)) --> [eat].
inf_verb(inf_verb(play)) --> [play].
inf_verb(inf_verb(live)) --> [live].
inf_verb(inf_verb(work)) --> [work].
past_verb(past_verb(liked)) --> [liked].
past_verb(past_verb(made)) --> [made].
place_preposition(place_preposition(in)) --> [in].
%time_preoposition(time_preoposition(in)) --> [in].
place_preposition(place_preposition(at)) --> [at].
pret_preposition(pret_preposition(with)) --> [with].
dir_preposition(dir_preposition(for)) --> [for].
time_tokens(time_tokens(tomorrow)) --> [tomorrow].
time_tokens(time_tokens(today)) --> [today].
time_conjunction(time_conjunction(while)) --> [while].
time_conjunction(time_conjunction(during)) --> [during].
main(Input) :- 
	split_string(Input,"\s","\s",String_list),
	maplist(atom_string, List, String_list),
	sentence(Parse_tree, List, []),
	write(Parse_tree).