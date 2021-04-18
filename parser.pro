% Representa una oración o una secuencia de oraciones
sentence(sentence(S)) --> single_sentence(S).
sentence(sentence(S, C, SS)) --> single_sentence(S), conjunction(C), sentence(SS).

% Representa una oración única
single_sentence(single_sentence(S, V, C)) --> subject_tps(S), verb_tps(V), complement(C).
single_sentence(single_sentence(S, V, C)) --> subject_other(S), inf_verb(V), complement(C).
single_sentence(single_sentence(S, V, C)) --> subject(S), future_verb(V), complement(C).
single_sentence(single_sentence(S, V, C)) --> subject(S), past_verb(V), complement(C).

% Sujeto en tercera persona del singular
subject_tps(subject_tps(P)) --> pronoun_tps(P).
subject_tps(subject_tps(C)) --> composed_noun(C).
subject_tps(subject_tps(A)) --> noun_phrase(A).
subject_tps(subject_tps(N, A, D)) --> composed_noun(N), article(A), description(D).

% Sustantivo compuesto
composed_noun(composed_noun(N)) --> noun_sing(N).
composed_noun(composed_noun(N, C)) --> noun_sing(N), composed_noun(C).

% Sustantivo con articulo y adjetivo posiblemente
noun_phrase(noun_phrase(A, N)) --> article(A), composed_noun(N).
noun_phrase(noun_phrase(D, N)) -->description(D), composed_noun(N).
noun_phrase(noun_phrase(A, D, N)) --> article(A), description(D), composed_noun(N).

% Sujeto en primera y segunda persona, y en tercera persona del plural
subject_other(subject(P)) --> pronoun(P).
subject_other(subject(N)) --> noun_plural(N).
subject_other(subject(A)) --> noun_phrase_plural(A).
subject_other(subject(L, C, S)) --> noun_list(L), conjunction(C), single_subject(S).

% Lista de sustantivos 
noun_list(noun_list(S)) --> single_subject(S).
noun_list(noun_list(S, C, L)) --> single_subject(S), conjunction(C), noun_list(L).

% Sustantivos simples iincluyendo sustantivos en tercera persona del singular
single_subject(single_subject(S)) --> noun_plural(S).
single_subject(single_subject(S)) --> noun_phrase_plural(S).
single_subject(single_subject(S)) --> subject_tps(S).

% Sustantivo plural compuesto con articulo y adjetivo
noun_phrase_plural(noun_phrase_plural(A, P)) --> article(A), noun_plural(P).
noun_phrase_plural(noun_phrase_plural(D, P)) --> description(D), noun_plural(P).
noun_phrase_plural(noun_phrase_plural(A, D, P)) --> article(A), description(D), noun_plural(P).

% Cualquier sujeto
subject(subject(S)) --> subject_other(S).
subject(subject(S)) --> subject_tps(S).

% Verbo en futuro
future_verb(future_verb(A, V)) --> aux(A), inf_verb(V).

% Complemento
complement(complement(C)) --> subject(C).
complement(complement(S, A)) --> subject(S), predicates(A).
complement(complement(C)) --> predicates(C).

% Lista de predicados 
predicates(predicates(A)) --> predicate(A).
predicates(predicates(A, AA)) --> predicate(A), predicates(AA).

% Predicado compuesto de adverbios 
predicate(predicate(A)) --> time_predicate(A).
predicate(predicate(A)) --> place_predicate(A).
predicate(predicate(A)) --> direction_predicate(A).
predicate(predicate(A)) --> pertenence_predicate(A).

% Predicados/adverbios de tiempo
time_predicate(time_predicate(C, T)) --> time_conjunction(C), sentence(T).
time_predicate(time_predicate(P, N)) --> time_preposition(P), subject(N).
time_predicate(time_predicate(T)) --> time_adverbs(T). 

% Predicados/adverbios de lugar
place_predicate(place(P, N)) --> place_preposition(P), subject(N).
place_predicate(place_predicate(P)) --> place_adverbs(P).

% Predicados/adverbios de dirección
direction_predicate(direction(P, D)) --> dir_preposition(P), subject(D).

% Predicados/adverbios de pertenencia
pertenence_predicate(pertenence(P, D)) --> pert_preposition(P), subject(D).

% tokens

aux(aux(will)) --> [will].

conjunction(conjunction(,)) --> [,].
conjunction(conjunction(and)) --> [and].

pronoun(pronoun(i)) --> [i].
pronoun(pronoun(you)) --> [you].
pronoun(pronoun(we)) --> [we].
pronoun(pronoun(they)) --> [they].

pronoun_tps(pronoun_tps(he)) --> [he].
pronoun_tps(pronoun_tps(she)) --> [she].
pronoun_tps(pronoun_tps(it)) --> [it].

noun_sing(noun_sing(comedores)) --> [comedores].
noun_sing(noun_sing(society)) --> [society].
noun_sing(noun_sing(binary)) --> [binary].
noun_sing(noun_sing(search)) --> [search].
noun_sing(noun_sing(mys)) --> [mys].
noun_sing(noun_sing(bamboo)) --> [bamboo].
noun_sing(noun_sing(postres)) --> [postres].
noun_sing(noun_sing(afternoon)) --> [afternoon].
noun_sing(noun_sing(daniel)) --> [daniel].
noun_sing(noun_sing(carlos)) --> [carlos].
noun_sing(noun_sing(jesus)) --> [jesus].
noun_sing(noun_sing(wahrman)) --> [wahrman].
noun_sing(noun_sing(neil)) --> [neil].
noun_sing(noun_sing(alex)) --> [alex].
noun_sing(noun_sing(music)) --> [music].
noun_sing(noun_sing(class)) --> [class].
noun_sing(noun_sing(project)) --> [project].
noun_sing(noun_sing(hbo)) --> [hbo].
noun_sing(noun_sing(justice)) --> [justice].
noun_sing(noun_sing(league)) --> [league].
noun_sing(noun_sing(film)) --> [film].
noun_sing(noun_sing(zack)) --> [zack].
noun_sing(noun_sing(snyder)) --> [snyder].
noun_sing(noun_sing(jared)) --> [jared].
noun_sing(noun_sing(caracas)) --> [caracas].
noun_sing(noun_sing(grade)) --> [grade].
noun_sing(noun_sing(house)) --> [house].
noun_sing(noun_sing(joker)) --> [joker].
noun_sing(noun_sing(trick)) --> [trick].

noun_plural(noun_plural(projects)) --> [projects].
noun_plural(noun_plural(people)) --> [people].
noun_plural(noun_plural(books)) --> [books].
noun_plural(noun_plural(films)) --> [films].

article(article(a)) --> [a].
article(article(the)) --> [the].

description(description(new)) --> [new].
description(description(old)) --> [old].
description(description(great)) --> [great].
description(description(gamer)) --> [gamer].
description(description(teachers)) --> [teachers].
description(description(student)) --> [student].
description(description(best)) --> [best].
description(description(impossible)) --> [impossible].
description(description(incredible)) --> [incredible].

inf_verb(inf_verb(like)) --> [like].
inf_verb(inf_verb(eat)) --> [eat].
inf_verb(inf_verb(play)) --> [play].
inf_verb(inf_verb(live)) --> [live].
inf_verb(inf_verb(work)) --> [work].
inf_verb(inf_verb(present)) --> [present].
inf_verb(inf_verb(need)) --> [need].
inf_verb(inf_verb(congratulate)) --> [congratulate].

verb_tps(verb_tps(likes)) --> [likes].
verb_tps(verb_tps(works)) --> [works].
verb_tps(verb_tps(plays)) --> [plays].
verb_tps(verb_tps(codes)) --> [codes].
verb_tps(verb_tps(lives)) --> [lives].
verb_tps(verb_tps(comes)) --> [comes].
verb_tps(verb_tps(has)) --> [has].
verb_tps(verb_tps(watches)) --> [watches].

past_verb(past_verb(made)) --> [made].
past_verb(past_verb(coded)) --> [coded].
past_verb(past_verb(slept)) --> [slept].
past_verb(past_verb(came)) --> [came].

place_preposition(place_preposition(in)) --> [in].
place_preposition(place_preposition(at)) --> [at].
place_preposition(place_preposition(on)) --> [on].
place_preposition(place_preposition(to)) --> [to].

pert_preposition(pret_preposition(with)) --> [with].

dir_preposition(dir_preposition(for)) --> [for].
dir_preposition(dir_preposition(to)) --> [to].

time_preposition(time_preposition(during)) --> [during].

place_adverbs(place_adverbs(here)) --> [here].

time_adverbs(time_adverbs(today)) --> [today].
time_adverbs(time_adverbs(tomorrow)) --> [tomorrow].
time_adverbs(time_adverbs(yesterday)) --> [yesterday].
time_adverbs(time_adverbs(everyday)) --> [everyday].
time_adverbs(time_adverbs(tonight)) --> [tonight].

time_conjunction(time_conjunction(while)) --> [while].

main(Input) :- 
    string_lower(Input, LowerInput),
	split_string(LowerInput,"\s","\s",String_list),
	maplist(atom_string, List, String_list),
	sentence(Parse_tree, List, []),
	write(Parse_tree).