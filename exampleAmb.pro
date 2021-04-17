
% Recursiones a la izquierda parecen no funcionar
% llamadas a si mismo como expr --> expr ...
% probar con expr//2

%expr --> expr, [mas], expr.
%expr --> [uno].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Aqui listaI y listaD con recursion a la derecha si funcionan
% Probar con listadoble//2
% listadoble([izq,izq,der,der],[]).
% o con phrase/2
% phrase(listadoble, [izq,izq,der,der]).

%listadoble --> listaI, listaD.
%listaI --> [izq], listaI.
%listaI --> [].
%listaD --> [der], listaD.
%listaD --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% en el tuto https://www.metalevel.at/prolog/dcg
% dice que se pueden usar recursiones izquierdas
% Agregando unas 'diferencias' de listas como args.
% video-tutorial: https://www.youtube.com/watch?v=6egAF4-HVzw

expr([_|Ls0], Ls) --> expr(Ls0,Ls1), [mas], expr(Ls1, Ls).
expr(Ls, Ls) --> [uno].

% Para probar:
% X = [uno,mas,uno], phrase(expr(X,_),X).
% phrase(expr([uno,mas,uno],_),[uno,mas,uno]).

% Entonces logramos correr una gramatica ambigua, yei!