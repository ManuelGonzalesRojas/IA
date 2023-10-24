%hechos
%FAMILIA_DE_MANUEL

progenitor(rosa, laura).
progenitor(manuel_I, laura).
progenitor(rosa, arturo).
progenitor(manuel_I, arturo).
progenitor(rosa, mirna).
progenitor(manuel_I,mirna).
progenitor(rosa, manuel_II).
progenitor(manuel_I,manuel_II).
progenitor(kim, toshiro).
progenitor(arturo, toshiro).
progenitor(mirna, raul_II).
progenitor(raul_I, raul_II).
progenitor(mirna, angel).
progenitor(raul_I, angel).
progenitor(eusebio, rofina).
progenitor(rosaura, rofina).
progenitor(rofina, erika).
progenitor(amador, erika).
progenitor(rofina, socimo).
progenitor(amador, socimo).
progenitor(rofina, claudia).
progenitor(amador, claudia).
progenitor(rofina, urbano).
progenitor(amador, urbano).
progenitor(julia, jesus).
progenitor(urbano, jesus).
progenitor(claudia, cristian).
progenitor(pablo, cristian).
progenitor(claudia, jose).
progenitor(pablo, jose).
progenitor(natalia, ariana).
progenitor(jose, ariana).
progenitor(cleme, cesar).
progenitor(socimo, cesar).
progenitor(cleme, diana).
progenitor(socimo, diana).
progenitor(cleme, mari).
progenitor(socimo, mari).
progenitor(mari, kenny).
progenitor(joel, kenny).
progenitor(erika, hernan).
progenitor(manuel_II, hernan).
progenitor(erika, pamela).
progenitor(manuel_II, pamela).
progenitor(erika, leslie).
progenitor(manuel_II, leslie).
progenitor(pamela, rodrigo).
progenitor(nelson, rodrigo).


esposa(rosa, manuel_I).
esposa(kim, arturo).
esposa(mirna, raul_I).
%FAMILIA_DE_ERIKA
esposa(rosaura, eusebio).
esposa(rofina, amador).
esposa(julia, urbano).
esposa(claudia, pablo).
esposa(natalia, jose).
esposa(cleme, socimo).
esposa(mari, joel).
%CRUCE_ENTRE_FAMILIAS
esposa(erika, manuel_II).
esposa(pamela, nelson).
esposa(leslie, jorge).


esposo(manuel_I, rosa).
esposo(arturo, kim).
esposo(raul_I, mirna).
esposo(eusebio, rosaura).
esposo(amador, rofina).
esposo(urbano, julia).
esposo(pablo, claudia).
esposo(jose, natalia).
esposo(socimo, cleme).
esposo(joel, mari).
esposo(manuel_II, erika).
esposo(nelson, pamela).
esposo(jorge, leslie).




%VARONES
varon(manuel_I).
varon(arturo).
varon(raul_I).
varon(manuel_II).
varon(toshiro).
varon(raul_II).
varon(angel).
varon(hernan).
varon(nelson).
varon(jorge).
varon(rodrigo).
varon(eusebio).
varon(amador).
varon(socimo).
varon(cesar).
varon(joel).
varon(kenny).
varon(pablo).
varon(cristian).
varon(jose).
varon(urbano).
varon(jesus).




%MUJERES
mujer(rosa).
mujer(laura).
mujer(kim).
mujer(mirna).
mujer(erika).
mujer(pamela).
mujer(leslie).
mujer(rosaura).
mujer(rofina).
mujer(cleme).
mujer(diana).
mujer(mari).
mujer(claudia).
mujer(natalia).
mujer(ariana).
mujer(julia).




%PAIS_DE_NACIMIENTO 
pais(manuel_I, japon).
pais(kim, japon).
pais(toshiro, japon).

pais(rosa, espania).
pais(raul_I, espania).
pais(raul_II, alemania).
pais(angel, espania).
pais(rosaura, espania).
pais(natalia, espania).

pais(eusebio, peru).
pais(amador, peru).
pais(rofina, peru).
pais(erika, peru).
pais(socimo, peru).
pais(cleme, peru).
pais(claudia, peru).
pais(pablo, peru).
pais(urbano, peru).
pais(julia, peru).
pais(jesus, peru).
pais(cristian, peru).
pais(jose, peru).
pais(ariana, peru).
pais(cesar, peru).
pais(diana, peru).
pais(mari, peru).
pais(joel, peru).
pais(kenny, peru).
pais(hernan, peru).
pais(pamela, peru).
pais(nelson, peru).
pais(rodrigo, peru).
pais(leslie, peru).
pais(jorge, peru).
pais(manuel_II, peru).
pais(mirna, peru).
pais(arturo, peru).
pais(laura, peru).

%reglas
diferente(X, Y) :- not(igual(X, Y)).
igual(X, X).


padre(X,Y) :- progenitor(X,Y),varon(X).
madre(X,Y) :- progenitor(X,Y),mujer(X).
hijo(X,Y) :- progenitor(Y,X),varon(X).
hija(X,Y) :- progenitor(Y,X),mujer(X).




abuelo(X,Y) :- padre(X,Z),progenitor(Z,Y).
abuela(X,Y) :- madre(X,Z),progenitor(Z,Y).




bisabuela(X,Y) :-madre(X,Z),abuelo(Z,Y).
bisabuela(X,Y) :-madre(X,Z),abuela(Z,Y).
bisabuelo(X,Y) :-padre(X,Z),abuelo(Z,Y).
bisabuelo(X,Y) :-padre(X,Z),abuela(Z,Y).




nieto(X,Y) :- hijo(X,Z),progenitor(Y,Z),varon(X).
nieta(X,Y) :- hija(X,Z),progenitor(Y,Z),mujer(X).




bisnieto(X,Y) :- nieto(X,Z),progenitor(Y,Z).
bisnieta(X,Y) :- nieta(X,Z),progenitor(Y,Z).




hermano(X,Y) :- padre(Z,X),padre(Z,Y),madre(W,X),madre(W,Y),varon(X),diferente(X,Y).
hermana(X,Y) :- padre(Z,X),padre(Z,Y),madre(W,X),madre(W,Y),mujer(X),diferente(X,Y).




tio(X,Y) :- hermano(X,Z),progenitor(Z,Y).
tia(X,Y) :- hermana(X,Z),progenitor(Z,Y).




tioabuelo(X,Y) :-hermano(X,Z),abuelo(Z,Y),varon(X).
tiaabuela(X,Y) :-hermana(X,Z),abuela(Z,Y),mujer(X).




sobrino(X,Y) :- tio(Y,X),varon(X).
sobrino(X,Y) :- tia(Y,X),varon(X).
sobrina(X,Y) :- tio(Y,X),mujer(X).
sobrina(X,Y) :- tia(Y,X),mujer(X).




primohermano(X,Y) :- padre(Z,X),tio(Z,Y),varon(X).
primohermano(X,Y) :- madre(Z,X),tia(Z,Y),varon(X).
primahermana(X,Y) :- padre(Z,X),tio(Z,Y),mujer(X).
primahermana(X,Y) :- madre(Z,X),tia(Z,Y),mujer(X).




primo(X,Y) :-abuelo(Z,X),tioabuelo(Z,Y),varon(X).
primo(X,Y) :-abuela(Z,X),tiaabuela(Z,Y),varon(X).
prima(X,Y) :-abuela(Z,X),tiaabuela(Z,Y),mujer(X).
prima(X,Y) :-abuelo(Z,X),tioabuelo(Z,Y),mujer(X).




cuniado(X,Y) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),hermano(X,W),varon(X).
cuniado(Y,X) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),hermano(X,W),varon(Y).




cuniada(X,Y) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),hermana(X,W),mujer(X).
cuniada(Y,X) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),hermana(X,W),mujer(Y).




suegro(X,Y) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),progenitor(X,W),varon(X).
suegra(X,Y) :- progenitor(Y,Z),progenitor(W,Z),diferente(Y,W),progenitor(X,W),mujer(X).




yerno(X,Y) :- suegro(Y,X),varon(X).
yerno(X,Y) :- suegra(Y,X),varon(X).
nuera(X,Y) :- suegro(Y,X),mujer(X).
nuera(X,Y) :- suegra(Y,X),mujer(X).


es_consuegro(X,Y) :-(progenitor(X,Z),varon(X)),(esposo(Z,W);esposa(Z,W)),progenitor(Y,W).
es_consuegra(X,Y) :-(progenitor(X,Z),mujer(X)),(esposo(Z,W);esposa(Z,W)), progenitor(Y,W).
son_consuegros(X,Y) :-progenitor(X,Z),(esposo(Z,W);esposa(Z,W)), progenitor(Y,W).

continente(X,europa) :- pais(X,espania);pais(X,alemania).
continente(X,asia) :- pais(X,japon).
continente(X,america) :- pais(X,peru).

% Regla para determinar si X es descendiente de Y y el pais de naciemiento de cada descendiente.
descendiente(X, Y, Pais, C) :- progenitor(Y, X), pais(X, Pais), continente(X,C).
descendiente(X, Y, Pais, C) :- progenitor(Z, X), descendiente(Z, Y, _,_), pais(X, Pais), continente(X,C).
