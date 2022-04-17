:- use_module(library(lists)).

edge("City1", "City2", 25).
edge("City2", "City3", 20).
edge("City2", "City4", 40).
edge("City3", "City4", 25).
edge("City4", "City5", 15).
edge("City4", "City1", 20).
edge("City5", "City1", 10).

connected(X,Y,L) :- edge(X,Y,L) ; edge(Y,X,L).

sum_path([], A) :- A is 0, !.
sum_path([edge(_, _, T)], A) :-
       A is T, !.
sum_path([edge(_, _, T) | Tail], A) :-
       sum_path(Tail, B),
       A is B + T, !.

find_optimal(Begin, Path) :-
       search_step(Begin, [], Path).

all_cities(Cities) :-
       findall(A, edge(A, _, _), RawCities),
       list_to_set(RawCities, Cities).

search_step(_, Visited, Path) :-
       writeln("SEARCH END"),
       all_cities(Cities),
       permutation(Visited, Cities),
       Path is Visited, writeln("done"), !.
search_step(Current, Visited, Path) :-
       writeln("SEARCH MAIN"),
       closest(Current, Visited, City, _),
       write("Closest to "), write(Current), write(" is "), writeln(City),
       write("Visited = "), writeln(Visited),
%       write("Path = "), writeln(Path),
       search_step(City, [Current|Visited], Path).

cities_to_city_dist_pairs(_, [], Out) :-
       Out = [].
cities_to_city_dist_pairs(Root, [City], Out) :-
       connected(Root, City, Dist),
       Out = [[City, Dist]].
cities_to_city_dist_pairs(Root, [City | Tail], Out) :-
       connected(Root, City, Dist),
       cities_to_city_dist_pairs(Root, Tail, OutRest),
       append([[City, Dist]], OutRest, Out).

city_dist_pairs_to_cities([], Out) :-
       Out = [].
city_dist_pairs_to_cities([[City, _]], Out) :-
       Out = [City].
city_dist_pairs_to_cities([[City, _] | Tail], Out) :-
       city_dist_pairs_to_cities(Tail, InnerOut),
       append([City], InnerOut, Out).

closest(Fst, Visited, Snd, Dist) :-
       findall([City, D], connected(Fst, City, D), Output),
       city_dist_pairs_to_cities(Output, CityOutput),
       subtract(CityOutput, Visited, FittingCities),
       cities_to_city_dist_pairs(Fst, FittingCities, Fitting),
%       write("Fitting = "), writeln(Fitting),
       min_dist(Fitting, Snd, Dist), !.

min_dist([], City, Dist) :-
       City = [], Dist = [], !.
min_dist([[A, B]], City, Dist) :-
       City = [A], Dist = [B], !.
min_dist([[A, B] | Tail], City, Dist) :-
       min_dist(Tail, [City2], [Dist2]),
       (B < Dist2 -> City = [A], Dist = [B] ; City = [City2], Dist = [Dist2]), !.
   
unique([]).
unique([X|Xs]) :- \+ memberchk(X, Xs), unique(Xs).