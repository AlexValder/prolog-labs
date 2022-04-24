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
       search_step(Begin, [], [Last|RawPath]),
       shortest(Last, Begin, [_|WayBack], _),
       reverse([Last|RawPath], WayThere),
       append(WayThere, WayBack, Path).

city_reverse([], []).
city_reverse([L|Ls], [city_reverse(Ls) | L]).

all_cities(Cities) :-
       findall(A, connected(A, _, _), RawCities),
       list_to_set(RawCities, Cities).

search_step(_, Visited, Path) :-
       all_cities(Cities),
       permutation(Visited, Cities),
       Path is Visited, !.
search_step(Current, Visited, Path) :-
       closest(Current, Visited, CityList, _),
       (
              CityList = [] -> Path = [Current|Visited] ; 
              CityList = [C] -> search_step(C, [Current|Visited], Path)
       ), !.
search_step(Current, Visited, Path) :-
       closest(Current, Visited, [], _),
       Path is [Current|Visited].

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
       min_dist(Fitting, Snd, Dist), !.

min_dist([], City, Dist) :-
       City = [], Dist = [], !.
min_dist([[A, B]], City, Dist) :-
       City = [A], Dist = [B], !.
min_dist([[A, B] | Tail], City, Dist) :-
       min_dist(Tail, [City2], [Dist2]),
       (B < Dist2 -> City = [A], Dist = [B] ; City = [City2], Dist = [Dist2]), !.

path(A,B,Path,Len) :-
       travel(A,B,[A],Q,Len),
       reverse(Q,Path).

travel(A,B,P,[B|P],L) :- 
       connected(A,B,L).
travel(A,B,Visited,Path,L) :-
       connected(A,C,D),
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path,L1),
       L is D+L1.

shortest(A, B, Path, Length) :-
       setof([P,L], path(A,B,P,L), Set),
       Set = [_|_], % fail if empty
       minimal(Set, [Path,Length]).
    
minimal([F|R],M) :- min(R,F,M).
    
% minimal path
min([], M, M).
min([[P,L]|R], [_,M], Min) :- L < M, !, min(R, [P,L], Min). 
min([_|R], M, Min) :- min(R, M, Min).