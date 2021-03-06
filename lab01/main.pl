% helper predicates

sibling(A, B) :-
    A \= B,
    parent(C, A),
    parent(C, B).

brother(A, B) :-
    male(A),
    sibling(A, B).

sister(A, B) :-
    female(A),
    sibling(A, B).

child(A, B) :-
    parent(B, A),
    A \= B.

father(A, B) :-
    male(A),
    parent(A, B),
    A \= B.

mother(A, B) :-
    female(A),
    parent(A, B),
    A \= B.

spouse(A, B) :-
    parent(A, C),
    parent(B, C),
    A \= B.

husband(A, B) :-
    male(A),
    spouse(A, B).

wife(A, B) :-
    female(A),
    spouse(A, B).    

child_in_law(A, B) :-
    parent(B, C),
    spouse(A, C).

sibling_in_law(A, B) :-
    sibling(A, C),
    spouse(C, B).

brother_in_law(A, B) :-
    male(A),
    sibling_in_law(A, B).

sister_in_law(A, B) :-
    female(A),
    sibling_in_law(A, B).

cousin(A, B) :-
    parent(C, A),
    parent(D, B),
    sibling(C, D),
    A \= B.

female_cousing(A, B) :-
    female(A),
    cousin(A, B).

% aunt or uncle
pibling(A, B) :-
    parent(C, B),
    sibling(A, C),
    A \= B.

% nephew or niece
nibling(A, B) :-
    child(A, C),
    sibling(C, B),
    A \= B.

grandparent(A, B) :-
    parent(A, C),
    parent(C, B),
    A \= B.

grandchild(A, B) :-
    grandparent(B, A).

greatgrandparent(A, B) :-
    parent(A, C),
    grandparent(C, B).

greatgrandchild(A, B) :-
    greatgrandparent(B, A).

% 1
greatgranddaughter(A, B) :-
    female(A),
    greatgrandchild(A, B).

% 2
male_cousin(A, B) :-
    male(A),
    cousin(A, B).

% 3
grandson(A, B) :-
    male(A),
    grandchild(A, B).

% 4
husbands_brother(A, B) :-
    husband(C, B),
    brother(A, C).

% 5
daughters_husband(A, B) :-
    male(A),
    child_in_law(A, B).

% 6
wifes_father(A, B) :-
    father(A, C),
    wife(C, B).

% 7
husbands_mother(A, B) :-
    mother(A, C),
    husband(C, B).

% 8
wifes_mother(A, B) :-
    mother(A, C),
    wife(C, B).

% 9
husbands_father(A, B) :-
    father(A, C),
    husband(C, B).

% 10
greatgrandfather(A, B) :-
    male(A),
    greatgrandparent(A, B).

% 11
nephew(A, B) :-
    male(A),
    nibling(A, B).

% 12
greatgrandmother(A, B) :-
    female(A),
    greatgrandparent(A, B).

% 13
niece(A, B) :-
    female(A),
    nibling(A, B).

% 14
wifes_sisters_husband(A, B) :-
    husband(A, C),
    wifes_sister(C, B).

% 15
aunt(A, B) :-
    female(A),
    pibling(A, B).

% 16
wifes_sister(A, B) :-
    sister(A, C),
    wife(C, B).

% 17
uncle(A, B) :-
    male(A),
    pibling(A, B).

% 18
grandfather(A, B) :-
    male(A),
    grandparent(A, B).

% 19
greatgrandson(A, B) :-
    male(A),
    greatgrandchild(A, B).

% 20
granddaughter(A, B) :-
    female(A),
    grandchild(A, B).

% 21
grandmother(A, B) :-
    female(A),
    grandparent(A, B).

% test data

female("??????????????").
female("??????????????????").
female("??????????").
female("??????????").
female("??????????").
female("??????????????").
female("????????").
female("??????????").
female("??????????????????").

male("????????????????").
male("????????????").
male("??????????????????").
male("????????").
male("????????????").
male("??????????????").
male("??????????????").
male("??????????").
male("????????????").
male("????????").

parent("??????????????", "??????????").
parent("????????????????", "??????????").
parent("??????????????????", "??????????").
parent("????????????", "??????????").
parent("??????????????????", "??????????").
parent("????????????", "??????????").
parent("??????????????????", "????????").
parent("????????????", "????????").
parent("??????????????????", "????????????").
parent("????????????", "????????????").
parent("??????????", "??????????????").
parent("??????????????????", "??????????????").
parent("??????????????", "??????????").
parent("??????????", "??????????").
parent("??????????????", "????????????").
parent("??????????", "????????????").
parent("??????????", "????????").
parent("??????????????", "????????").
parent("??????????", "??????????").
parent("??????????????", "??????????").
parent("????????????", "????????").
parent("??????????????", "????????").
parent("????????????", "??????????????????").
parent("??????????????", "??????????????????").

:- begin_tests(tests).
% 1
test(greatgranddaughter, nondet) :-
    greatgranddaughter("??????????????????", "??????????????").
% 2
test(male_cousin, nondet) :-
    male_cousin("??????????", "????????").
% 3
test(grandson, nondet) :-
    grandson("??????????", "??????????????????").
% 4
test(husbands_brother, nondet) :-
    husbands_brother("????????", "??????????????").
% 5
test(daughters_husband, nondet) :-
    daughters_husband("??????????????", "??????????????????").
% 6
test(wifes_father, nondet) :-
    wifes_father("????????????", "??????????????").
% 7
test(husbands_mother, nondet) :-
    husbands_mother("??????????????????", "??????????????").
% 8
test(wifes_mother, nondet) :-
    wifes_mother("??????????????????", "??????????????").
% 9
test(husbands_father, nondet) :-
    husbands_father("????????????", "??????????????").
% 10
test(greatgrandfather, nondet) :-
    greatgrandfather("????????????????", "??????????????????").
% 11
test(nephew, nondet) :-
    nephew("??????????", "??????????").
% 12
test(greatgrandmother, nondet) :-
    greatgrandmother("??????????????", "??????????????????").
% 13
test(niece, nondet) :-
    niece("????????", "??????????").
% 14
test(wifes_sisters_husband, nondet) :-
    wifes_sisters_husband("??????????????", "??????????????").
% 15
test(aunt, nondet) :-
    aunt("??????????", "????????").
% 16
test(wifes_sister, nondet) :-
    wifes_sister("??????????", "??????????????").
% 17
test(uncle, nondet) :-
    uncle("????????????", "????????").
% 18
test(grandfather, nondet) :-
    grandfather("??????????????????", "??????????????????").
% 19
test(greatgrandson, nondet) :-
    greatgrandson("????????", "????????????????").
% 20
test(granddaughter, nondet) :-
    granddaughter("??????????", "??????????????????").
% 21
test(grandmother, nondet) :-
    grandmother("??????????", "??????????????????").
:- end_tests(tests).
