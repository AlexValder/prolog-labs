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

female("Євдокія").
female("Валентина").
female("Арина").
female("Ольга").
female("Ірина").
female("Наталія").
female("Юлія").
female("Дарія").
female("Анастасія").

male("Афанасій").
male("Сергій").
male("Володимир").
male("Юрій").
male("Максим").
male("Михайло").
male("Алексій").
male("Денис").
male("Дмитро").
male("Ілля").

parent("Євдокія", "Арина").
parent("Афанасій", "Арина").
parent("Валентина", "Ольга").
parent("Сергій", "Ольга").
parent("Валентина", "Ірина").
parent("Сергій", "Ірина").
parent("Валентина", "Юрій").
parent("Сергій", "Юрій").
parent("Валентина", "Максим").
parent("Сергій", "Максим").
parent("Арина", "Наталія").
parent("Володимир", "Наталія").
parent("Михайло", "Денис").
parent("Ольга", "Денис").
parent("Михайло", "Дмитро").
parent("Ольга", "Дмитро").
parent("Ірина", "Юлія").
parent("Алексій", "Юлія").
parent("Ірина", "Дарія").
parent("Алексій", "Дарія").
parent("Максим", "Ілля").
parent("Наталія", "Ілля").
parent("Максим", "Анастасія").
parent("Наталія", "Анастасія").

:- begin_tests(tests).
% 1
test(greatgranddaughter, nondet) :-
    greatgranddaughter("Анастасія", "Євдокія").
% 2
test(male_cousin, nondet) :-
    male_cousin("Денис", "Юлія").
% 3
test(grandson, nondet) :-
    grandson("Денис", "Валентина").
% 4
test(husbands_brother, nondet) :-
    husbands_brother("Юрій", "Наталія").
% 5
test(daughters_husband, nondet) :-
    daughters_husband("Михайло", "Валентина").
% 6
test(wifes_father, nondet) :-
    wifes_father("Сергій", "Михайло").
% 7
test(husbands_mother, nondet) :-
    husbands_mother("Валентина", "Наталія").
% 8
test(wifes_mother, nondet) :-
    wifes_mother("Валентина", "Михайло").
% 9
test(husbands_father, nondet) :-
    husbands_father("Сергій", "Наталія").
% 10
test(greatgrandfather, nondet) :-
    greatgrandfather("Афанасій", "Анастасія").
% 11
test(nephew, nondet) :-
    nephew("Денис", "Ірина").
% 12
test(greatgrandmother, nondet) :-
    greatgrandmother("Євдокія", "Анастасія").
% 13
test(niece, nondet) :-
    niece("Юлія", "Ольга").
% 14
test(wifes_sisters_husband, nondet) :-
    wifes_sisters_husband("Михайло", "Алексій").
% 15
test(aunt, nondet) :-
    aunt("Ірина", "Ілля").
% 16
test(wifes_sister, nondet) :-
    wifes_sister("Ірина", "Михайло").
% 17
test(uncle, nondet) :-
    uncle("Максим", "Юлія").
% 18
test(grandfather, nondet) :-
    grandfather("Володимир", "Анастасія").
% 19
test(greatgrandson, nondet) :-
    greatgrandson("Ілля", "Афанасій").
% 20
test(granddaughter, nondet) :-
    granddaughter("Дарія", "Валентина").
% 21
test(grandmother, nondet) :-
    grandmother("Арина", "Анастасія").
:- end_tests(tests).
