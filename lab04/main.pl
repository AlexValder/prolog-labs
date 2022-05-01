% Variant #9 - topic 3 (dog breed selection), explanation module, table form of dialog.

start :- write("Hello. Please enter your name: "),read(N),nl,own(N).

dog_trait(Owner,longhair) :- verify(Owner, "does it have long hair? (y/n) ").
dog_trait(Owner,huge) :- verify(Owner, "is it huge? (y/n) ").
dog_trait(Owner,hangtail) :- verify(Owner, "does it have hanging tail? (y/n) ").
dog_trait(Owner,longears) :- verify(Owner, "does it have long ears? (y/n) ").
dog_trait(Owner,goodchar) :- verify(Owner, "is it good tempered? (y/n) ").
dog_trait(Owner,heavy) :- verify(Owner, "is it heavy? (y/n) ").


dog_breed(Owner, bulldog) :-
    not(dog_trait(Owner,longhair)),
    dog_trait(Owner,hangtail),
    not(dog_trait(Owner,huge)),
    dog_trait(Owner,goodchar).

dog_breed(Owner, hound) :-
    not(dog_trait(Owner,longhair)),
    dog_trait(Owner,longears),
    dog_trait(Owner,huge),
    dog_trait(Owner,goodchar),
    not(dog_trait(Owner,heavy)).

dog_breed(Owner, dane) :-
    not(dog_trait(Owner,longhair)),
    dog_trait(Owner,huge),
    dog_trait(Owner,hangtail),
    dog_trait(Owner,longears),
    dog_trait(Owner,goodchar),
    dog_trait(Owner,heavy).

dog_breed(Owner, foxterrier) :-
    not(dog_trait(Owner,longhair)),
    not(dog_trait(Owner,huge)),
    dog_trait(Owner,longears),
    dog_trait(Owner,goodchar).

dog_breed(Owner, spaniel) :-
    dog_trait(Owner,longhair),
    not(dog_trait(Owner,huge)),
    dog_trait(Owner,hangtail),
    dog_trait(Owner,longears),
    dog_trait(Owner,goodchar).

dog_breed(Owner, setter) :-
    dog_trait(Owner,longhair),
    dog_trait(Owner,huge),
    dog_trait(Owner,longears).

dog_breed(Owner, collie) :-
    dog_trait(Owner,longhair),
    dog_trait(Owner,huge),
    dog_trait(Owner,hangtail),
    dog_trait(Owner,goodchar).

dog_breed(Owner, stbernard) :-
    dog_trait(Owner,longhair),
    dog_trait(Owner,hangtail),
    dog_trait(Owner,goodchar),
    dog_trait(Owner,heavy).

dog_breed(_, stray).


dog2str(bulldog, "English Bulldog").
dog2str(hound, "Hound").
dog2str(dane, "Dutch Dane").
dog2str(foxterrier, "American Fox Terrier").
dog2str(spaniel, "Cocker Spaniel").
dog2str(setter, "Irish Setter").
dog2str(collie, "Collie").
dog2str(stbernard, "St. Bernard").
dog2str(stray, "Stray").

:- dynamic yes/1, no/1.

ask(Owner, Question) :-
    write(Owner), write(", pls answer "),write(Question),
    read(Answer),
    (
        Answer == yes ; Answer == y -> assert(yes(Question)) ; assert(no(Question)), fail
    ).

verify(O,T) :- (yes(T) -> true ; (no(T) -> fail ; ask(O, T))).

undo :- retract(yes(_)), fail.
undo :- retract(no(_)), fail.
undo.

own(Owner) :-
    dog_breed(Owner, Breed), dog2str(Breed, BreedName),
    write(Owner), write(", maybe you should pick a "), write(BreedName), writeln("."), undo.