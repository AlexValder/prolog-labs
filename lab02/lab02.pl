:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).

% 9
husbands_father(A, B) :-
    father(A, C),
    husband(C, B).

father(A, B) :-
    male(A),
    parent(A, B),
    A \= B.

spouse(A, B) :-
    parent(A, C),
    parent(B, C),
    A \= B.

husband(A, B) :-
    male(A),
    spouse(A, B).

main:- print_menu, input_loop.

print_menu :-
    nl,
    writeln("1. Append to the DB."),
    writeln("2. Save the DB to file."),
    writeln("3. Load DB from file."),
    writeln("4. Show current DB."),
    writeln("5. Find husband's father."),
    writeln("0. Exit.").

input_loop :-
    read(X), entry(X).

entry(1) :-
    append_to_db, nl, main.
entry(2) :-
    save_db, nl, main.
entry(3) :-
    load_db, nl, main.
entry(4) :-
    print_all, nl, main.
entry(5) :-
    try_find_husbands_father, nl, main.
entry(0) :-
    writeln("Exiting..."), halt(0).
entry(X) :-
    write("Unknown value: "), writeln(X), nl, main.
    
% adding to db
append_to_db :-
    writeln("Please choose base predicate:"),
    writeln("1. parent"),
    writeln("2. male"),
    writeln("3. female"),
    writeln("0. [Back]"),
    write("Enter 1-3: "), read(X),
    process_new_entry(X), main.

process_new_entry(1) :- % parent
    writeln("Enter parent's name:"), read(Parent),
    writeln("Enter child's name:"), read(Child),
    assert(parent(Parent, Child)),
    append_to_db.
process_new_entry(2) :- % male
    writeln("Enter name: "), read(Name),
    assert(male(Name)),
    append_to_db.
process_new_entry(3) :- % female
    writeln("Enter name: "), read(Name),
    assert(female(Name)),
    append_to_db.
process_new_entry(4) :- % back
    main.
process_new_entry(X) :- % unknown
    X=X, write("Unknown option: "), writeln(X), append_to_db.

% saving the db
save_db :-
    writeln("Please enter filename: "), read(Filename),
    writeln("Saving the DB..."),
    catch_with_backtrace(
        (
            open(Filename, write, File),
            with_output_to(File,
                (
                    catch(listing(parent/2), _, nl),
                    catch(listing(female/1), _, nl),
                    catch(listing(male/1), _, nl)
                )),
            close(File),
            write("Saved to "), write(Filename), writeln("!")
        ), _, writeln("Failed to save DB :(")),
    main.

% loading the db
load_db :-
    writeln("Please enter filename: "), read(Filename),
    writeln("Loading the DB..."),
    abolish_all,
    catch_with_backtrace(consult(Filename), _, "Failed to consult the file."),
    write("Loaded from "), write(Filename), writeln("!"),
    main.

abolish_all :-
    abolish(parent/2),
    abolish(male/1),
    abolish(female/1).

% print all
print_all :-
    writeln("All existing predicates:"),
    catch(listing(parent/2), _, writeln("No predicates of \"parent\"")),
    catch(listing(female/1), _, writeln("No predicates of \"female\"")),
    catch(listing(male/1), _, writeln("No predicates of \"male\"")),
    main.

% search
try_find_husbands_father :-
    writeln("Enter your name:"),
    read(Name),
    findall(HusbandsFather, husbands_father(HusbandsFather, Name), RawList),
    list_to_set(RawList, ResultSet),
    process_results_of_search(ResultSet, Name),
    main.

process_results_of_search([], _) :-
    writeln("Failed to find anything :("), nl.
process_results_of_search([Res], Name) :-
    write("Found! "), write(Res), write(" is husband's father of "), writeln(Name), nl.
process_results_of_search(Res, Name) :-
    write("Found several results for "), writeln(Name), writeln(Res).