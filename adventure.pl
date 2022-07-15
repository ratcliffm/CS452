% These are predicates. start swipl - swipl adventure.pl (also make.)
:- dynamic location/2.
:- dynamic nextto/3.
:- dynamic over/1.
:- dynamic oven_on/1.
:- dynamic mixed/1.
:- dynamic putDish/1.

over(false).

% location / 2
% location(X, Y)
% X is in Y.
location(egg, duck_pen). 
location(carrot, garden). 
location(ducks, duck_pen). 
location(fox, woods). 
location(you, house).

% nextto / 3
% Placement of locations
nextto(duck_pen, yard, closed).
nextto(yard, house, open).
nextto(garden, yard, open).
nextto(yard, woods, open).

% other checks
oven_on(no).
mixed(no).
baking(no).

sym_nextto(X, Y, Z) :- nextto(X, Y, Z).
sym_nextto(X, Y, Z) :- nextto(Y, X, Z).

nearby :- 
    findall(Y, (location(you, X), sym_nextto(X, Y, _)), Z),
    write(Z), nl.
    
% Can move between two places if they
% are next to each other and open
connect(X, Y) :- sym_nextto(X, Y, open).

% Actions

% Can we build fast-travel via transitive connection?
goto(X) :-
    location(you, L),
    connect(L, X),
    retract(location(you, L)),
    assert(location(you, X)),
    write('You are in the '), write(X), nl.

goto(_) :-
    write("You can't get there from here. Try getting closer or opening a door."), nl.

openPen :-
    location(you, yard),
    retract(nextto(duck_pen, yard, _)), 
    assert(nextto(duck_pen, yard, open)).
    write("The duck pen is open."), nl.


closePen :-
    location(you, yard),
    retract(nextto(duck_pen, yard, _)), 
    assert(nextto(duck_pen, yard, closed)).
    write("The duck pen is closed."), nl.

closeFence :-
    location(you, yard),
    retract(nextto(yard, woods, open)), 
    assert(nextto(yard, woods, closed)).
    write("The fence is closed."), nl.

openFence :-
    location(you, yard),
    retract(nextto(yard, woods, _)), 
    assert(nextto(yard, woods, open)).
    write("The fence is open."), nl.

openGarden :-
    location(you, yard),
    retract(nextto(garden, yard, _)), 
    assert(nextto(garden, yard, open)).
    write("The garden fence is open."), nl.

closeGarden :-
    location(you, yard),
    retract(nextto(garden, yard, _)), 
    assert(nextto(garden, yard, closed)).
    write("The garden fence is closed."), nl.

take_egg() :-
    location(you, duck_pen),
    assert(you_have(egg)).
    write("You got the last egg! Nice job."), nl.

harvest_carrot() :-
    location(you, garden),
    location(carrot, garden),
    assert(you_h(carrot)).
    write("You got the last carrot! Nice job."), nl.

mix() :-
    location(you, house),
    meal1(egg),
    meal_has(carrot),
    assert(mixed(yes)),
    write("Cake mixed!").

preheat_oven() :-
    location(you, house),
    assert(oven_on(yes)),
    write("Oven pre-heated!").

bake() :-
    location(you, house),
    oven_on(yes),
    mixed(yes),
    assert(baking(yes)).
    write("We're cookin' now!"), nl.

add_carrot :-
    location(you, X),
    added_carrot(X).
add_carrot.

added_carrot(house) :-
    retract(you_h(carrot)),
    assert(meal_has(carrot)),
    write("Carrot Added!").

added_carrot(yard) :- 
    assert(over(true)),
    write("You dropped the last carrot in a mud puddle. You lose.").

added_carrot(garden) :- 
    assert(over(true)),
    write("You dropped the last carrot in the garden - the snails ate it. You lose.").

added_carrot(duck_pen) :- 
    assert(over(true)),
    write("Oh no, the duck ate your carrot. You lose.").

added_carrot(woods) :- 
    assert(over(true)),
    write("The fox ate your carrot. You lose.").

add_egg :-
    location(you, X),
    added_egg(X).
add_egg. 

added_egg(house) :-
    retract(you_have(egg)),
    assert(meal1(egg)),
    write("Ingredient Added!").

added_egg(yard) :-
    assert(over(true)),
    write("You dropped the last egg and it cracked! You lose.").

added_egg(garden) :-
    assert(over(true)),
    write("You dropped the egg on your garden flowers! You lose.").

added_egg(duck_pen) :-
    assert(over(true)),
    write("Hey! No throwing eggs at your ducks! You lose.").

added_egg(woods) :-
    assert(over(true)),
    write("You can't bribe the fox with an egg! You lose.").

you_have(self).
you_h(self).

go :- over(true).
go :-
    help,
    write(">> "), nl,
    catch(read(X), _, (write('Bad action'), nl)),
    catch(call(X), E, (write(E), nl)),
    write(X), nl,
    baked,
    go.

% Actions

baked :-
    location(you, house),
    baking(yes),
    assert(over(true)),
    write("Thanks for finishing the cake. Now, time to feast!"), nl.
baked.

%%% USAGE %%%
% Every location should have a usage

help :-
    location(you, X),
    usage(X).
help.

usage(house) :-
    write("Your goal is to bake a delicious carrot cake! You are only missing a couple more ingredients..."), nl,
    write("You are currently in the house."), nl,
    write("Commands: goto/1, nearby/3, preheat_oven, bake, add_egg, add_carrot, mix"), nl.

usage(yard) :-
    write("What are you doing in the yard?! No time to sunbathe when there's a cake to make."), nl,
    write("Commands: nearby, goto/1, openPen, closePen, openFence, closeFence, openGarden, closeGarden"), nl.

usage(woods) :-
    write("You are in the woods. There is a hostile fox. You want to go back to the yard."), nl,
    write("Commands: nearby, goto/1."), nl.

usage(duck_pen) :-
    write("Quack quack!"), nl,
    write("Commands: nearby, goto/1, take_egg"), nl.

usage(garden) :-
    write("You are in the garden, have a snack!"), nl,
    write("Commands: nearby, goto/1, harvest_carrot."), nl.
