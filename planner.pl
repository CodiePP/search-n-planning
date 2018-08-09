:- module(planner, [planning/4]).


% action(Prereqs, NegPrereqs, AddState, RemoveState, Description)

% depth-first search
all_actions(A) :-
    loc_actions(A).
all_actions(A) :-
    tools_actions(A).

% breadth-first search
% (cartesian product)
bfs_actions(Action) :-
    findall( L, 
             ( loc_actions(Loc)
             , findall(T, tools_actions(T), Ts)
             , L=[Loc|Ts]
             )
           , Actions),
    member(As, Actions),
    member(Action, As).

location_text(N, [Who, _, Room2], Text) :-
    N == 1,
    swritef(Text, 'On a sunny day, "%t" visited his grandparents in their spooky castle far in the North.\nAfter he rang the bell for minutes, he was led by an ill-humoured gnome to the "%t".', [Who, Room2]).
location_text(N, Data, Text) :-
    random(Rnd),
    location_text_(Rnd, Data, N, Text).

location_text_(R, [Who, _, Room2], N, Text) :-
    R =< 0.2,
    swritef(Text, '%t "%t" rushed into the "%t"!', [N, Who, Room2]).
location_text_(R, [Who, Room1, Room2], N, Text) :-
    R =< 0.4,
    swritef(Text, '%t Why did "%t" leave the "%t" and came to the "%t"?', [N, Who, Room1, Room2]).
location_text_(R, [_, _, Room2], N, Text) :-
    R =< 0.6,
    swritef(Text, '%t Cryptically, he slid into the "%t".', [N, Room2]).
location_text_(R, [_, _, Room2], N, Text) :-
    R =< 0.7,
    swritef(Text, '%t Finally, he slid into the "%t".', [N, Room2]).
location_text_(R, [Who, Room1, Room2], N, Text) :-
    R =< 0.8,
    swritef(Text, '%t "%t" left the "%t" and went over to the "%t".', [N, Who, Room1, Room2]).
location_text_(_, [Who, _, Room2], N, Text) :-
    swritef(Text, '%t "%t" now appeared in the "%t".', [N, Who, Room2]).

loc_actions(A) :-
    adjacent(R1, R2),
    A =
    action( [person_in_room(X, R1)] % Prereqs
          , [person_in_room(X, R2)] % NegPrereqs
          , [person_in_room(X, R2)] % AddState
          , [person_in_room(X, R1)] % RemoveState
          , location_text([X,R1,R2])
        ).

adjacent(bathroom, corridor).
adjacent(livingroom, corridor).
adjacent(kitchen, corridor).
adjacent(garage, corridor).
adjacent(corridor, livingroom).
adjacent(corridor, kitchen).
adjacent(corridor, bathroom).
adjacent(corridor, garage).


tooling_text(N, Data, Text) :-
    random(Rnd),
    tooling_text_(Data, Rnd, N, Text).

% first pick up of a tool
tooling_text_([Who, What, Room], R, N, Text) :-
    R =< 0.4,
    swritef(Text, '%t "%t" took the "%t" from the "%t".', [N, Who, What, Room]).
tooling_text_([Who, What, _], _, N, Text) :-
    swritef(Text, '%t What is "%t" now doing with the "%t"!?', [N, Who, What]).
% exchange of tools
tooling_text_([Who, What1, What2, Room], R, N, Text) :-
    R =< 0.2,
    swritef(Text, '%t "%t" left "%t" in the "%t" and picked up the "%t".', [N, Who, What1, Room, What2]).
tooling_text_([Who, What1, What2, Room], R, N, Text) :-
    R =< 0.8,
    swritef(Text, '%t In the "%t",  "%t" dropped the "%t" and grabbed the "%t".', [N, Room, Who, What1, What2]).
tooling_text_([Who, What1, What2, Room], _, N, Text) :-
    swritef(Text, '%t The "%t" is now in the "%t", where "%t" left it for the "%t".', [N, What1, Room, Who, What2]).

tools_actions(A) :-
    tool(T),
    A =
    action( [person_in_room(X, R), tool_in_room(T, R)] % Prereqs
          , [tool_in_hands(X, _)] % NegPrereqs (hands are free)
          , [tool_in_hands(X, T)] % AddState
          , [tool_in_room(T, R)] % RemoveState
          , tooling_text([X,T,R])
          %%, ('person "~w" picks up tool "~w" in the "~w"~n', [X,T,R])
        ).

tools_actions(A) :-
    tool(T),
    A =
    action( [person_in_room(X, R), tool_in_hands(X, T1), tool_in_room(T, R)] % Prereqs
          , [] % NegPrereqs
          , [tool_in_room(T1, R), tool_in_hands(X, T)] % AddState  (exchange tool)
          , [tool_in_room(T, R), tool_in_hands(X, T1)] % RemoveState
          , tooling_text([X,T1,T,R])
          %%, ('"~w" has been brought to "~w". Person "~w" now holds tool "~w"~n', [T1,R,X,T])
        ).

tool(hammer).
tool(vacuum).
tool(scrub).

describe_actions(Ps) :-
    length(Ps, LL),
    describe_actions_(Ps, 1, LL).
describe_actions_([],_,_).
describe_actions_([S|R], N, LL) :-
    describe_action(N, S),
    N2 is N + 1,
    describe_actions_(R, N2, LL).
describe_action(N, location_text(Ps)) :-
    location_text(N, Ps, Text),
    writeln(Text).
describe_action(N, tooling_text(Ps)) :-
    tooling_text(N, Ps, Text),
    writeln(Text).
describe_action(_, (F,Ps)) :-
    format(F, Ps).

apply_action(Action, State, NewState) :-
    action(Prereq, Negprereq, AddState, RemoveState, _)=Action,
    intersection(Prereq, State, Prereq),  % all of prereqs are in the state
    intersection(Negprereq, State, []),  % but none of the negprepreqs
    subtract(State, RemoveState, MidState),
    append(MidState, AddState, NewState).

planning(State, Goals, NegGoals, Steps) :-
    planning_(State, Goals, NegGoals, [], Steps),
    describe_actions(Steps), !.

% reached goal
planning_(State, Goals, NegGoals, Steps0, Steps) :-
    intersection(Goals, State, Goals),
    intersection(NegGoals, State, []),
    %format("   => ~w~n", [State]),
    reverse(Steps0, Steps).

% make a new plan
planning_(State, Goals, NegGoals, Acc, Steps) :-
    %all_actions(Action),
    bfs_actions(Action),
    apply_action(Action, State, NewState),
    action(_, _, _, _, Description)=Action,
    \+ member(Description, Acc), % DRY - don't repeat yourself
    planning_(NewState, Goals, NegGoals, [Description|Acc], Steps).


do_plan(Who, StartWhere, Tools, (EndWhere, EndWhat), S) :-
    append([person_in_room(Who, StartWhere)], Tools, InitialState),
    planning(InitialState, [person_in_room(Who, EndWhere), tool_in_hands(Who, EndWhat)],
             [], S).

% move from the garage to the kitchen
% returns the list of steps applied
test1(S) :- planning(
    [person_in_room(me, bathroom)],  % initial state
    [person_in_room(me, garage)], % goal to be reached
    [],                            % cannot pass through this state
    S).

% sitting in the bathroom
% find the vacuum cleaner
test2(S) :- planning(
    [person_in_room('Little Paul', bathroom),
     tool_in_room(hammer,garage), tool_in_room(vacuum,kitchen),
     tool_in_room(scrub,corridor)
    ],  % initial state
    [person_in_room('Little Paul', bathroom), tool_in_hands('Little Paul', vacuum)],     % goal to be reached
    [],                              % cannot pass through this state
    S).

% send 'Little Paul' to fetch the hammer
% and bring to the bathroom
test3(S) :- 
  do_plan('Little Paul', bathroom, [ tool_in_room(hammer,garage),
                                     tool_in_room(vacuum,kitchen),
                                     tool_in_room(scrub,corridor) ],
          (bathroom, hammer), S).


