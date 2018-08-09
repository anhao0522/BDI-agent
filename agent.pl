% COMP9414 Project 3,Option 2: Prolog(BDI Agent)
% Group number: 570
% agent.pl

% get agent position as a goal in path search.

goal(X/Y) :-
    agent_at(X,Y).

% rule s for path search.
% For path search, we use agent current position as goal position and start from a stone or monster.
% costs are 1 if move to land or stoned already dropped.
s(X/Y, X1/Y1, 1) :-        
    dis(X/Y, X1/Y1, 1),
    land_or_dropped(X1,Y1).

% costs are 1000 if move to water.
s(X/Y, X1/Y1, 1000) :-    
    dis(X/Y, X1/Y1, 1),
    not(land_or_dropped(X1,Y1)).

dis(X/Y, X1/Y1, D) :-
    X1 is X,
    Y1 is Y-D.
dis(X/Y, X1/Y1, D) :-
    X1 is X,
    Y1 is Y+D.
dis(X/Y, X1/Y1, D) :-
    X1 is X-D,
    Y1 is Y.
dis(X/Y, X1/Y1, D) :-
    X1 is X+D,
    Y1 is Y.

% find the initial location where we need drop a stone.

initial_intentions(intents(Int,[])) :-
    monster(X,Y),
    solve(X/Y,Sol,_,_),
    concat(Sol,[],Int).

% convert path to required form [[goal(X1,Y1),[]], ... , [goal(Xn,Yn),[]]]
concat([], List2, List2).

concat([Item | Tail1], List2, [[goal(X,Y),[]] | Concat_Tail1_List2]) :-
    Item=X/Y,
    not(land(X,Y)),
    concat(Tail1, List2, Concat_Tail1_List2).
      
concat([Item | Tail1], List2, Concat_Tail1_List2) :-
    Item=X/Y,
    land(X,Y),
    concat(Tail1, List2, Concat_Tail1_List2).

% convert percepts into corresponding list of goals in form of goal(X,Y).

trigger([],[]).

trigger([stone(X,Y)|List], [goal(X,Y)|List1]):-
 trigger(List,List1).

% put goal into Int_pick

incorporate_goals(Goals,intents(Int_drop,Int_pick),intents(Int_drop,Result)):-
    incorporate_goals_c(Goals,intents(Int_drop,Int_pick),[Int_drop,Int_pick],Result).

incorporate_goals_c([],intents(_,Intentions3),[_,Intentions2],Intentions2):-
    Intentions3 = Intentions2.

% put goal into Int_pick if there is a valid path to pick this stone
% and this goal is not in Int_pick.
incorporate_goals_c([goal(X1,Y1)|Goals], intents(Int_drop,Int_pick), [Int_drop,_],Result) :-
    not(member([goal(X1,Y1),_],Int_pick)),
    goal(_/_),
    solve(X1/Y1,Path,_,_),
    check_path(Path),
    length(Path,Len),
    find_position(X1/Y1,Len,Int_pick,Intentions2),
    incorporate_goals_c(Goals, intents(Int_drop,Intentions2), [Int_drop,Intentions2],Result).

% skip this goal if there is  not a valid path to pick this stone.
incorporate_goals_c([goal(X1,Y1)|Goals], intents(Int_drop,Int_pick), [Int_drop,Intentions1],Result) :-
    not(member([goal(X1,Y1),_],Int_pick)),
    goal(_/_),
    solve(X1/Y1,Path,_,_),
    not(check_path(Path)),
    incorporate_goals_c(Goals, intents(Int_drop,Intentions1), [Int_drop,Intentions1],Result).

% skip this goal if this goal is already in Int_pick.
incorporate_goals_c([goal(X1,Y1)|Goals], intents(Int_drop,Int_pick), [Int_drop,Intentions1],Result) :-
    member([goal(X1,Y1),_],Int_pick),
    incorporate_goals_c(Goals, intents(Int_drop,Intentions1), [Int_drop,Intentions1],Result).

% check wether the path is valid or not.

check_path([]).

check_path([X/Y|Path]) :-
    land_or_dropped(X,Y),
    check_path(Path).

% find a correct position to insert this goal into Int_pick.

find_position(X1/Y1,_,[],[[goal(X1,Y1),[]]]).

% if agent at the position where there is a stone, just skip this goal.
find_position(X1/Y1,Len,[[goal(X,Y),Rest1]|Rest],[[goal(X,Y),Rest1]|NewL]):-
    agent_at(X,Y),
    find_position(X1/Y1,Len,Rest,NewL).

find_position(X1/Y1,Len,[[goal(X,Y),Rest1]|Rest],[[goal(X,Y),Rest1]|NewL]) :-
    goal(_/_),
    solve(X/Y,Path,_,_),
    length(Path,Len1),
    Len > Len1,
    find_position(X1/Y1,Len,Rest,NewL).

find_position(X1/Y1,Len,[[goal(X,Y),Rest1]|Rest],[[goal(X1,Y1),[]],[goal(X,Y),Rest1]|Rest]) :-
    goal(_/_),
    solve(X/Y,Path,_,_),
    length(Path,Len1),
    Len < Len1.

find_position(X1/Y1,Len,[[goal(X,Y),Rest1]|Rest],[[goal(X1,Y1),[]],[goal(X,Y),Rest1]|Rest]) :-
    goal(_/_),
    solve(X/Y,Path,_,_),
    length(Path,Len1),
    Len = Len1.

% take agent current Intentions and computes an action to be taken by the agent 
% as well as the updated Intentions.

% if the agent is currently holding a stone, 
% select the first intention in the list Int_drop of dropping intertions.
get_action(intents([[goal(X,Y),_]|Rest],Int_pick), intents([[goal(X,Y),Plan]|Rest],Int_pick), Action):-
    agent_stones(1),
    goal(_/_),
    solve(X/Y,[_|Rest1],_,_),
    get_drop_action(Rest1,List),
    Tmp = List,
    Tmp = [Action|Plan],
    applicable(Action).

% otherwise, if the list Int_pick of picking intentions is not empty, 
% then select its first item.
get_action(intents(Int_drop,[[goal(X,Y),_]|Rest]),intents(Int_drop,[[goal(X,Y),Plan]|Rest]),Action):-
    agent_stones(0),
    not(agent_at(X,Y)),
    goal(_/_),
    solve(X/Y,[_|Rest1],_,_),
    get_pick_action(Rest1,List),
    Tmp = List,
    Tmp = [Action|Plan],
    applicable(Action).

% if currently agent stands on a stone, just move to valid adjacent position. 
get_action(intents(Int_drop,[[goal(X,Y),Plan]|Rest]),intents(Int_drop,[[goal(X,Y),Plan]|Rest]),move(X1,Y1)):-
    agent_stones(0),
    agent_at(X,Y),
    agent_move(X,Y,X1,Y1),
    applicable(move(X1,Y1)).

% otherwise, no intention is selected.
get_action(intents(Int_drop,Int_pick),intents(Int_drop,Int_pick),move(X,Y)):-
    Int_pick = [],
    agent_stones(0),
    agent_at(X,Y),
    applicable(move(X,Y)).

% convert path into correct action plan.

get_drop_action([X/Y|[]],[drop(X,Y)]).

get_drop_action([X/Y|Rest],[move(X,Y)|List]):-
    get_drop_action(Rest,List).

get_pick_action([X/Y|[]],[pick(X,Y)]).

get_pick_action([X/Y|Rest],[move(X,Y)|List]):-
    get_pick_action(Rest,List).

% for special move that if currently agent stands on a stone, 
% just move to valid adjacent position. 

agent_move(X,Y,X1,Y1):-
    X1 is X +1,
    Y1 is Y,
    land_or_dropped(X1,Y1).

agent_move(X,Y,X1,Y1):-
    X1 is X -1,
    Y1 is Y,
    land_or_dropped(X1,Y1).

agent_move(X,Y,X1,Y1):-
    X1 is X,
    Y1 is Y+1,
    land_or_dropped(X1,Y1).

agent_move(X,Y,X1,Y1):-
    X1 is X,
    Y1 is Y-1,
    land_or_dropped(X1,Y1).

% update agent intentions based on observation.

update_intentions(at(_,_), Intentions, Intentions).
update_intentions(picked(_,_), intents(Int_drop,[_|Rest]), intents(Int_drop,Rest)).
update_intentions(dropped(_,_), intents([_|Rest],Int_pick), intents(Rest,Int_pick)).


%following code is copied from ass2.

% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
    %consult(pathsearch), % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N),!.

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).



% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).

