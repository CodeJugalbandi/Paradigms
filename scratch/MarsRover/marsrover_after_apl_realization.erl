-module (marsrover_after_apl_realization).
-export ([rove/2]).

execute(Cmd,{X,Y,Deg},{Dx,Dy}) ->
  case Cmd of
    'L' -> 
      {X,Y,Deg-1};
    'R' -> 
      {X,Y,Deg+1};
    'M' -> 
      {X+Dx,Y+Dy,Deg};
    _ -> 
      {X,Y,Deg}
  end.
  
rove(Vec={X,Y,D},Cmd) ->
  Directions = ['N', 'E', 'S', 'W'],
  case index_of(D, Directions) of
    not_found -> Vec;
    Index -> 
     {Dx,Dy} = lists:nth(Index, [{0,1},{1,0},{0,-1},{-1,0}]),
     {NewX,NewY,NewIndex} = execute(Cmd, {X,Y,Index}, {Dx,Dy}),
     NewD = case NewIndex of
       0 -> 4;
       5 -> 1;
       _ -> NewIndex
     end,
     {NewX,NewY,lists:nth(NewD, Directions)}
  end.
  
  
index_of(Item, List) -> 
  index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tail], Index) -> index_of(Item, Tail, Index + 1).

%1> P1 = marsrover_after_apl_realization:rove({3,3,'E'}, 'M').
%2> P2 = marsrover_after_apl_realization:rove(P1, 'R').
%3> P3 = marsrover_after_apl_realization:rove(P2, 'M').
%4> marsrover_after_apl_realization:rove(P3, 'L').
% {4,2,'E'}