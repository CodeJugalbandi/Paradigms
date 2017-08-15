-module (marsrover).
-export ([rove/2]).
-define(ROVER, 
  #{
    'N' => 
       #{'L' => fun({X,Y,_}) -> {X,Y,'W'} end, 
         'R' => fun({X,Y,_}) -> {X,Y,'E'} end, 
         'M' => fun({X,Y,D}) -> {X,Y+1,D} end },
    'E' =>           
       #{'L' => fun({X,Y,_}) -> {X,Y,'N'} end, 
         'R' => fun({X,Y,_}) -> {X,Y,'S'} end, 
         'M' => fun({X,Y,D}) -> {X+1,Y,D} end },
    'W' =>           
       #{'L' => fun({X,Y,_}) -> {X,Y,'S'} end, 
         'R' => fun({X,Y,_}) -> {X,Y,'N'} end, 
         'M' => fun({X,Y,D}) -> {X-1,Y,D} end },
    'S' =>           
       #{'L' => fun({X,Y,_}) -> {X,Y,'E'} end, 
         'R' => fun({X,Y,_}) -> {X,Y,'W'} end, 
         'M' => fun({X,Y,D}) -> {X,Y-1,D} end }
}).

rove(Vector, Cmds) -> 
  lists:foldl(fun(Cmd, Vec={_X,_Y,D}) -> 
    Function = maps:get(Cmd, maps:get(D, ?ROVER)),
    Function(Vec)
  end, Vector, Cmds).

%1> marsrover:rove({3,3,'E'}, ['M','R','M','L']).
% {4,2,'E'}