-module (marsrover).
-export ([rove/2]).
-define(COMPASS, #{
    {'N',00} => {0,1},
    {'E',09} => {1,0},
    {'S',18} => {0,-1},
    {'W',27} => {-1,0}
}).

execute(Cmd,{X,Y,Deg},{Dx,Dy}) ->
  case Cmd of
    'L' -> 
      {X,Y,Deg-9};
    'R' -> 
      {X,Y,Deg+9};
    'M' -> 
      {X+Dx,Y+Dy,Deg};
    _ -> 
      {X,Y,Deg}
  end.
  
rove(Vec={X,Y,D},Cmd) ->
  Keys = maps:keys(?COMPASS),
  case lists:keyfind(D, 1, Keys) of
    CKey = {_, Deg} ->
      {Dx,Dy} = maps:get(CKey, ?COMPASS),
      {NewX,NewY,NewDeg} = execute(Cmd, {X,Y,Deg}, {Dx,Dy}),
      {NewD,_} = lists:keyfind(NewDeg rem 36, 2, Keys),
      {NewX,NewY,NewD};
      
    false ->
      Vec
  end.

% escript provides support for running short Erlang programs without 
% having to compile them first and an easy way to retrieve the command 
% line arguments. By default, the script will be interpreted.  Execution
% of interpreted code is slower than compiled code. If much of
% the execution takes place in interpreted code it may be worthwhile to 
% compile it, even though the compilation itself will take a little while.
% 
% The header of the Erlang script in the example differs from a normal 
% Erlang module. The first line is intended to be the interpreter line,
% which invokes escript. However if you invoke the escript like this
%
% $ escript marsrover        
%
% the contents of the first line does not matter, but it cannot contain
% Erlang code as it will be ignored.
%
% Run this as escript marsrover

main([]) ->
 P1 = rove({3,3,'E'}, 'M'),
 P2 = rove(P1, 'R'),
 P3 = rove(P2, 'M'),
 io:format("~p", [rove(P3, 'L')]).
