%%%-------------------------------------------------------------------
%%% @author dawid
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. maj 2018 16:20
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("dawid").
-compile(export_all).

start() ->
  register(server, spawn_link(?MODULE, init, [])).

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

stop() ->
  server ! stop.

loop(Monitor) ->
  receive
    {addStation, Name, Coordinates} ->
      NewMonitor = pollution:addStation(Name, Coordinates, Monitor),
      pollution_server:properLoop(Monitor, NewMonitor);
    {addValue, Name, Type, Date, Value} ->
      NewMonitor = pollution:addValue(Name, Type, Date, Value, Monitor),
      pollution_server:properLoop(Monitor, NewMonitor);
    {removeValue, Name, Type, Date} ->
      NewMonitor = pollution:removeValue(Name, Type, Date, Monitor),
      pollution_server:properLoop(Monitor, NewMonitor);
    {getOneValue, Name, Type, Date} ->
      Value = pollution:getOneValue(Name, Type, Date, Monitor),
      io:format("Value: ~w~n", [Value]),
      loop(Monitor);
    {getStationMean, Name, Type} ->
      Value = pollution:getStationMean(Name, Type, Monitor),
      io:format("Value: ~w~n", [Value]),
      loop(Monitor);
    {getDailyMean, Type, Date, Monitor} ->
      Value = pollution:getDailyMean(Type, Date, Monitor),
      io:format("Value: ~w~n", [Value]),
      loop(Monitor);
    {getAverageFromRadius, Name, Type, Radius, Monitor} ->
      Value = pollution:getAverageFromRadius(Name, Type, Radius, Monitor),
      io:format("Value: ~w~n", [Value]),
      loop(Monitor);
    stop -> stopped
  end.

properLoop(Monitor, {error, _}) ->
  loop(Monitor);
properLoop(_, NewMonitor) ->
  loop(NewMonitor).

addStation(Name, Coordinates = {_, _}) -> server ! {addStation, Name, Coordinates}.

addValue(Name, Type, Date, Value) -> server ! {addValue, Name, Type, Date, Value}.

removeValue(Name, Type, Date) -> server ! {removeValue, Name, Type, Date}.

getOneValue(Name, Type, Date) -> server ! {getOneValue, Name, Type, Date}.

getStationMean(Name, Type) -> server ! {getStationMean, Name, Type}.

getDailyMean(Type, Date) -> server ! {getDailyMean, Type, Date}.

getAverageFromRadius(Name, Type, Radius) -> server ! {getAverageFromRadius, Name, Type, Radius}.