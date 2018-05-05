%%%-------------------------------------------------------------------
%%% @author dawid
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 16:07
%%%-------------------------------------------------------------------
-module(pollution).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-author("Dawid Drozd").

%% API
-export([]).

-record(measurement, {type, date, value}).
-record(station, {name, latitude, longitude, measurements}).

createMonitor() -> #{}.

addStation(Name, {Latitude, Longitude}, Monitor) ->
  case {maps:is_key(Name, Monitor), maps:is_key({Latitude, Longitude}, Monitor)} of
    {false, false} ->
      NewStation = #station{name = Name, latitude = Latitude, longitude = Longitude, measurements = []},
      Monitor#{Name => NewStation, {Latitude, Longitude} => NewStation};
    _ -> {error, conflict}
  end.

addValue(Name, Type, Date, Value, Monitor) ->
  case maps:get(Name, Monitor, not_found) of
    not_found -> {error, not_found};
    OldStation ->
      NewStation = OldStation#station{measurements =
      [#measurement{type = Type, date = Date, value = Value} | OldStation#station.measurements]},
      maps:map(fun(_, V) when V == OldStation -> NewStation; (_, T) -> T end, Monitor)
  end.

removeValue(Name, Type, Date, Monitor) ->
  case maps:get(Name, Monitor, not_found) of
    not_found -> {error, not_found};
    OldStation = #station{measurements = Measurements} ->
      NewMeasurements = lists:filter(fun(#measurement{type = T, date = D}) when T == Type, D == Date -> false;
        (_) -> true end, Measurements),
      NewStation = OldStation#station{measurements = NewMeasurements},
      maps:map(fun(_, V) when V == OldStation -> NewStation; (_, T) -> T end, Monitor)
  end.

getOneValue(Name, Type, Date, Monitor) ->
  case maps:get(Name, Monitor, not_found) of
    not_found -> {error, not_found};
    Station -> Measurement = lists:filter(fun(M) ->
      (M#measurement.type == Type andalso M#measurement.date == Date) end, Station#station.measurements),
      case Measurement of
        [Found] -> Found#measurement.value;
        [] -> {error, not_found}
      end
  end.

getStationMean(Name, Type, Monitor) ->
  case maps:get(Name, Monitor, not_found) of
    not_found -> {error, not_found};
    Station -> Measurements = lists:filter(fun(M) -> (M#measurement.type == Type) end, Station#station.measurements),
      case Measurements of
        [] -> {error, no_measurements};
        _ -> Values = lists:map(fun(M) -> M#measurement.value end, Measurements),
          lists:sum(Values) / length(Values)
      end
  end.

getDailyMean(Type, Date, Monitor) ->
  Measurements = lists:flatmap(fun(K) -> K#station.measurements end, maps:values(Monitor)),
  Filtered = lists:filter(fun(#measurement{type = T, date = {D, _}}) when T == Type, D == Date -> true;
    (_) -> false end, Measurements),
  case lists:map(fun(K) -> K#measurement.value end, Filtered) of
    [] -> {error, no_measurements};
    Values -> lists:sum(Values) / erlang:length(Values)
  end.

getAverageFromRadius(Name, Type, Radius, Monitor) ->
  case maps:get(Name, Monitor, not_found) of
    not_found -> {error, not_found};
    Station ->
      Coordinates = {Station#station.latitude, Station#station.longitude},
      Filtered = lists:filter(fun(V) ->
        isWithinRadius(Coordinates, {V#station.latitude, V#station.longitude}, Radius) end, maps:values(Monitor)),
      Flattened = lists:flatmap(fun(V) -> V#station.measurements end, Filtered),
      case lists:filtermap(fun(V) -> V#measurement.type == Type end, Flattened) of
        [] -> {error, no_measurements};
        FilteredByType ->
          Mapped = lists:map(fun (V) -> V#measurement.value end, FilteredByType),
          lists:sum(Mapped) / erlang:length(Mapped)
      end
  end.

isWithinRadius(Coordinates1, Coordinates2, Radius) ->
  {Lat1, Long1} = Coordinates1,
  {Lat2, Long2} = Coordinates2,
  math:sqrt(
    math:pow(Lat1 - Lat2, 2)
      + math:pow(Long1 - Long2, 2)
  ) * 73 < Radius.