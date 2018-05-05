%%%-------------------------------------------------------------------
%%% @author dawid
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 16:43
%%%-------------------------------------------------------------------
-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-record(measurement, {type, date, value}).
-record(station, {name, latitude, longitude, measurements}).

-define(EMPTY_STATION, #station{
  name = "Test", latitude = 1, longitude = 1,
  measurements = []
}).

-define(GOOD_MAP, #{
  "Test" => ?EMPTY_STATION,
  {1, 1} => ?EMPTY_STATION
}).

-define(STATION_WITH_MEASUREMENT, #station{
  name = "Test", latitude = 1, longitude = 1,
  measurements = [#measurement{type = 1, date = 1, value = 1}]
}).

createMonitor_test() ->
  Actual = pollution:createMonitor(),
  Expected = #{},
  ?assertEqual(Expected, Actual).

addStation_test() ->
  Actual = pollution:addStation("Test", {1, 1}, pollution:createMonitor()),
  ?assertEqual(?GOOD_MAP, Actual).

adding_existing_station_return_error_test() ->
  ?assertEqual({error, conflict}, pollution:addStation("Test", {1, 1}, ?GOOD_MAP)).

addValue_test() ->
  ?assertEqual(?GOOD_MAP#{"Test" := ?STATION_WITH_MEASUREMENT, {1, 1} := ?STATION_WITH_MEASUREMENT},
    pollution:addValue("Test", 1, 1, 1, ?GOOD_MAP)).

removeValue_test() ->
  MonitorWithMeasurement = pollution:addValue("Test", 1, 1, 1, ?GOOD_MAP),
  Actual = pollution:removeValue("Test", 1, 1, MonitorWithMeasurement),
  ?assertEqual(?GOOD_MAP, pollution:removeValue("Test", 1, 1, Actual)).

addOneValue_test() ->
  Ok_Value = 25,
  Ok_Date = {{2018, 4, 16}, {21, 21, 23}},
  MonitorWithMeasurement = pollution:addValue("Test", "PM2.5", Ok_Date, Ok_Value, ?GOOD_MAP),
  Actual = pollution:getOneValue("Test", "PM2.5", Ok_Date, MonitorWithMeasurement),
  ?assertEqual(Ok_Value, Actual).

getStationMean_test() ->
  Ok_Date = {{2018, 4, 16}, {21, 21, 23}},
  M0 = pollution:addStation("Some other", {2,2}, ?GOOD_MAP),
  M1 = pollution:addValue("Test", "PM2.5", Ok_Date, 1, M0),
  M2 = pollution:addValue("Test", "PM2.5", Ok_Date, 2, M1),
  M3 = pollution:addValue("Some other", "PM2.5", Ok_Date, 3, M2),
  M4 = pollution:addValue("Test", "PM10", Ok_Date, 4, M3),
  M5 = pollution:addValue("Test", "PM2.5", Ok_Date, 5, M4),
  Actual = pollution:getStationMean("Test", "PM2.5", M5),
  ?assertEqual(Actual, (1+2+5)/3).

getDailyMean_test() ->
  OkDate1 = {{2018, 4, 16}, {21, 21, 23}},
  OkDate2 = {{2018, 4, 16}, {21, 21, 23}},
  OkDate3 = {{2018, 4, 16}, {21, 21, 23}},
  NotOkDate = {{2018, 4, 15}, {21, 21, 23}},
  M0 = pollution:addStation("Some other", {2,2}, ?GOOD_MAP),
  M1 = pollution:addValue("Test", "PM2.5", OkDate1, 5, M0),
  M2 = pollution:addValue("Test", "PM10", OkDate2, 1, M1),
  M3 = pollution:addValue("Test", "PM2.5", OkDate3, 4, M2),
  M4 = pollution:addValue("Test", "PM2.5", NotOkDate, 1, M3),
  Actual = pollution:getDailyMean("PM2.5",{2018,4,16}, M4),
  ?assertEqual(Actual, (5+4)/2).