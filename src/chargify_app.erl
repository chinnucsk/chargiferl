%%%-------------------------------------------------------------------
%%% @author Christopher Brown <>
%%% @copyright (C) 2013, Christopher Brown
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2013 by Christopher Brown <>
%%%-------------------------------------------------------------------
-module(chargify_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    chargify_sup:start_link().

stop(_State) ->
    ok.

