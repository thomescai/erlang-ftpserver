
-module(ftpserver_sup).

-behaviour(supervisor).

-include("log.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
%% 	bifrost:start_link(memory_server, []).
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    GenBifrostServerModule = memory_server,
	Opts = [],
  	{ok, { {one_for_one, 5, 10},
         [{bifrost,
           {bifrost,
            start_link,
            [GenBifrostServerModule, Opts]},
           permanent,
           5000,
           worker,
           [bifrost]}]}}.

%% init([]) ->
%% 	{ok, { {one_for_one, 5, 10},[] } }.

