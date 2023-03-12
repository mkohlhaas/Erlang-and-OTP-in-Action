-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(WAIT_FOR_RESOURCES, 2500).

%%%%%%%
% API %
%%%%%%%

start(_StartType, _StartArgs) ->
  ok = ensure_contact(),
  resource_discovery:add_local_resource(simple_cache, node()), % we have simple_cache (we store node not pid!)
  resource_discovery:add_target_resource_type(simple_cache), % we want simple_cache
  resource_discovery:trade_resources(), % exchange resource information with other nodes
  timer:sleep(?WAIT_FOR_RESOURCES),
  sc_store:init(), % create Mnesia cluster
  case sc_sup:start_link() of
    {ok, Pid} ->
      sc_event_logger:add_handler(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions %
%%%%%%%%%%%%%%%%%%%%%%

% get_env ------------------------------------------------------------------------------
get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined ->
      Default;
    {ok, Value} ->
      Value
  end.

% ensure_contact -----------------------------------------------------------------------
ensure_contact() ->
  DefaultNodes = [contact1@pc1, contact2@pc1], % this is our default predefined cluster
  case get_env(simple_cache, contact_nodes, DefaultNodes) of
    [] ->
      {error, no_contact_nodes};
    ContactNodes ->
      ensure_contact(ContactNodes)
  end.

ensure_contact(ContactNodes) ->
  Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      {error, no_contact_nodes_reachable};
    _ ->
      DefaultTime = 6000,
      WaitTime = get_env(simple_cache, wait_time, DefaultTime),
      wait_for_nodes(length(Answering), WaitTime)
  end.

% wait_for_nodes -----------------------------------------------------------------------
% TODO: not sure if this is the right way to do it
wait_for_nodes(MinNodes, WaitTime) ->
  Slices = 10,
  SliceTime = round(WaitTime / Slices),
  wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
  ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
  case length(nodes()) > MinNodes of
    true -> % we joined the cluster
      ok;
    false ->
      timer:sleep(SliceTime),
      wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
  end.

% --------------------------------------------------------------------------------------
