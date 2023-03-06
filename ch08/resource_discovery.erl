-module(resource_discovery).

-behaviour(gen_server).

-export([start_link/0, add_target_resource_type/1, add_local_resource/2,
         fetch_resources/1, trade_resources/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

% target_resource_types ∷ List Resource_Type, e.g. [simple_cache, logger], aka "I want"/wanted list.
% local_resource_dict ∷ Dict (Resource_Type ⇒ [Resource]), e.g. simple_cache ⇒ [Pid], aka "I have".
% found_resource_dict ∷ Dict (Resource_Type ⇒ [Resource]), e.g. simple_cache ⇒ [Pid], resources matching wanted list.

-record(state, {target_resource_types, local_resource_dict, found_resource_dict}).

%%%%%%%
% API %
%%%%%%%

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

init([]) ->
  {ok,
   #state{target_resource_types = [],
          local_resource_dict = dict:new(),
          found_resource_dict = dict:new()}}.

handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_dict), State}.

handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Resource}}, State) ->
  ResourceDict = State#state.local_resource_dict,
  NewResourceDict = add_resource(Type, Resource, ResourceDict),
  {noreply, State#state{local_resource_dict = NewResourceDict}};
handle_cast(trade_resources, State) ->
  ResourceDict = State#state.local_resource_dict,
  AllNodes = [node() | nodes()],
  lists:foreach(fun(Node) ->
                   gen_server:cast({?SERVER, Node}, {trade_resources, {node(), ResourceDict}})
                end,
                AllNodes),
  {noreply, State};
handle_cast({trade_resources, {ReplyTo, Remotes}},
            #state{target_resource_types = TargetTypes,
                   local_resource_dict = Locals,
                   found_resource_dict = OldFound} =
              State) ->
  % Add resources from calling node, but only the ones in the wanted list.
  FilteredRemotes = resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      % Send the caller our local resource dictionary.
      gen_server:cast({?SERVER, ReplyTo}, {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_dict = NewFound}}.

handle_info(ok = _Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions %
%%%%%%%%%%%%%%%%%%%%%%

add_resources([{Type, Resource} | T], Dict) ->
  add_resources(T, add_resource(Type, Resource, Dict));
add_resources([], Dict) ->
  Dict.

add_resource(Type, Resource, Dict) ->
  case dict:find(Type, Dict) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, Dict);
    error ->
      dict:store(Type, [Resource], Dict)
  end.

resources_for_types(Types, ResourceDict) ->
  Fun =
    fun(Type, Acc) ->
       case dict:find(Type, ResourceDict) of
         {ok, List} -> [{Type, Resource} || Resource <- List] ++ Acc;
         error -> Acc
       end
    end,
  lists:foldl(Fun, [], Types).
