-module(test_erl_app_server).
-behaviour(gen_server).

%% API
-export([start_link/0
         ,print_status/0
         ,get_status/0
         ,get_connected_nodes/0
         ,get_disconnected_nodes/0
         ,get_majority_count/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% status = startup|all|majority|minority
-record(state, {status}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
print_status() ->
    gen_server:cast(?MODULE, print_status).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    erlang:send_after(10000, erlang:self(), ping_disconnected_nodes),
    {ok, #state{status=startup}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {ok, Reply, State}.
    
handle_cast(print_status, State) ->
    io:format("current status: ~p~nmajority count: ~p~nconnected nodes: ~p~ndisconnected nodes: ~p~n", [State#state.status, get_majority_count(), get_connected_nodes(), get_disconnected_nodes()]), 
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}. 

handle_info({nodeup, NodeName, _InfoList}, State) ->
    State2 = handle_nodeup(NodeName, State),
    {noreply, State2};
    
handle_info({nodedown, NodeName, [{nodedown_reason, Reason}]}, State) ->
    State2 = handle_nodedown(NodeName, Reason, State),
    {noreply, State2};

handle_info(ping_disconnected_nodes, State) ->
    ping_disconnected_nodes(),
    erlang:send_after(10000, erlang:self(), ping_disconnected_nodes),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Received unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Arg, _State) ->
    io:format("terminate: ~p", [Arg]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions
%% ===================================================================
handle_nodeup(NodeName, State) ->
    OldStatus = State#state.status,
    case OldStatus of
        startup ->
            io:format("node ~p detected node ~p is up.~n", [erlang:node(), NodeName]),
            State;
        _ -> 
            NewStatus = get_status(),
            io:format("node ~p detected node ~p is up. previous status = '~p', current status = '~p'~n", 
                      [erlang:node(), NodeName, OldStatus, NewStatus]),
            %% restart if recover from minority
            case {OldStatus, NewStatus} of
                {minority, majority} ->
                    io:format("node ~p are about to restart because node status changed from '~p' to '~p'~n", 
                              [erlang:node(), OldStatus, NewStatus]),
                    init:restart();
                {minority, all} ->
                    io:format("node ~p are about to restart because node status changed from '~p' to '~p'~n", 
                              [erlang:node(), OldStatus, NewStatus]),

                    init:restart();
                _ ->
                    do_nothing
            end,
            State#state{status=NewStatus}
    end.

handle_nodedown(NodeName, Reason, State) ->
    OldStatus = State#state.status,
    NewStatus = get_status(),
    io:format("node ~p detected node ~p is down with reason: ~p, previous status = '~p', current status = '~p'~n", 
              [erlang:node(), NodeName, Reason, OldStatus, NewStatus]),
    NewStatus = get_status(),
    State#state{status=NewStatus}.

get_status() ->
    DisconnectedNodeCount = erlang:length(get_disconnected_nodes()),
    case DisconnectedNodeCount of
        0 -> all;
        _ ->
            ConnectedNodeCount = erlang:length(get_connected_nodes()),
            MajorityCount = get_majority_count(),
            if
                ConnectedNodeCount >= MajorityCount -> majority;
                true -> minority
            end
    end.

get_connected_nodes() ->
    [erlang:node() | erlang:nodes()].

get_disconnected_nodes() ->
    CurrentNodes = get_connected_nodes(),
    {ok, ConfigedNodes} = application:get_env(mnesia, extra_db_nodes),
    lists:subtract(ConfigedNodes, CurrentNodes).

get_majority_count() ->
    {ok, ConfigedNodes} = application:get_env(mnesia, extra_db_nodes),
    (erlang:length(ConfigedNodes) div 2) + 1.

ping_disconnected_nodes() ->
    DisconnectedNodes = get_disconnected_nodes(),
    lists:foreach(fun(Node) -> 
                      %io:format("[~p] start ping ~p~n", [erlang:localtime(), Node]),
                      net_adm:ping(Node)
                  end, DisconnectedNodes).
