-module(test).
-compile(export_all).


init() ->
    mnesia:create_table(test, [{majority, true}]),

    {ok, ConfigedNodes} = application:get_env(mnesia, extra_db_nodes),
    lists:foreach(fun(Node) -> 
                    mnesia:add_table_copy(test, Node, ram_copies)
                  end, ConfigedNodes),

    write("key1"),
    ok.

write(Name) ->
    mnesia:transaction(fun() -> mnesia:write({test, Name, 1}) end).

read() ->
    mnesia:dirty_all_keys(test).

info() ->
    test_erl_app_server:print_status().
