-module(die_please1).

-export([go/0]).

-define(SLEEP_TIME, 2000).

go() ->
    %% just sleep for a while, then crash
    timer:sleep(?SLEEP_TIME),
    % matching will fail and cause an exception (on purpose)
    i_really_want_to_die = right_now.
