-module(user_default).
-include_lib("chargify.hrl").

-export([cstart/2]).

cstart(Subdomain, ApiSecret) when is_list(Subdomain) andalso is_list(ApiSecret) ->
    apply(chargify, start, [#chargify_state{subdomain=Subdomain, api_secret=ApiSecret}]).


