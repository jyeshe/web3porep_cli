-module(poreppack_nif).
-include("cargo.hrl").

-export([sha256_hash/2]).
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

init() ->
    ok = ?load_nif_from_crate(poreppacknif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

sha256_hash(_Data, _MinerAddr) ->
    ?NOT_LOADED.
