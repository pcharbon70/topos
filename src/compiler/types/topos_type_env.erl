%%%-------------------------------------------------------------------
%%% @doc Type Environment for Algorithm W
%%%
%%% Implements type environments (Γ) that map variable names to type schemes.
%%% Used in Algorithm W to track the types of let-bound variables and
%%% function parameters during type inference.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_env).

-export([
    empty/0,
    singleton/2,
    lookup/2,
    extend/3,
    remove/2,
    ftv_env/1,
    from_list/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type environment: mapping from variable names to type schemes
-type env() :: #{atom() => topos_type_scheme:scheme()}.

-export_type([env/0]).

%%====================================================================
%% Environment Construction
%%====================================================================

-spec empty() -> env().
%% @doc Create an empty type environment
empty() ->
    #{}.

-spec singleton(atom(), topos_type_scheme:scheme()) -> env().
%% @doc Create a type environment with a single binding
singleton(VarName, Scheme) when is_atom(VarName) ->
    #{VarName => Scheme}.

-spec from_list([{atom(), topos_type_scheme:scheme()}]) -> env().
%% @doc Create a type environment from a list of bindings
from_list(Bindings) when is_list(Bindings) ->
    maps:from_list(Bindings).

%%====================================================================
%% Environment Operations
%%====================================================================

-spec lookup(env(), atom()) -> {ok, topos_type_scheme:scheme()} | none.
%% @doc Look up a variable in the type environment
lookup(Env, VarName) ->
    case maps:find(VarName, Env) of
        {ok, Scheme} -> {ok, Scheme};
        error -> none
    end.

-spec extend(env(), atom(), topos_type_scheme:scheme()) -> env().
%% @doc Extend the environment with a new binding
%% If the variable already exists, it is shadowed (new binding takes precedence)
extend(Env, VarName, Scheme) ->
    maps:put(VarName, Scheme, Env).

-spec remove(env(), atom()) -> env().
%% @doc Remove a variable from the environment
remove(Env, VarName) ->
    maps:remove(VarName, Env).

%%====================================================================
%% Free Type Variables
%%====================================================================

-spec ftv_env(env()) -> sets:set(topos_types:type_var_id()).
%% @doc Compute free type variables in an environment
%% Returns the union of free variables from all type schemes
ftv_env(Env) ->
    maps:fold(
        fun(_VarName, Scheme, Acc) ->
            SchemeFtv = topos_type_scheme:ftv_scheme(Scheme),
            sets:union(Acc, SchemeFtv)
        end,
        sets:new(),
        Env
    ).

%%====================================================================
%% Internal Functions
%%====================================================================

% (None yet)
