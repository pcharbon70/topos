%%%-------------------------------------------------------------------
%%% @doc Inference State Management for Algorithm W
%%%
%%% This module manages the state used during type inference,
%%% including fresh variable generation, substitution accumulation,
%%% and error collection.
%%%
%%% The inference state is threaded through all inference operations
%%% to maintain functional purity (no process dictionary).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_infer_state).

-export([
    new/0,
    fresh_var/1,
    fresh_vars/2,
    get_subst/1,
    add_subst/2,
    compose_subst/2,
    add_error/2,
    get_errors/1,
    has_errors/1,
    get_next_var/1,
    set_next_var/2
]).

-export_type([infer_state/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(infer_state, {
    next_var :: pos_integer(),              % Next fresh type variable ID
    subst :: topos_type_subst:subst(),      % Accumulated substitution
    errors :: [topos_type_error:type_error()] % Collected errors
}).

-opaque infer_state() :: #infer_state{}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new inference state
-spec new() -> infer_state().
new() ->
    #infer_state{
        next_var = 1,
        subst = topos_type_subst:empty(),
        errors = []
    }.

%% @doc Generate a fresh type variable
%% Returns a new type variable and updated state
-spec fresh_var(infer_state()) -> {{tvar, pos_integer()}, infer_state()}.
fresh_var(#infer_state{next_var = N} = State) ->
    Var = {tvar, N},
    State1 = State#infer_state{next_var = N + 1},
    {Var, State1}.

%% @doc Generate multiple fresh type variables
%% Returns a list of new type variables and updated state
-spec fresh_vars(non_neg_integer(), infer_state()) ->
    {[{tvar, pos_integer()}], infer_state()}.
fresh_vars(Count, State) when Count >= 0 ->
    fresh_vars_acc(Count, [], State).

fresh_vars_acc(0, Acc, State) ->
    {lists:reverse(Acc), State};
fresh_vars_acc(N, Acc, State) ->
    {Var, State1} = fresh_var(State),
    fresh_vars_acc(N - 1, [Var | Acc], State1).

%% @doc Get the current substitution
-spec get_subst(infer_state()) -> topos_type_subst:subst().
get_subst(#infer_state{subst = Subst}) ->
    Subst.

%% @doc Add a substitution to the state
%% This replaces the current substitution (for single unification steps)
-spec add_subst(topos_type_subst:subst(), infer_state()) -> infer_state().
add_subst(NewSubst, #infer_state{} = State) ->
    State#infer_state{subst = NewSubst}.

%% @doc Compose a substitution with the current state substitution
%% Composes S2 with S1 (current), resulting in S2 ∘ S1
-spec compose_subst(topos_type_subst:subst(), infer_state()) -> infer_state().
compose_subst(S2, #infer_state{subst = S1} = State) ->
    Composed = topos_type_subst:compose(S2, S1),
    State#infer_state{subst = Composed}.

%% @doc Add an error to the state
-spec add_error(topos_type_error:type_error(), infer_state()) -> infer_state().
add_error(Error, #infer_state{errors = Errors} = State) ->
    State#infer_state{errors = [Error | Errors]}.

%% @doc Get all collected errors (in reverse order of occurrence)
-spec get_errors(infer_state()) -> [topos_type_error:type_error()].
get_errors(#infer_state{errors = Errors}) ->
    lists:reverse(Errors).

%% @doc Check if any errors have been collected
-spec has_errors(infer_state()) -> boolean().
has_errors(#infer_state{errors = []}) ->
    false;
has_errors(#infer_state{errors = [_|_]}) ->
    true.

%% @doc Get the next variable ID (for testing/debugging)
-spec get_next_var(infer_state()) -> pos_integer().
get_next_var(#infer_state{next_var = N}) ->
    N.

%% @doc Set the next variable ID (for testing/debugging)
-spec set_next_var(pos_integer(), infer_state()) -> infer_state().
set_next_var(N, #infer_state{} = State) when N > 0 ->
    State#infer_state{next_var = N}.
