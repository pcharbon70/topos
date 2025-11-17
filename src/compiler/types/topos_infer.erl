%%%-------------------------------------------------------------------
%%% @doc Algorithm W Type Inference Orchestrator
%%%
%%% This module provides the top-level API for Algorithm W type inference,
%%% orchestrating all the inference components (expression inference,
%%% unification, generalization, instantiation) into a cohesive workflow.
%%%
%%% The main entry points provide:
%%% - Single expression inference
%%% - Program-level type checking
%%% - Error collection and formatting
%%%
%%% This is the public interface for the type inference system.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_infer).

-export([
    infer_expr/1,
    infer_expr/2,
    check_program/1,
    infer_expr_with_env/2,
    infer_lambda/3,
    check_type/2,
    format_errors/1
]).

-export_type([
    result/0,
    program_result/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% @doc Result of single expression inference
-type result() ::
    {ok, topos_types:type()} |
    {error, [topos_type_error:type_error()]}.

%% @doc Result of program type checking
-type program_result() ::
    {ok, #{atom() => topos_types:type()}} |
    {error, [topos_type_error:type_error()]}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of an expression with empty environment
%% This is the most convenient entry point for simple cases
-spec infer_expr(topos_ast:expr()) -> result().
infer_expr(Expr) ->
    infer_expr(Expr, topos_type_env:empty()).

%% @doc Infer the type of an expression with provided environment
%% This is the main Algorithm W workflow:
%% 1. Initialize inference state
%% 2. Infer expression type
%% 3. Handle errors or return result
-spec infer_expr(topos_ast:expr(), topos_type_env:env()) -> result().
infer_expr(Expr, Env) ->
    infer_expr_with_env(Expr, Env).

%% @doc Internal implementation that does the actual work
-spec infer_expr_with_env(topos_ast:expr(), topos_type_env:env()) -> result().
infer_expr_with_env(Expr, Env) ->
    % Step 1: Initialize fresh inference state
    State0 = topos_infer_state:new(),
    
    % Step 2: Perform expression inference
    case topos_infer_expr:infer(Expr, Env, State0) of
        {Type, State1} ->
            % Step 3: Check for accumulated errors
            case topos_infer_state:has_errors(State1) of
                false ->
                    % Step 4: Apply final substitution and return type
                    FinalSubst = topos_infer_state:get_subst(State1),
                    FinalType = topos_type_subst:apply(FinalSubst, Type),
                    {ok, FinalType};
                true ->
                    % Return all collected errors
                    Errors = topos_infer_state:get_errors(State1),
                    {error, Errors}
            end;
        {error, Error, State1} ->
            % Single error returned - add to error list
            AllErrors = [Error | topos_infer_state:get_errors(State1)],
            {error, AllErrors}
    end.

%% @doc Type check a complete program
%%
%% A program is a list of let/rec bindings that should be
%% type checked in order, simulating top-level definitions.
%%
%% Returns either a map of variable names to their inferred types
%% or a list of errors.
%%
%% Example program:
%% ```
%% [
%%   {'letrec', id, {'lam', 'x', {'var', 'x'}}}, % let rec id = λx. x
%%   {'letrec', const, {'lam', 'x', {'lit', {int, 42}}}}  % let rec const = λx. 42
%% ]
%% ```
-spec check_program([{atom(), topos_ast:expr()}]) -> program_result().
check_program(Bindings) ->
    check_program(Bindings, topos_type_env:empty(), #{}).

%% @doc Internal implementation for program checking
-spec check_program([{atom(), topos_ast:expr()}], topos_type_env:env(), #{atom() => topos_types:type()}) -> program_result().
check_program([], _Env, Types) ->
    {ok, Types};
check_program([{Name, Expr} | Rest], Env, Types) ->
    case infer_expr_with_env(Expr, Env) of
        {ok, Type} ->
            % Add binding to environment for remaining expressions
            Scheme = topos_type_scheme:mono(Type),
            Env1 = topos_type_env:extend(Env, Name, Scheme),
            Types1 = Types#{Name => Type},
            check_program(Rest, Env1, Types1);
        {error, Errors} ->
            {error, Errors}
    end.

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

%% @doc Infer a lambda expression with the given parameter type
%% This is a helper for common patterns
-spec infer_lambda(atom(), topos_types:type(), topos_ast:expr()) -> result().
infer_lambda(ParamName, ParamType, Body) ->
    % Create environment with the parameter
    ParamScheme = topos_type_scheme:mono(ParamType),
    Env = topos_type_env:extend(topos_type_env:empty(), ParamName, ParamScheme),
    
    % Infer the body
    case infer_expr_with_env(Body, Env) of
        {ok, BodyType} ->
            % Create function type
            Effects = topos_types:empty_effects(),
            FunType = topos_types:tfun(ParamType, BodyType, Effects),
            {ok, FunType};
        {error, Errors} ->
            {error, Errors}
    end.

%% @doc Check if an expression has a specific type (type annotation check)
%% Returns true if the inferred type matches the expected type
-spec check_type(topos_ast:expr(), topos_types:type()) -> boolean().
check_type(Expr, ExpectedType) ->
    case infer_expr(Expr) of
        {ok, InferredType} ->
            % Try to unify the inferred type with expected type
            State0 = topos_infer_state:new(),
            case topos_infer_unify:unify(InferredType, ExpectedType, State0) of
                {ok, _Subst, _State1} -> true;
                {error, _Error, _State1} -> false
            end;
        {error, _Errors} ->
            false
    end.

%% @doc Format type errors for human consumption
%% Provides a clean, readable format for type error messages
-spec format_errors([topos_type_error:type_error()]) -> string().
format_errors(Errors) ->
    {_, Strings} = lists:mapfoldl(
        fun(Error, Index) ->
            ErrorStr = topos_type_error:format(Error),
            Message = io_lib:format("~b. ~s", [Index + 1, ErrorStr]),
            {lists:flatten(Message), Index + 1}
        end,
        0,
        Errors
    ),
    string:join(Strings, "\n").

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @doc Create a fresh type environment with common primitives
%% This is useful for testing and interactive use
-spec fresh_env_with_primitives() -> topos_type_env:env().
fresh_env_with_primitives() ->
    % Add common primitive types and functions
    Primitives = [
        {int, topos_type_scheme:mono(topos_types:tcon(int))},
        {float, topos_type_scheme:mono(topos_types:tcon(float))},
        {bool, topos_type_scheme:mono(topos_types:tcon(bool))},
        {string, topos_type_scheme:mono(topos_types:tcon(string))}
    ],
    lists:foldl(
        fun({Name, Type}, Env) ->
            topos_type_env:extend(Env, Name, Type)
        end,
        topos_type_env:empty(),
        Primitives
    ).