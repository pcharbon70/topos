%%%-------------------------------------------------------------------
%%% @doc Expression Type Inference (Algorithm W)
%%%
%%% This module implements the core of Algorithm W - type inference for
%%% expressions. It generates constraints, performs unification, and
%%% implements let-polymorphism through generalization and instantiation.
%%%
%%% Expression inference returns:
%%% - The inferred type of the expression
%%% - Updated inference state (with substitutions and errors)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_infer_expr).

-export([
    infer/3,
    instantiate/2,
    generalize/3
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of an expression
%%
%% **Pattern 1 Error:** Returns {error, Error, State} for inference errors
%% since this function threads inference state through the expression
%% inference process.
-spec infer(topos_ast:expr(), topos_type_env:env(), topos_infer_state:infer_state()) ->
    {topos_types:type(), topos_infer_state:infer_state()} |
    {error, topos_type_error:type_error(), topos_infer_state:infer_state()}.

% Expression: Literal
% 42 : Int
infer({lit, Lit}, _Env, State) ->
    Type = topos_infer_utils:literal_type(Lit),
    {Type, State};

% Expression: Variable
% x : instantiate(Γ(x))
infer({var, Name}, Env, State) ->
    case topos_type_env:lookup(Env, Name) of
        {ok, Scheme} ->
            {Type, State1} = instantiate(Scheme, State),
            {Type, State1};
        none ->
            Error = topos_type_error:unbound_variable(Name),
            State1 = topos_infer_state:add_error(Error, State),
            {error, Error, State1}
    end;

% Expression: Lambda abstraction
% λx.e : α → T  where e : T in Γ[x : α]
infer({lam, Param, Body}, Env, State) ->
    % Generate fresh type variable for parameter
    {ParamType, State1} = topos_infer_state:fresh_var(State),

    % Extend environment with parameter binding
    ParamScheme = topos_type_scheme:mono(ParamType),
    Env1 = topos_type_env:extend(Env, Param, ParamScheme),

    % Infer body type
    case infer(Body, Env1, State1) of
        {BodyType, State2} ->
            % Function type: ParamType -> BodyType (pure for now)
            FunType = {tfun, ParamType, BodyType, {effect_set, []}},
            {FunType, State2};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Function application
% e1 e2 : β  where e1 : T1, e2 : T2, unify(T1, T2 → β)
infer({app, Fun, Arg}, Env, State) ->
    % Infer function type
    case infer(Fun, Env, State) of
        {FunType, State1} ->
            % Infer argument type
            case infer(Arg, Env, State1) of
                {ArgType, State2} ->
                    % Generate fresh type variable for result
                    {ResultType, State3} = topos_infer_state:fresh_var(State2),

                    % Unify function type with ArgType → ResultType
                    ExpectedFunType = {tfun, ArgType, ResultType, {effect_set, []}},
                    case topos_infer_unify:unify(FunType, ExpectedFunType, State3) of
                        {ok, _Subst, State4} ->
                            % Apply current substitution to result type
                            FinalSubst = topos_infer_state:get_subst(State4),
                            FinalResultType = topos_type_subst:apply(FinalSubst, ResultType),
                            {FinalResultType, State4};
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Let binding (non-recursive)
% let x = e1 in e2 : T2  where e1 : T1, e2 : T2 in Γ[x : ∀ᾱ.T1]
infer({'let', Name, Expr, Body}, Env, State) ->
    % Infer type of bound expression
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Apply current substitution to environment and expression type
            Subst = topos_infer_state:get_subst(State1),
            ExprType1 = topos_type_subst:apply(Subst, ExprType),

            % Generalize the type (introduce ∀ quantifiers)
            Scheme = generalize(ExprType1, Env, State1),

            % Extend environment with generalized binding
            Env1 = topos_type_env:extend(Env, Name, Scheme),

            % Infer body type
            infer(Body, Env1, State1);
        {error, _, _} = Error ->
            Error
    end;

% Expression: Let-rec binding (recursive)
% For PoC: simplified - just bind as monomorphic
infer({'letrec', Name, Expr, Body}, Env, State) ->
    % Generate fresh type variable for recursive binding
    {RecType, State1} = topos_infer_state:fresh_var(State),

    % Extend environment with monomorphic binding
    RecScheme = topos_type_scheme:mono(RecType),
    Env1 = topos_type_env:extend(Env, Name, RecScheme),

    % Infer type of expression in extended environment
    case infer(Expr, Env1, State1) of
        {ExprType, State2} ->
            % Unify recursive type with expression type
            case topos_infer_unify:unify(RecType, ExprType, State2) of
                {ok, _Subst, State3} ->
                    % Infer body with recursive binding
                    infer(Body, Env1, State3);
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: If-then-else
% if c then t else e : T  where c : Bool, t : T, e : T
infer({'if', Cond, Then, Else}, Env, State) ->
    % Infer condition type
    case infer(Cond, Env, State) of
        {CondType, State1} ->
            % Unify condition with Bool
            case topos_infer_unify:unify(CondType, {tcon, bool}, State1) of
                {ok, _Subst1, State2} ->
                    % Infer then branch
                    case infer(Then, Env, State2) of
                        {ThenType, State3} ->
                            % Infer else branch
                            case infer(Else, Env, State3) of
                                {ElseType, State4} ->
                                    % Unify both branches
                                    case topos_infer_unify:unify(ThenType, ElseType, State4) of
                                        {ok, _Subst2, State5} ->
                                            % Apply final substitution to then type
                                            FinalSubst = topos_infer_state:get_subst(State5),
                                            FinalType = topos_type_subst:apply(FinalSubst, ThenType),
                                            {FinalType, State5};
                                        {error, _, _} = Error ->
                                            Error
                                    end;
                                {error, _, _} = Error ->
                                    Error
                            end;
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Tuple construction
% (e1, ..., en) : (T1, ..., Tn)
infer({tuple, Elements}, Env, State) ->
    case infer_exprs(Elements, Env, State) of
        {Types, State1} ->
            {{ttuple, Types}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Record construction
% {l1: e1, ..., ln: en} : {l1: T1, ..., ln: Tn | closed}
infer({record, Fields}, Env, State) ->
    case infer_record_fields(Fields, Env, State) of
        {FieldTypes, State1} ->
            {{trecord, FieldTypes, closed}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Record field access
% e.field : T  where e : {..., field: T, ... | ρ}
infer({field, Expr, FieldName}, Env, State) ->
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Generate fresh type variable for field
            {FieldType, State2} = topos_infer_state:fresh_var(State1),

            % Generate fresh row variable
            {RowVar, State3} = topos_infer_state:fresh_var(State2),
            {tvar, RowVarId} = RowVar,

            % Expected record type with this field
            ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},

            % Unify expression type with expected record type
            case topos_infer_unify:unify(ExprType, ExpectedType, State3) of
                {ok, _Subst, State4} ->
                    % Apply substitution to field type
                    FinalSubst = topos_infer_state:get_subst(State4),
                    FinalFieldType = topos_type_subst:apply(FinalSubst, FieldType),
                    {FinalFieldType, State4};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Variant constructor
% C e1 ... en : [... | C T1 ... Tn | ...]
infer({variant, Constructor, Args}, Env, State) ->
    case infer_exprs(Args, Env, State) of
        {ArgTypes, State1} ->
            Type = {tvariant, [{Constructor, ArgTypes}]},
            {Type, State1};

% Expression: Variant constructor (monadic version)
% Shows how monadic combinators eliminate deep nesting
infer_m_variant(Constructor, Args, Env) ->
    InferExprs = topos_infer_monad:infer_exprs(Args, Env),
    topos_infer_monad:map(
        fun(ArgTypes) -> {tvariant, [{Constructor, ArgTypes}]} end,
        InferExprs
    ).

% Expression: Record field access (monadic version)
% Demonstrates how monadic combinators clean up nested case statements
infer_m_field_access(Expr, FieldName, Env) ->
    % Chain operations using monadic bind instead of nested cases
    topos_infer_monad:bind(
        fun(S) -> infer(Expr, Env, S) end,  % Infer the expression
        fun(ExprType) ->
            topos_infer_monad:with_fresh_var(
                fun(FieldType) ->
                    topos_infer_monad:with_fresh_var(
                        fun(RowVar) ->
                            {tvar, RowVarId} = RowVar,
                            ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},
                            
                            fun(State3) ->
                                case topos_infer_unify:unify(ExprType, ExpectedType, State3) of
                                    {ok, _Subst, State4} ->
                                        FinalSubst = topos_infer_state:get_subst(State4),
                                        FinalFieldType = topos_type_subst:apply(FinalSubst, FieldType),
                                        {FinalFieldType, State4};
                                    {error, _, _} = Error ->
                                        Error
                                end
                            end
                        end
                    )
                end
            )
        end,
        topos_infer_state:new()  % Initial state
    ).
        {error, _, _} = Error ->
            Error
    end;

% Expression: Type annotation
% (e : T) : T  where e : T' and unify(T, T')
infer({ann, Expr, AnnotType}, Env, State) ->
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Unify inferred type with annotation
            case topos_infer_unify:unify(ExprType, AnnotType, State1) of
                {ok, _Subst, State2} ->
                    % Return annotated type
                    FinalSubst = topos_infer_state:get_subst(State2),
                    FinalType = topos_type_subst:apply(FinalSubst, AnnotType),
                    {FinalType, State2};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end.

%% @doc Instantiate a type scheme by replacing quantified variables with fresh ones
-spec instantiate(topos_type_scheme:scheme(), topos_infer_state:infer_state()) ->
    {topos_types:type(), topos_infer_state:infer_state()}.
instantiate({mono, Type}, State) ->
    % Monomorphic scheme - apply current substitution
    Subst = topos_infer_state:get_subst(State),
    InstType = topos_type_subst:apply(Subst, Type),
    {InstType, State};
instantiate({poly, Quantified, Type}, State) ->
    % Polymorphic scheme - replace quantified vars with fresh ones
    {FreshVars, State1} = topos_infer_state:fresh_vars(length(Quantified), State),

    % Build substitution from quantified vars to fresh vars
    Pairs = lists:zip(Quantified, FreshVars),
    InstSubst = maps:from_list(Pairs),

    % Apply instantiation substitution to type
    Type1 = topos_type_subst:apply(InstSubst, Type),

    % Apply current substitution from state
    CurrentSubst = topos_infer_state:get_subst(State1),
    InstType = topos_type_subst:apply(CurrentSubst, Type1),

    {InstType, State1}.

%% @doc Generalize a type by quantifying over free variables
%% Variables free in the type but not in the environment are generalized
-spec generalize(topos_types:type(), topos_type_env:env(), topos_infer_state:infer_state()) ->
    topos_type_scheme:scheme().
generalize(Type, Env, State) ->
    % Apply current substitution to type
    Subst = topos_infer_state:get_subst(State),
    Type1 = topos_type_subst:apply(Subst, Type),

    % Get free variables in type
    TypeVars = topos_types:type_vars(Type1),

    % Get free variables in environment
    EnvVars = topos_type_env:ftv_env(Env),

    % Variables to generalize: in type but not in environment
    ToGeneralize = sets:subtract(TypeVars, EnvVars),

    case sets:size(ToGeneralize) of
        0 ->
            % No variables to generalize - monomorphic
            topos_type_scheme:mono(Type1);
        _ ->
            % Quantify over free variables
            QuantVars = sets:to_list(ToGeneralize),
            topos_type_scheme:poly(QuantVars, Type1)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Infer types for a list of expressions
-spec infer_exprs([topos_ast:expr()], topos_type_env:env(), topos_infer_state:infer_state()) ->
    {[topos_types:type()], topos_infer_state:infer_state()} |
    {error, topos_type_error:type_error(), topos_infer_state:infer_state()}.
infer_exprs(Exprs, Env, State) ->
    infer_exprs_acc(Exprs, Env, State, []).

infer_exprs_acc([], _Env, State, TypesAcc) ->
    {lists:reverse(TypesAcc), State};
infer_exprs_acc([E | Rest], Env, State, TypesAcc) ->
    case infer(E, Env, State) of
        {Type, State1} ->
            infer_exprs_acc(Rest, Env, State1, [Type | TypesAcc]);
        {error, _, _} = Error ->
            Error
    end.

%% @doc Infer types for record fields
-spec infer_record_fields([{atom(), topos_ast:expr()}], topos_type_env:env(),
                         topos_infer_state:infer_state()) ->
    {[{atom(), topos_types:type()}], topos_infer_state:infer_state()} |
    {error, topos_type_error:type_error(), topos_infer_state:infer_state()}.
infer_record_fields(Fields, Env, State) ->
    infer_record_fields_acc(Fields, Env, State, []).

infer_record_fields_acc([], _Env, State, FieldsAcc) ->
    {lists:reverse(FieldsAcc), State};
infer_record_fields_acc([{Label, Expr} | Rest], Env, State, FieldsAcc) ->
    case infer(Expr, Env, State) of
        {Type, State1} ->
            infer_record_fields_acc(Rest, Env, State1, [{Label, Type} | FieldsAcc]);
        {error, _, _} = Error ->
            Error
    end.


