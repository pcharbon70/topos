%%%-------------------------------------------------------------------
%%% @doc Type Pretty-Printing
%%%
%%% Converts internal type representations to human-readable strings
%%% for error messages, REPL output, and debugging. Uses Topos-friendly
%%% terminology and syntax.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_pp).

-export([
    pp_type/1,
    pp_scheme/1,
    pp_effects/1
]).

%%====================================================================
%% Pretty-Printing Functions
%%====================================================================

-spec pp_type(topos_types:ty()) -> string().
%% @doc Pretty-print a type to a string
pp_type({tvar, Id}) ->
    "α" ++ integer_to_list(Id);

pp_type({tcon, Name}) ->
    atom_to_list(Name);

pp_type({tapp, Constructor, Args}) ->
    case Args of
        [] ->
            pp_type(Constructor);
        _ ->
            pp_type(Constructor) ++ "<" ++
            string:join([pp_type(Arg) || Arg <- Args], ", ") ++
            ">"
    end;

pp_type({tfun, From, To, Effects}) ->
    FromStr = pp_type_with_parens(From),
    ToStr = pp_type(To),
    EffStr = pp_effects(Effects),

    case EffStr of
        "" ->
            % Pure function: α -> β
            FromStr ++ " -> " ++ ToStr;
        _ ->
            % Effectful function: α -> β / {Effects}
            FromStr ++ " -> " ++ ToStr ++ " / " ++ EffStr
    end;

pp_type({trecord, Fields, RowVar}) ->
    FieldStrs = [atom_to_list(Name) ++ ": " ++ pp_type(Type)
                || {Name, Type} <- Fields],
    FieldsStr = string:join(FieldStrs, ", "),

    case RowVar of
        closed ->
            "{" ++ FieldsStr ++ "}";
        VarId when is_integer(VarId) ->
            case FieldsStr of
                "" -> "{| ρ" ++ integer_to_list(VarId) ++ "}";
                _ -> "{" ++ FieldsStr ++ " | ρ" ++ integer_to_list(VarId) ++ "}"
            end
    end;

pp_type({ttuple, Elements}) ->
    case Elements of
        [] -> "()";
        _ ->
            ElemStrs = [pp_type(Elem) || Elem <- Elements],
            "(" ++ string:join(ElemStrs, ", ") ++ ")"
    end;

pp_type({tvariant, Constructors}) ->
    ConStrs = [pp_variant_constructor(Name, Args)
              || {Name, Args} <- Constructors],
    string:join(ConStrs, " | ").

-spec pp_variant_constructor(atom(), [topos_types:ty()]) -> string().
pp_variant_constructor(Name, []) ->
    atom_to_list(Name);
pp_variant_constructor(Name, Args) ->
    ArgStrs = [pp_type_with_parens(Arg) || Arg <- Args],
    atom_to_list(Name) ++ " " ++ string:join(ArgStrs, " ").

-spec pp_type_with_parens(topos_types:ty()) -> string().
%% @doc Pretty-print a type, adding parentheses when necessary
%% Functions and variants need parentheses when appearing as arguments
pp_type_with_parens(Type) ->
    case needs_parens(Type) of
        true -> "(" ++ pp_type(Type) ++ ")";
        false -> pp_type(Type)
    end.

-spec needs_parens(topos_types:ty()) -> boolean().
needs_parens({tfun, _, _, _}) -> true;
needs_parens({tvariant, _}) -> true;
needs_parens(_) -> false.

-spec pp_effects(topos_types:effect_set()) -> string().
%% @doc Pretty-print an effect set
pp_effects({effect_set, []}) ->
    "";  % Empty effect set (pure)
pp_effects({effect_set, Effects}) ->
    EffStrs = [atom_to_list(Eff) || Eff <- Effects],
    "{" ++ string:join(EffStrs, ", ") ++ "}".

-spec pp_scheme(topos_type_scheme:scheme()) -> string().
%% @doc Pretty-print a type scheme
pp_scheme({mono, Type}) ->
    pp_type(Type);
pp_scheme({poly, Vars, Type}) ->
    VarStrs = ["α" ++ integer_to_list(V) || V <- Vars],
    "∀" ++ string:join(VarStrs, " ") ++ ". " ++ pp_type(Type).

%%====================================================================
%% Internal Functions
%%====================================================================

% (None yet)
