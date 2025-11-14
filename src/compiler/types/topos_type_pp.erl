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
pp_type(Type) ->
    lists:flatten(pp_type_iolist(Type)).

%% Internal helper that builds iolists for efficiency
pp_type_iolist({tvar, Id}) ->
    ["α", integer_to_list(Id)];

pp_type_iolist({tcon, Name}) ->
    atom_to_list(Name);

pp_type_iolist({tapp, Constructor, Args}) ->
    case Args of
        [] ->
            pp_type_iolist(Constructor);
        _ ->
            ArgStrs = [pp_type_iolist(Arg) || Arg <- Args],
            [pp_type_iolist(Constructor), "<", join(ArgStrs, ", "), ">"]
    end;

pp_type_iolist({tfun, From, To, Effects}) ->
    FromStr = pp_type_with_parens_iolist(From),
    ToStr = pp_type_iolist(To),
    EffStr = pp_effects_iolist(Effects),

    case EffStr of
        [] ->
            % Pure function: α -> β
            [FromStr, " -> ", ToStr];
        _ ->
            % Effectful function: α -> β / {Effects}
            [FromStr, " -> ", ToStr, " / ", EffStr]
    end;

pp_type_iolist({trecord, Fields, RowVar}) ->
    FieldStrs = [[atom_to_list(Name), ": ", pp_type_iolist(Type)]
                || {Name, Type} <- Fields],
    FieldsStr = join(FieldStrs, ", "),

    case RowVar of
        closed ->
            ["{", FieldsStr, "}"];
        VarId when is_integer(VarId) ->
            case FieldStrs of
                [] -> ["{| ρ", integer_to_list(VarId), "}"];
                _ -> ["{", FieldsStr, " | ρ", integer_to_list(VarId), "}"]
            end
    end;

pp_type_iolist({ttuple, Elements}) ->
    case Elements of
        [] -> "()";
        _ ->
            ElemStrs = [pp_type_iolist(Elem) || Elem <- Elements],
            ["(", join(ElemStrs, ", "), ")"]
    end;

pp_type_iolist({tvariant, Constructors}) ->
    ConStrs = [pp_variant_constructor_iolist(Name, Args)
              || {Name, Args} <- Constructors],
    join(ConStrs, " | ").

%% Internal helpers for iolists
pp_variant_constructor_iolist(Name, []) ->
    atom_to_list(Name);
pp_variant_constructor_iolist(Name, Args) ->
    ArgStrs = [pp_type_with_parens_iolist(Arg) || Arg <- Args],
    [atom_to_list(Name), " ", join(ArgStrs, " ")].

pp_type_with_parens_iolist(Type) ->
    case needs_parens(Type) of
        true -> ["(", pp_type_iolist(Type), ")"];
        false -> pp_type_iolist(Type)
    end.

-spec needs_parens(topos_types:ty()) -> boolean().
needs_parens({tfun, _, _, _}) -> true;
needs_parens({tvariant, _}) -> true;
needs_parens(_) -> false.

-spec pp_effects(topos_types:effect_set()) -> string().
%% @doc Pretty-print an effect set
pp_effects(Effects) ->
    lists:flatten(pp_effects_iolist(Effects)).

pp_effects_iolist({effect_set, []}) ->
    [];  % Empty effect set (pure)
pp_effects_iolist({effect_set, Effects}) ->
    EffStrs = [atom_to_list(Eff) || Eff <- Effects],
    ["{", join(EffStrs, ", "), "}"].

-spec pp_scheme(topos_type_scheme:scheme()) -> string().
%% @doc Pretty-print a type scheme
pp_scheme({mono, Type}) ->
    pp_type(Type);
pp_scheme({poly, Vars, Type}) ->
    VarStrs = [["α", integer_to_list(V)] || V <- Vars],
    lists:flatten(["∀", join(VarStrs, " "), ". ", pp_type_iolist(Type)]).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Join iolist elements with a separator (like string:join but for iolists)
join([], _Sep) -> [];
join([H], _Sep) -> H;
join([H|T], Sep) ->
    [H, Sep | join(T, Sep)].
