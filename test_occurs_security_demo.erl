%%%
%%% Security Demo - Occurs Check Protection
%%%
-module(test_occurs_security_demo).

-compile(export_all).

demo() ->
    io:format("=== Occurs Check Security Fix Demo ===\n\n"),
    
    % Test 1: extend/3 protects against infinite types
    io:format("1. Substitution extend/3 protection:\n"),
    test_extend_protection(),
    
    % Test 2: singleton/2 also protects against infinite types
    io:format("\n2. Substitution singleton/2 protection:\n"),
    test_singleton_protection(),
    
    % Test 3: Safe substitutions still work
    io:format("\n3. Safe substitutions still work:\n"),
    test_safe_substitutions(),
    
    % Test 4: Complex infinite type scenarios
    io:format("\n4. Complex infinite type protection:\n"),
    test_complex_scenarios(),
    
    io:format("\n=== Security Demo Complete ===\n").

test_extend_protection() ->
    Subst = topos_type_subst:empty(),
    
    % Simple infinite: α ≡ α
    try
        topos_type_subst:extend(Subst, 123, {tvar, 123}),
        io:format("   ✗ FAIL: Should prevent α ≡ α\n")
    catch
        error:{occurs_check, 123, {tvar, 123}} ->
            io:format("   ✓ SUCCESS: α ≡ α blocked by occurs check\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for α ≡ α\n")
    end,
    
    % Function infinite: α ≡ α -> α
    FunType = topos_types:tfun({tvar, 456}, {tvar, 456}, topos_types:empty_effects()),
    try
        topos_type_subst:extend(Subst, 456, FunType),
        io:format("   ✗ FAIL: Should prevent α ≡ α -> α\n")
    catch
        error:{occurs_check, 456, FunType} ->
            io:format("   ✓ SUCCESS: α ≡ α -> α blocked by occurs check\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for α ≡ α -> α\n")
    end.

test_singleton_protection() ->
    % Simple infinite via singleton: α ≡ α
    try
        topos_type_subst:singleton(789, {tvar, 789}),
        io:format("   ✗ FAIL: Should prevent singleton α ≡ α\n")
    catch
        error:{occurs_check, 789, {tvar, 789}} ->
            io:format("   ✓ SUCCESS: singleton α ≡ α blocked\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for singleton\n")
    end,
    
    % List infinite: α ≡ List<α>
    ListWithTVar = {tapp, {tcon, list}, {tvar, 888}},
    try
        topos_type_subst:singleton(888, ListWithTVar),
        io:format("   ✗ FAIL: Should prevent α ≡ List<α>\n")
    catch
        error:{occurs_check, 888, ListWithTVar} ->
            io:format("   ✓ SUCCESS: α ≡ List<α> blocked\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for List infinite type\n")
    end.

test_safe_substitutions() ->
    Subst = topos_type_subst:empty(),
    
    % Safe: α ≡ Int
    Result1 = topos_type_subst:extend(Subst, 111, {tcon, int}),
    case topos_type_subst:lookup(Result1, 111) of
        {ok, {tcon, int}} ->
            io:format("   ✓ SUCCESS: α ≡ Int allowed and correct\n");
        _ ->
            io:format("   ✗ FAIL: α ≡ Int not stored correctly\n")
    end,
    
    % Safe via singleton: α ≡ Bool
    Result2 = topos_type_subst:singleton(222, {tcon, bool}),
    case topos_type_subst:lookup(Result2, 222) of
        {ok, {tcon, bool}} ->
            io:format("   ✓ SUCCESS: singleton α ≡ Bool allowed and correct\n");
        _ ->
            io:format("   ✗ FAIL: singleton α ≡ Bool not stored correctly\n")
    end,
    
    % Safe: α ≡ β (different variables)
    Result3 = topos_type_subst:extend(Subst, 333, {tvar, 444}),
    case topos_type_subst:lookup(Result3, 333) of
        {ok, {tvar, 444}} ->
            io:format("   ✓ SUCCESS: α ≡ β allowed and correct\n");
        _ ->
            io:format("   ✗ FAIL: α ≡ β not stored correctly\n")
    end.

test_complex_scenarios() ->
    Subst = topos_type_subst:empty(),
    
    % Nested infinite: α ≡ (α -> Int) -> Bool
    InnerFun = topos_types:tfun({tvar, 666}, {tcon, int}, topos_types:empty_effects()),
    OuterFun = topos_types:tfun(InnerFun, {tcon, bool}, topos_types:empty_effects()),
    try
        topos_type_subst:extend(Subst, 666, OuterFun),
        io:format("   ✗ FAIL: Should prevent α ≡ (α -> Int) -> Bool\n")
    catch
        error:{occurs_check, 666, OuterFun} ->
            io:format("   ✓ SUCCESS: α ≡ (α -> Int) -> Bool blocked\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for nested infinite type\n")
    end,
    
    % Record infinite: α ≡ {x: α, y: Int} 
    RecordWithTVar = {trecord, [{x, {tvar, 777}}, {y, {tcon, int}}], closed},
    try
        topos_type_subst:extend(Subst, 777, RecordWithTVar),
        io:format("   ✗ FAIL: Should prevent α ≡ {x: α, y: Int}\n")
    catch
        error:{occurs_check, 777, RecordWithTVar} ->
            io:format("   ✓ SUCCESS: α ≡ {x: α, y: Int} blocked\n");
        _:_ ->
            io:format("   ? UNEXPECTED: Unknown error for record infinite type\n")
    end.