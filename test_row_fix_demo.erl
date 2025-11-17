%%%
%%% Demo script showing the row variable binding fixes
%%%
-module(test_row_fix_demo).

-compile(export_all).

demo() ->
    io:format("=== Row Variable Binding Fix Demo ===\n\n"),
    
    % Test 1: Row variable vs closed record (the main bug fix)
    io:format("1. Row variable vs closed record:\n"),
    RecWithRow = {trecord, [{x, {tcon, int}}], 123},
    ClosedRec = {trecord, [{x, {tcon, int}}], closed},
    
    case topos_infer_unify:unify_types(RecWithRow, ClosedRec) of
        {ok, Subst1} ->
            io:format("   ✓ SUCCESS: Row variable properly bound\n"),
            io:format("   Substitution: ~p\n", [Subst1]);
        {error, Error1} ->
            io:format("   ✗ FAILED: ~p\n", [Error1])
    end,
    
    % Test 2: Different row variables (substitution composition fix)
    io:format("\n2. Different row variables:\n"),
    Rec1 = {trecord, [{x, {tcon, int}}], 111},
    Rec2 = {trecord, [{x, {tcon, int}}], 222},
    
    case topos_infer_unify:unify_types(Rec1, Rec2) of
        {ok, Subst2} ->
            io:format("   ✓ SUCCESS: Row variables unified with composed substitution\n"),
            io:format("   Substitution: ~p\n", [Subst2]);
        {error, Error2} ->
            io:format("   ✗ FAILED: ~p\n", [Error2])
    end,
    
    % Test 3: Occurs check still works
    io:format("\n3. Occurs check (infinite type prevention):\n"),
    TVar = {tvar, 333},
    SelfRec = {trecord, [{x, {tcon, int}}], 333},
    
    case topos_infer_unify:unify_types(TVar, SelfRec) of
        {ok, _} ->
            io:format("   ✗ FAILED: Should have detected infinite type\n");
        {error, Error3} ->
            io:format("   ✓ SUCCESS: Infinite type prevented\n"),
            io:format("   Error: ~p\n", [Error3])
    end,
    
    % Test 4: Integration with orchestration
    io:format("\n4. Integration with orchestration layer:\n"),
    case topos_infer:infer_expr({lit, {int, 42}}) of
        {ok, Type4} ->
            io:format("   ✓ SUCCESS: Orchestration still works\n"),
            io:format("   Type: ~p\n", [Type4]);
        {error, Errors4} ->
            io:format("   ✗ FAILED: ~p\n", [Errors4])
    end,
    
    io:format("\n=== Demo Complete ===\n").