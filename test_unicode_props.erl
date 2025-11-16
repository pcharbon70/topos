#!/usr/bin/env escript
%%! -pa _build/default/lib/topos/ebin -pa _build/default/lib/proper/ebin -pa _build/test

main(_) ->
    Properties = [
        prop_valid_unicode_codepoints,
        prop_surrogate_codepoints_rejected,
        prop_beyond_unicode_rejected,
        prop_negative_codepoints_rejected,
        prop_invalid_utf8_rejected,
        prop_valid_utf8_accepted,
        prop_unicode_in_strings
    ],
    io:format("Running ~p Unicode/UTF-8 property tests (100 cases each)...~n~n", [length(Properties)]),
    Results = lists:map(fun(Prop) ->
        io:format("  ~-40s ", [atom_to_list(Prop)]),
        Result = proper:quickcheck(topos_lexer_properties:Prop(), [{numtests, 100}, quiet]),
        Status = case Result of
            true -> "PASS";
            _ -> "FAIL"
        end,
        io:format("~s~n", [Status]),
        {Prop, Result}
    end, Properties),
    Passed = length([ok || {_, true} <- Results]),
    io:format("~n========================================~n"),
    io:format("Results: ~p/~p properties passed~n", [Passed, length(Properties)]),
    io:format("========================================~n"),
    case Passed =:= length(Properties) of
        true -> halt(0);
        false -> halt(1)
    end.
