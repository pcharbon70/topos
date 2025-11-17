# Topos Compiler Makefile
# Provides convenient targets for building, testing, and coverage

.PHONY: help compile test coverage coverage-report clean

help:
	@echo "Topos Compiler - Available targets:"
	@echo ""
	@echo "  make compile         - Compile all source modules"
	@echo "  make test            - Run all tests"
	@echo "  make coverage        - Run tests with coverage reporting"
	@echo "  make coverage-report - Show coverage summary (after running coverage)"
	@echo "  make clean           - Remove build artifacts"
	@echo ""

# Compile all source modules
compile:
	@echo "Compiling source modules..."
	@mkdir -p _build/test
	@erlc -o _build/test -pa _build/test -I src \
		src/compiler/types/topos_types.erl \
		src/compiler/types/topos_type_subst.erl \
		src/compiler/types/topos_type_scheme.erl \
		src/compiler/types/topos_type_env.erl \
		src/compiler/types/topos_type_pp.erl \
		src/compiler/types/topos_type_error.erl \
		src/compiler/types/topos_ast.erl \
		src/compiler/types/topos_config.erl \
		src/compiler/types/topos_constraint.erl \
		src/compiler/types/topos_instance.erl \
		src/compiler/types/topos_coherence.erl \
		src/compiler/types/topos_handler_verify.erl \
		src/compiler/types/topos_infer_state.erl \
		src/compiler/types/topos_infer_unify.erl \
		src/compiler/types/topos_infer_pattern.erl \
		src/compiler/types/topos_infer_expr.erl \
		src/compiler/types/topos_infer_effect.erl \
		src/compiler/types/topos_infer.erl \
		src/compiler/topos_compiler_utils.erl
	@echo "✓ Compilation complete"

# Run all tests
test: compile
	@echo "Compiling test modules..."
	@erlc -o _build/test -pa _build/test -I src \
		test/compiler/types/topos_types_tests.erl \
		test/compiler/types/topos_type_subst_tests.erl \
		test/compiler/types/topos_type_scheme_tests.erl \
		test/compiler/types/topos_type_env_tests.erl \
		test/compiler/types/topos_type_pp_tests.erl \
		test/compiler/types/topos_type_integration_tests.erl \
		test/compiler/types/topos_type_error_tests.erl \
		test/compiler/types/topos_constraint_tests.erl \
		test/compiler/types/topos_instance_tests.erl \
		test/compiler/types/topos_coherence_tests.erl \
		test/compiler/types/topos_handler_verify_tests.erl \
		test/compiler/types/topos_infer_state_tests.erl \
		test/compiler/types/topos_infer_unify_tests.erl \
		test/compiler/types/topos_infer_pattern_tests.erl \
		test/compiler/types/topos_infer_expr_tests.erl \
		test/compiler/types/topos_infer_effect_tests.erl \
		test/compiler/types/topos_infer_tests.erl \
		test/compiler/types/topos_infer_row_unify_tests.erl \
		test/compiler/types/topos_type_subst_occurs_tests.erl
	@echo ""
	@echo "Running tests..."
	@erl -noshell -pa _build/test -eval " \
		eunit:test([ \
			topos_types_tests, \
			topos_type_subst_tests, \
			topos_type_scheme_tests, \
			topos_type_env_tests, \
			topos_type_pp_tests, \
			topos_type_integration_tests, \
			topos_type_error_tests, \
			topos_infer_state_tests, \
			topos_infer_unify_tests, \
			topos_infer_pattern_tests, \
			topos_infer_expr_tests, \
			topos_infer_effect_tests, \
			topos_infer_tests, \
			topos_infer_row_unify_tests, \
			topos_type_subst_occurs_tests \
		], [verbose]) \
	" -s init stop

# Run tests with coverage
coverage:
	@chmod +x scripts/run_coverage.sh
	@./scripts/run_coverage.sh

# Show coverage summary
coverage-report:
	@chmod +x scripts/coverage_summary.sh
	@./scripts/coverage_summary.sh

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf _build
	@echo "✓ Clean complete"
