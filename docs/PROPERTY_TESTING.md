# Property-Based Testing with PropEr

This document explains how to use property-based testing for the Topos type system.

## What is Property-Based Testing?

Property-based testing automatically generates hundreds/thousands of random test cases to verify that mathematical properties and invariants hold. Instead of writing specific examples, you define properties that should always be true.

For example, instead of testing that `union([1,2], [3,4]) = [1,2,3,4]`, you test that for ANY two sets, union is commutative: `union(A, B) = union(B, A)`.

## Setup

### 1. Install rebar3 (if not already installed)

```bash
# On Ubuntu/Debian
sudo apt-get install rebar3

# Or download directly
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

### 2. Install Dependencies

```bash
cd /home/ducky/code/topos
rebar3 get-deps
rebar3 compile
```

This will download and compile PropEr (the property-based testing framework).

## Running Property Tests

### Method 1: Using rebar3 (Recommended)

```bash
# Run all property tests
rebar3 proper

# Run with more test cases (default is 100)
rebar3 proper -n 1000

# Run with custom options
rebar3 proper -n 500 --cover
```

### Method 2: Using EUnit Integration

The property tests are integrated into EUnit, so they run as part of the regular test suite:

```bash
# Run all tests including properties
rebar3 eunit

# Or using erl directly
erl -noshell -pa _build/test/lib/*/ebin -pa _build/test \
    -eval "eunit:test([topos_type_properties], [verbose])" \
    -s init stop
```

### Method 3: Manual Execution

```erlang
% Start Erlang shell with PropEr loaded
erl -pa _build/test/lib/*/ebin -pa _build/test

% Run a specific property
1> topos_types:init_fresh_counter().
2> proper:quickcheck(topos_type_properties:prop_subst_identity()).

% Run with custom number of tests
3> proper:quickcheck(topos_type_properties:prop_effect_union_commutative(),
                     [{numtests, 1000}]).
```

## Available Properties

The `topos_type_properties` module defines the following properties:

### Substitution Laws

1. **`prop_subst_identity/0`** - Applying empty substitution is identity
   - Property: `∀T. apply(∅, T) = T`
   - Verifies: Empty substitution doesn't change types

2. **`prop_subst_composition/0`** - Substitution composition is associative
   - Property: `compose(compose(S3, S2), S1) = compose(S3, compose(S2, S1))`
   - Verifies: Composition order doesn't matter

3. **`prop_subst_idempotent/0`** - Idempotent substitutions stay idempotent
   - Property: `∀S,T. is_idempotent(S) ⟹ apply(S, apply(S, T)) = apply(S, T)`
   - Verifies: Applying twice has no additional effect

### Effect Set Properties

4. **`prop_effect_union_commutative/0`** - Effect union is commutative
   - Property: `∀E1,E2. union(E1, E2) = union(E2, E1)`
   - Verifies: Order of union doesn't matter

5. **`prop_effect_union_associative/0`** - Effect union is associative
   - Property: `union(union(E1, E2), E3) = union(E1, union(E2, E3))`
   - Verifies: Grouping of union doesn't matter

6. **`prop_effect_normalize_idempotent/0`** - Normalization is idempotent
   - Property: `normalize(normalize(E)) = normalize(E)`
   - Verifies: Normalizing twice has no additional effect

### Type Variable Properties

7. **`prop_type_vars_substitution/0`** - Type variables are preserved or removed
   - Property: Variables not in domain of substitution are preserved
   - Verifies: Substitution correctly tracks type variables

8. **`prop_occurs_check_soundness/0`** - Occurs check is sound
   - Property: If `occurs_check(V, T)` is true, then `V ∈ type_vars(T)`
   - Verifies: Occurs check correctly identifies variable occurrences

## Understanding Generators

PropEr uses generators to create random test data. Our generators include:

- **`simple_type/0`** - Generate base types and type variables
- **`type/1`** - Generate types up to a given depth (prevents infinite recursion)
- **`gen_type/0`** - Generate types with default depth (3)
- **`effect_set/0`** - Generate normalized effect sets
- **`substitution/0`** - Generate random substitutions
- **`safe_substitution/0`** - Generate substitutions that don't create cycles

## Interpreting Results

### Success
```
OK: Passed 100 test(s).
```
All generated test cases satisfied the property.

### Failure
```
Failed: After 42 test(s).
{counterexample, ...}
```
PropEr found a counterexample that violates the property. The counterexample shows the input that caused the failure.

### Shrinking
When a failure occurs, PropEr automatically tries to find the smallest/simplest input that still fails (called "shrinking"). This makes debugging easier.

## Adding New Properties

To add new properties to test:

1. **Define the property function** in `topos_type_properties.erl`:
```erlang
prop_my_property() ->
    ?FORALL({Input1, Input2}, {generator1(), generator2()},
        begin
            Result = my_function(Input1, Input2),
            expected_property(Result)
        end).
```

2. **Export the property** in the `-export` list

3. **Add to EUnit integration** (optional) in `proper_test_/0`

4. **Run the new property**:
```bash
rebar3 proper
```

## Examples

### Example 1: Testing Effect Union Commutativity

```erlang
% Property definition
prop_effect_union_commutative() ->
    ?FORALL({E1, E2}, {effect_set(), effect_set()},
        begin
            U1 = topos_types:union_effects(E1, E2),
            U2 = topos_types:union_effects(E2, E1),
            topos_types:effects_equal(U1, U2)
        end).

% Running
1> proper:quickcheck(topos_type_properties:prop_effect_union_commutative()).
.....................................................................................................
OK: Passed 100 test(s).
true
```

### Example 2: Testing Substitution Identity

```erlang
% Property definition
prop_subst_identity() ->
    ?FORALL(T, gen_type(),
        begin
            Empty = topos_type_subst:empty(),
            Result = topos_type_subst:apply(Empty, T),
            Result =:= T
        end).

% Running
2> proper:quickcheck(topos_type_properties:prop_subst_identity()).
.....................................................................................................
OK: Passed 100 test(s).
true
```

## Benefits for Topos

Property-based testing is especially valuable for the type system because:

1. **Mathematical Laws** - Type systems have well-defined mathematical properties that should always hold
2. **Edge Cases** - Automatically finds edge cases you might not think of
3. **Regression Prevention** - Ensures properties hold as code evolves
4. **Documentation** - Properties serve as executable specifications
5. **Confidence** - 1000s of random test cases provide much more confidence than a few hand-written examples

## Troubleshooting

### PropEr not found
```bash
rebar3 get-deps
rebar3 compile
```

### Tests timeout
Increase timeout in `proper_test_/0`:
```erlang
{timeout, 120, [  % Increase from 60 to 120 seconds
    ...
]}
```

### Generator creates invalid data
Add constraints with `?SUCHTHAT`:
```erlang
valid_type() ->
    ?SUCHTHAT(T, gen_type(), is_valid(T)).
```

## References

- [PropEr Documentation](https://proper-testing.github.io/)
- [PropEr Tutorial](https://proper-testing.github.io/tutorials/PropEr_testing.html)
- [Property-Based Testing (book)](https://propertesting.com/)
- [Erlang PropEr GitHub](https://github.com/proper-testing/proper)
