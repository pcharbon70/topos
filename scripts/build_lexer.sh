#!/bin/bash
# Build script for Topos lexer generation
# Compiles .xrl file to .erl using leex and renames to _gen.erl

set -e

LEXER_DIR="src/compiler/lexer"
XRL_FILE="$LEXER_DIR/topos_lexer.xrl"
TEMP_FILE="$LEXER_DIR/topos_lexer.erl"
GEN_FILE="$LEXER_DIR/topos_lexer_gen.erl"

# Check if .xrl file exists
if [ ! -f "$XRL_FILE" ]; then
    echo "Error: $XRL_FILE not found"
    exit 1
fi

# Check if generated file is up to date
if [ -f "$GEN_FILE" ] && [ "$GEN_FILE" -nt "$XRL_FILE" ]; then
    echo "Lexer is up to date, skipping generation"
    exit 0
fi

echo "Generating lexer from $XRL_FILE..."

# Compile .xrl to .erl using erlc
erlc -o "$LEXER_DIR" "$XRL_FILE"

# Check if compilation succeeded
if [ ! -f "$TEMP_FILE" ]; then
    echo "Error: leex compilation failed"
    exit 1
fi

# Rename to _gen.erl
mv "$TEMP_FILE" "$GEN_FILE"

# Fix module name: topos_lexer -> topos_lexer_gen
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS uses BSD sed
    sed -i '' 's/^-module(topos_lexer)\./-module(topos_lexer_gen)./' "$GEN_FILE"
else
    # Linux uses GNU sed
    sed -i 's/^-module(topos_lexer)\./-module(topos_lexer_gen)./' "$GEN_FILE"
fi

echo "✓ Lexer generated successfully: $GEN_FILE"
