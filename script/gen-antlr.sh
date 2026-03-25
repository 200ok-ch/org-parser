#!/usr/bin/env bash

set -euo pipefail

OUT_DIR="src/java"
PKG_DIR="$OUT_DIR/org_parser/antlr"
GRAMMAR_DIR="resources/antlr"
CLASS_DIR="target/classes/org_parser/antlr"

mkdir -p "$PKG_DIR"
rm -f "$PKG_DIR"/*.java "$PKG_DIR"/*.tokens "$PKG_DIR"/*.interp
rm -rf "$CLASS_DIR"

CP="$(lein classpath)"

java -cp "$CP" org.antlr.v4.Tool \
  -visitor \
  -no-listener \
  -Xexact-output-dir \
  -package org_parser.antlr \
  -o "$PKG_DIR" \
  "$GRAMMAR_DIR/OrgLexer.g4"

java -cp "$CP" org.antlr.v4.Tool \
  -visitor \
  -no-listener \
  -Xexact-output-dir \
  -package org_parser.antlr \
  -lib "$PKG_DIR" \
  -o "$PKG_DIR" \
  "$GRAMMAR_DIR/OrgParser.g4"

echo "Generated ANTLR sources in $PKG_DIR"
