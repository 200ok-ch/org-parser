#!/usr/bin/env bash

set -euo pipefail

OUT_DIR="src/js/antlr"
GRAMMAR_DIR="resources/antlr"

mkdir -p "$OUT_DIR"
rm -f "$OUT_DIR"/*.js "$OUT_DIR"/*.cjs "$OUT_DIR"/*.tokens "$OUT_DIR"/*.interp

CP="$(lein classpath)"

java -cp "$CP" org.antlr.v4.Tool \
  -Dlanguage=JavaScript \
  -visitor \
  -no-listener \
  -Xexact-output-dir \
  -o "$OUT_DIR" \
  "$GRAMMAR_DIR/OrgLexer.g4"

java -cp "$CP" org.antlr.v4.Tool \
  -Dlanguage=JavaScript \
  -visitor \
  -no-listener \
  -Xexact-output-dir \
  -lib "$OUT_DIR" \
  -o "$OUT_DIR" \
  "$GRAMMAR_DIR/OrgParser.g4"

node <<'EOF'
const fs = require('fs')
const path = require('path')

const dir = 'src/js/antlr'
for (const base of ['OrgLexer', 'OrgParserVisitor', 'OrgParser']) {
  const inFile = path.join(dir, `${base}.js`)
  let source = fs.readFileSync(inFile, 'utf8')
  source = source.replace(/import antlr4 from 'antlr4';/g, "const antlr4 = require('antlr4').default;")
  source = source.replace(/import (\w+) from '\.\/(\w+)\.js';/g, "const $1 = require('./$2.cjs');")
  source = source.replace(/export default class (\w+)/, 'class $1')
  source += `\nmodule.exports = ${base};\n`
  fs.writeFileSync(path.join(dir, `${base}.cjs`), source)
}
EOF

echo "Generated ANTLR JavaScript sources in $OUT_DIR (.js + .cjs)"
