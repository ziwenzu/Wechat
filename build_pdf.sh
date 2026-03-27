#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_INPUT="$ROOT_DIR/writing/Main.tex"

usage() {
  echo "Usage: ./build_pdf.sh [path/to/file.tex|file.tex]"
}

resolve_input() {
  local candidate="$1"

  if [[ -f "$candidate" ]]; then
    printf '%s\n' "$candidate"
    return 0
  fi

  if [[ -f "$ROOT_DIR/$candidate" ]]; then
    printf '%s\n' "$ROOT_DIR/$candidate"
    return 0
  fi

  if [[ -f "$ROOT_DIR/writing/$candidate" ]]; then
    printf '%s\n' "$ROOT_DIR/writing/$candidate"
    return 0
  fi

  return 1
}

if [[ $# -gt 1 ]]; then
  usage
  exit 1
fi

INPUT_ARG="${1:-$DEFAULT_INPUT}"

if ! RESOLVED_INPUT="$(resolve_input "$INPUT_ARG")"; then
  echo "Error: could not find TeX file '$INPUT_ARG'." >&2
  usage
  exit 1
fi

INPUT_DIR="$(cd "$(dirname "$RESOLVED_INPUT")" && pwd)"
INPUT_FILE="$(basename "$RESOLVED_INPUT")"
INPUT_STEM="${INPUT_FILE%.tex}"

if [[ "$INPUT_FILE" == "$INPUT_STEM" ]]; then
  echo "Error: input must be a .tex file." >&2
  exit 1
fi

cleanup_intermediate_files() {
  latexmk -c "$INPUT_FILE" >/dev/null 2>&1 || true

  rm -rf \
    "$INPUT_DIR/_minted-$INPUT_STEM" \
    "$INPUT_DIR"/_minted-* \
    "$INPUT_DIR/$INPUT_STEM.aux" \
    "$INPUT_DIR/$INPUT_STEM.bbl" \
    "$INPUT_DIR/$INPUT_STEM.bcf" \
    "$INPUT_DIR/$INPUT_STEM.blg" \
    "$INPUT_DIR/$INPUT_STEM.fdb_latexmk" \
    "$INPUT_DIR/$INPUT_STEM.fls" \
    "$INPUT_DIR/$INPUT_STEM.log" \
    "$INPUT_DIR/$INPUT_STEM.out" \
    "$INPUT_DIR/$INPUT_STEM.ptc" \
    "$INPUT_DIR/$INPUT_STEM.run.xml" \
    "$INPUT_DIR/$INPUT_STEM.synctex.gz" \
    "$INPUT_DIR/$INPUT_STEM-blx.bib"
}

(
  cd "$INPUT_DIR"
  latexmk -pdf -shell-escape -interaction=nonstopmode -halt-on-error "$INPUT_FILE"
)

cleanup_intermediate_files

echo "Built PDF: $INPUT_DIR/$INPUT_STEM.pdf"
