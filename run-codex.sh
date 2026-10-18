#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
model="${MAXWELL_CODEX_MODEL:-gpt-5.6-sol}"

maxwell_args=(
  --base-url https://api.openai.com/v1
  --wire-api responses
  --auth openai
  --model "${model}"
  --request-json '{"store":false,"stream":true}'
  "$@"
)

if [[ "${1:-}" == "--login" ]]; then
  maxwell_args=(--login openai)
fi

if [[ -x "${script_dir}/maxwell" ]]; then
  exec "${script_dir}/maxwell" "${maxwell_args[@]}"
fi

if ! command -v sbcl >/dev/null 2>&1; then
  echo "run-codex.sh: neither a built Maxwell executable nor sbcl was found" >&2
  exit 1
fi

exec sbcl --script "${script_dir}/maxwell.lisp" "${maxwell_args[@]}"
