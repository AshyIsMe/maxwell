#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
models_file="${MAXWELL_PI_MODELS_FILE:-${HOME}/.pi/agent/models.json}"

if ! command -v jq >/dev/null 2>&1; then
  echo "run.sh: jq is required to read ${models_file}" >&2
  exit 1
fi

if [[ ! -r "${models_file}" ]]; then
  echo "run.sh: cannot read Pi model configuration: ${models_file}" >&2
  exit 1
fi

safeai_base_url="$(
  jq -er '.providers.safeailabs.baseUrl
          | select(type == "string" and length > 0)' "${models_file}"
)"
safeai_model="$(
  jq -er '.providers.safeailabs.models[0].id
          | select(type == "string" and length > 0)' "${models_file}"
)"
api_key_reference="$(
  jq -er '.providers.safeailabs.apiKey
          | select(type == "string" and length > 0)' "${models_file}"
)"

case "${api_key_reference}" in
  '!'*)
    # Pi treats a leading ! as a credential command evaluated at request time.
    SAFEAILABS_API_KEY="$(bash -c "${api_key_reference#!}")"
    ;;
  '${'*'}')
    environment_name="${api_key_reference:2:-1}"
    SAFEAILABS_API_KEY="${!environment_name:-}"
    ;;
  '$'*)
    environment_name="${api_key_reference#'$'}"
    SAFEAILABS_API_KEY="${!environment_name:-}"
    ;;
  *)
    SAFEAILABS_API_KEY="${api_key_reference}"
    ;;
esac

if [[ -z "${SAFEAILABS_API_KEY}" ]]; then
  echo "run.sh: the Safe AI Labs credential resolved to an empty value" >&2
  exit 1
fi

export SAFEAILABS_API_KEY

maxwell_args=(
  --base-url "${safeai_base_url}"
  --api-key-env SAFEAILABS_API_KEY
  --model "${safeai_model}"
  "$@"
)

if [[ -x "${script_dir}/maxwell" ]]; then
  exec "${script_dir}/maxwell" "${maxwell_args[@]}"
fi

if ! command -v sbcl >/dev/null 2>&1; then
  echo "run.sh: neither a built Maxwell executable nor sbcl was found" >&2
  exit 1
fi

exec sbcl --script "${script_dir}/maxwell.lisp" "${maxwell_args[@]}"
