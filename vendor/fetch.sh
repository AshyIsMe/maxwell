#!/usr/bin/env bash
# Re-create Maxwell's vendored Lisp libraries from their pinned upstream
# sources. Change a pin below, run this script, and review the diff.
#
#   vendor/fetch.sh
#
# Each library is copied without its .git directory. Only the
# mgl-pax-bootstrap subsystem of mgl-pax is kept: named-readtables needs it,
# and the rest of mgl-pax is a documentation tool Maxwell does not use.
set -euo pipefail

vendor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "${work}"' EXIT

# name  repository  commit (tag in the comment)
pins=(
  "bordeaux-threads https://github.com/sionescu/bordeaux-threads.git         a44d59ee2452b360369750a675d673abb2175e0f" # master, 2026-10-21
  "global-vars      https://github.com/lmj/global-vars.git                   c749f32c9b606a1457daa47d59630708ac0c266e" # master, 2026-10-21
  "trivial-features https://github.com/trivial-features/trivial-features.git 828246a1c1efdcad94c1ba2f2a210bb9cb4cde3e" # master, 2026-10-21
  "trivial-garbage  https://github.com/trivial-garbage/trivial-garbage.git   f0663f7b0c900ed78ac19f6db306e7c6cac999fd" # master, 2026-10-21
  "fset             https://github.com/slburson/fset.git                     1af9d2084d94457a2be39d8a6017f760a622c37c" # v2.5.0
  "misc-extensions  https://github.com/slburson/misc-extensions.git          c35c739e0180b828d73c10247cce6edf313e8086" # v4.3.2
  "named-readtables https://github.com/melisgl/named-readtables.git          9b774b51a08124c41881f10b30fb33425a3e6085" # master, 2026-07-01
  "mgl-pax          https://github.com/melisgl/mgl-pax.git                   517325aaa0fdf69b5d975dcc7b7a54570424084b" # master
  "alexandria       https://gitlab.common-lisp.net/alexandria/alexandria.git 49e82add16cb9f1ffa72c77cd687271247181ff3" # v1.4
)

# mt19937 has no public repository; it comes from its Quicklisp archive,
# checked against the MD5 published in the Quicklisp release index.
mt19937_url="https://beta.quicklisp.org/archive/mt19937/2011-02-19/mt19937-1.1.1.tgz"
mt19937_md5="54c63977b6d77abd66ebe0227b77c143"

for pin in "${pins[@]}"; do
  read -r name repository commit <<<"${pin}"
  echo "fetching ${name} at ${commit}"
  git init -q "${work}/${name}"
  git -C "${work}/${name}" fetch -q --depth 1 "${repository}" "${commit}"
  git -C "${work}/${name}" checkout -q FETCH_HEAD
  rm -rf "${work}/${name}/.git" "${work}/${name}/.github"
done

# FSet's manual is online; keep the code, README, and licence.
rm -rf "${work}/fset/Doc" "${work}/fset/docs"

mkdir "${work}/mgl-pax-bootstrap"
mkdir -p "${work}/mgl-pax-bootstrap/src"
cp "${work}/mgl-pax/mgl-pax-bootstrap.asd" "${work}/mgl-pax/COPYING" \
   "${work}/mgl-pax-bootstrap/"
cp -R "${work}/mgl-pax/src/bootstrap" "${work}/mgl-pax-bootstrap/src/"
rm -rf "${work}/mgl-pax"

echo "fetching mt19937 1.1.1"
curl -sSfL "${mt19937_url}" -o "${work}/mt19937.tgz"
echo "${mt19937_md5}  ${work}/mt19937.tgz" | md5sum --check --quiet
tar -xzf "${work}/mt19937.tgz" -C "${work}"
mv "${work}/mt19937-1.1.1" "${work}/mt19937"
rm "${work}/mt19937.tgz"

for name in bordeaux-threads global-vars trivial-features trivial-garbage \
            fset misc-extensions named-readtables mgl-pax-bootstrap alexandria \
            mt19937; do
  rm -rf "${vendor_dir:?}/${name}"
  mv "${work}/${name}" "${vendor_dir}/${name}"
done
echo "done; review with: git status vendor/"
