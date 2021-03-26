#!/usr/bin/env bash

set -eu

help() {
  echo -e 'Uses ormolu to find un-ormolized .hs files in current directory.

If any file fails to parse or is not ormolized, the compilation will fail. For
this reason it is helpful to use `check-ormolized` as a git pre-commit hook, so
that git forbids you from committing un-ormolized files. Here is an example on how
to do it:

    echo "#! /usr/bin/env bash
   ./scripts/check-hlint" > .git/hooks/pre-commit
    chmod +x .git/hooks/pre-commit
'
}

if [[ "${1-}" == -h* ]]; then
  help
  exit 0
fi

PATCH_FILE=ormolu.patch

echo "Checking style of haskell files"
find . -name "*.hs" -and -not -path "*/.stack-work/*" -exec bash -c 'mydiff=$(diff {} <(stack exec ormolu -- --ghc-opt -XTypeApplications --ghc-opt -XPatternSynonyms {})); if test -n "$mydiff"; then echo "Not matching ormolu format: {}" ; fi;' \; | tee $PATCH_FILE

if [[ -s $PATCH_FILE ]]; then
  exit 1
fi

echo "All files match ormolu."