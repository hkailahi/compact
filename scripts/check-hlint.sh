#!/usr/bin/env bash

set -eu

help() {
  echo -e 'Uses hlint to find un-linted .hs files in current directory.

If any file fails to parse or is not linted, the compilation will fail. For
this reason it is helpful to use `check-hlint` as a git pre-commit hook, so
that git forbids you from committing un-linted files. Here is an example on how
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

stack exec hlint -- -c .hlint.yaml .

echo "All files linted."