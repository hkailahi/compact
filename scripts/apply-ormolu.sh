#!/usr/bin/env bash

set -eu

help() {
 echo -e "Runs ormolu on every .hs file found within the current directory.
"
}

if [[ "${1-}" == -h* ]]; then
   help
   exit 0
fi

scriptdir=$(dirname $0)
shareddir="$scriptdir"/..

find . -name "*.hs" -and -not -path "*/.stack-work/*" -exec ormolu --ghc-opt -XTypeApplications --ghc-opt -XPatternSynonyms -i {} \;
