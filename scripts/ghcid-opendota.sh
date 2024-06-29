#!/usr/bin/env sh
ghcid -c "cabal repl opendota" --reload spec --restart opendota.cabal -T ":! cabal test all"
