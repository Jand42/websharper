#!/bin/bash

set -e

cd "$(dirname $(dirname ${BASH_SOURCE[0]}))"
if ! [ -d "packages" ]; then
  dotnet restore WebSharper.sln
fi

if [ "$OS" = "Windows_NT" ]; then
    NO_MONO=true
fi

if [ "$NO_MONO" != "" ]; then
    ./packages/build/FAKE/tools/FAKE.exe tools/GenSingleFw.fsx
else
    mono ./packages/build/FAKE/tools/FAKE.exe tools/GenSingleFw.fsx
fi
