#!/bin/bash

mkdir -p build
touch build/dummyfile
rm build/*

set -e

FPC=fpc
MAKE=make
if ! command -v make &> /dev/null && command -v wsl.exe &> /dev/null; then
    FPC="wsl fpc"
    MAKE="wsl make"
fi

$FPC -FEbuild/ -FuDeclarations -FuStatements -FuTypes -FuFile -FuExpressions -FuUnits -gl ParseFile.pas

cd Tests

$MAKE -B
