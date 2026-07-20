#!/bin/bash

mkdir -p build
rm -f build/ParseFile*

set -e

FPC=fpc
MAKE=make
if ! command -v make &> /dev/null && command -v wsl.exe &> /dev/null && [[ "$(uname)" == MINGW* || "$(uname)" == MSYS* || "$(uname)" == CYGWIN* ]]; then
    FPC="wsl fpc"
    MAKE="wsl make"
fi

$FPC -FEbuild/ -FuDeclarations -FuStatements -FuTypes -FuTypes/TypeDefs -FuFile -FuExpressions -FuUnits -gl ParseFile.pas

cd Tests

$MAKE -B
