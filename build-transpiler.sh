#!/bin/bash
mkdir -p build
rm -f build/TranspileFile*

set -e

fpc -FEbuild/ -FuDeclarations -FuDeclarations/Utils -FuStatements -FuTypes -FuTypes/TypeDefs -FuFile -FuExpressions -FuExpressions/Utils -FuUnits -gl TranspileFile.pas

if [ -f ./build/TranspileFile.exe ]; then
    ./build/TranspileFile.exe test_transpile.pas
else
    ./build/TranspileFile test_transpile.pas
fi
