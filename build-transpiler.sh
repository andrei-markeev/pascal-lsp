#!/bin/bash
mkdir -p build
rm -f build/TranspileFile*

set -e

fpc -FEbuild/ -FuDeclarations -FuDeclarations/Utils -FuStatements -FuTypes -FuTypes/TypeDefs -FuFile -FuExpressions -FuExpressions/Utils -FuUnits -gl TranspileFile.pas

if [ -f ./build/TranspileFile.exe ]; then
    ./build/TranspileFile.exe test.pas
else
    ./build/TranspileFile test.pas
fi
