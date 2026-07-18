#!/bin/bash
mkdir -p build
rm -f build/LspServer*

set -e

fpc -FEbuild/ -FuDeclarations -FuStatements -FuTypes -FuFile -FuExpressions -FuUnits -FuLsp -gl LspServer.pas