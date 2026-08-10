#!/bin/bash
mkdir -p build
rm -f build/LspServer*

set -e

fpc -FEbuild/ -FuDeclarations -FuDeclarations/Utils -FuStatements -FuTypes -FuTypes/TypeDefs -FuFile -FuExpressions -FuExpressions/Utils -FuUnits -FuLsp -gl LspServer.pas