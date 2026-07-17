#!/bin/bash

mkdir -p build
rm -f build/LspServer*

set -e

fpc -FEbuild/ -FuDeclarations\;Statements\;Types\;File\;Expressions\;Units -gl LspServer.pas
