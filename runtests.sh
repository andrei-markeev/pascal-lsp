#!/bin/bash

rm build/*

set -e

fpc -FEbuild/ -FuDeclarations\;Statements\;Types\;File -gl ParseFile.pas
cd Tests
make -B
