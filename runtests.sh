#!/bin/bash

mkdir -p build
rm build/*

set -e

fpc -FEbuild/ -FuDeclarations\;Statements\;Types\;File\;Expressions -gl ParseFile.pas
cd Tests
make -B
