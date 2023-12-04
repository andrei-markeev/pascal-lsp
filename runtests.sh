#!/bin/bash

rm build/*
fpc -FEbuild/ -gl ParseFile.pas
cd Tests
make -B
