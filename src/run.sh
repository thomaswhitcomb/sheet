#!/bin/bash	
	erl -pz ../../erltest/ebin -pz ../ebin -s erltest run sheet_test -s init stop;
