#!/usr/bin/env bash

erlc dog_gopher.erl
erl -eval 'dog_gopher:run_simulation("../input.txt")'
