#!/bin/bash
erl -pa ebin -pa deps/*/ebin -config dynjs -s dynjs -sname dynjs@localhost