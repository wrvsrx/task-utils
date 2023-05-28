#!/usr/bin/env bash

export TASKRC=taskrc
task 1 export | taskwarrior-to-dot | dot -T svg > default.svg
task 1 export | taskwarrior-to-dot --impure | dot -T svg > impure.svg
task 1 export | taskwarrior-to-dot --outside | dot -T svg > outside.svg
task export all | taskwarrior-to-dot --deleted | dot -T svg > deleted.svg
