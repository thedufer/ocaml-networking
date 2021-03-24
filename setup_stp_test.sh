#!/bin/bash

for i in {1..3}; do
  _build/default/bin/main.exe client add-node switch_$i 4
done

for i in {1..6}; do
  _build/default/bin/main.exe client add-node endpoint_$i 1
done

_build/default/bin/main.exe client add-connection switch_1 0 switch_2 0
_build/default/bin/main.exe client add-connection switch_1 1 switch_3 0
_build/default/bin/main.exe client add-connection switch_2 1 switch_3 1

_build/default/bin/main.exe client add-connection switch_1 2 endpoint_1 0
_build/default/bin/main.exe client add-connection switch_1 3 endpoint_2 0
_build/default/bin/main.exe client add-connection switch_2 2 endpoint_3 0
_build/default/bin/main.exe client add-connection switch_2 3 endpoint_4 0
_build/default/bin/main.exe client add-connection switch_3 2 endpoint_5 0
_build/default/bin/main.exe client add-connection switch_3 3 endpoint_6 0
