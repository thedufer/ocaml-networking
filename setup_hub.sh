#!/bin/bash

for i in {1..3}; do
    _build/default/bin/main.exe client add-node endpoint_$i 1
done
_build/default/bin/main.exe client add-node hub 3

for i in {1..3}; do
    _build/default/bin/main.exe client add-connection endpoint_$i 0 hub $(($i - 1))
done
