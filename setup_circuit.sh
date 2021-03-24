#!/bin/bash

for node in A B C; do
    _build/default/bin/main.exe client add-node $node 2
done

_build/default/bin/main.exe client add-connection A 0 B 0 -perfect
_build/default/bin/main.exe client add-connection A 1 C 0 -perfect
_build/default/bin/main.exe client add-connection B 1 C 1 -perfect
