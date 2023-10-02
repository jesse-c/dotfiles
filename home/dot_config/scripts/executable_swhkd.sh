#!/usr/bin/env bash

echo "Killing existing processes"
pkill swhks
pkill swhkd

echo "Starting swhks"
swhks &
echo "Starting swhkd (config: $1)"
pkexec swhkd -c "$1"/.config/swhkd/swhkdrc
