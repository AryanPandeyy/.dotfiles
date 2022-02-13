#!/bin/bash

URL='https://www.google.com/search?q='
QUERY=$(echo '' | dmenu -p "Search:" -fn "-xos4-terminus-medium-r-*-*-14-*")
if [ -n "$QUERY" ]; then
  microsoft-edge-stable --inprivate "${URL}${QUERY}" 2> /dev/null
fi
