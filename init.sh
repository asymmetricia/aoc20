#!/bin/sh

#/ init.sh dayN

usage() {
  grep '^#' "$0" | cut -c4- >&2
  exit 1
}

if [ "$#" -ne 1 ]; then
  usage
fi

mkdir -p "$1/part1" "$1/part2"

cat > "$1"/part1/main.go <<EOF
package main

import (
	"io/ioutil"
)

func main() {
	input, err := ioutil.ReadFile("$1.input")
  if err != nil {
  	panic(err)
  }
  lines := strings.Split(strings.TrimSpace(string(input)), "\n")
}
EOF
