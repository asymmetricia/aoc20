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
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("$1.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)
}
EOF
go fmt "$1/part1/main.go"
git add "$1/part1/main.go"

for part in part1 part2; do
  cat > .idea/runConfigurations/"$1$part".xml <<EOF
<component name="ProjectRunConfigurationManager">
  <configuration default="false" name="$1$part" type="GoApplicationRunConfiguration" factoryName="Go Application">
    <module name="aoc20" />
    <working_directory value="\$PROJECT_DIR$" />
    <kind value="FILE" />
    <filePath value="\$PROJECT_DIR\$/$1/$part/main.go" />
    <directory value="\$PROJECT_DIR\$" />
    <method v="2" />
  </configuration>
</component>
EOF
done
