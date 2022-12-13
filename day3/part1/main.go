package main

import (
	"io/ioutil"
	"strings"
)

func main() {
	terrain, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(strings.TrimSpace(string(terrain)), "\n")
	x, y := 0, 0
	dx, dy := 3, 1

	count := 0
	for y < len(lines)-1 {
		x = (x + dx) % len(lines[0])
		y += dy
		if lines[y][x] == '#' {
			count++
		}
	}
	println(count)
}
