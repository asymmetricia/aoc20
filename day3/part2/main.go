package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	terrain, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}

	var accum int = 1
	lines := strings.Split(strings.TrimSpace(string(terrain)), "\n")
	for _, slope := range [][2]int{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	} {
		x, y := 0, 0
		dx, dy := slope[0], slope[1]

		count := 0
		for y < len(lines)-1 {
			x = (x + dx) % len(lines[0])
			y += dy
			if lines[y][x] == '#' {
				count++
			}
		}
		fmt.Printf("%v %d\n", slope, count)
		accum *= count
	}
	fmt.Println(accum)

}
