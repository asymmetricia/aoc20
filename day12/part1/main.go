package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"
)

const (
	North = 0
	East  = 90
	South = 180
	West  = 270
)

func main() {
	input, err := ioutil.ReadFile("day12.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
//	lines = strings.Split(`F10
//N3
//F7
//R90
//F11`, "\n")
	fmt.Println(lines)
	x, y := 0, 0
	facing := East

	for _, line := range lines {
		amt, _ := strconv.Atoi(line[1:])
		ix := line[0]
		if line[0] == 'F' {
			switch facing {
			case North:
				ix = 'N'
			case East:
				ix = 'E'
			case South:
				ix = 'S'
			case West:
				ix = 'W'
			}
		}
		switch ix {
		case 'N':
			y += amt
		case 'E':
			x += amt
		case 'S':
			y -= amt
		case 'W':
			x -= amt
		case 'R':
			facing = (facing + amt) % 360
		case 'L':
			facing = (facing - amt) % 360
		}
		for facing < 0 {
			facing += 360
		}
		fmt.Println(x, y, facing)
	}

	fmt.Println(x, y, math.Abs(float64(x))+math.Abs(float64(y)))
}
