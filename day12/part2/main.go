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
	//lines = strings.Split(`F10
	//N3
	//F7
	//R90
	//F11`, "\n")
	fmt.Println(lines)
	wpx, wpy := 10, 1
	x, y := 0, 0

	for _, line := range lines {
		line = strings.TrimSpace(line)
		amt, _ := strconv.Atoi(line[1:])
		switch line[0] {
		case 'N':
			wpy += amt
		case 'E':
			wpx += amt
		case 'S':
			wpy -= amt
		case 'W':
			wpx -= amt
		case 'R':
			// +y -> +x
			// -y -> -x
			// +x -> -y
			// -x -> +y
			for amt < 0 {
				amt += 360
			}
			for amt > 0 {
				wpx, wpy = wpy, -wpx
				amt -= 90
			}
		case 'L':
			// +y -> -x
			// -y -> +x
			// +x -> +y
			// -x -> -y
			for amt < 0 {
				amt += 360
			}
			for amt > 0 {
				wpx, wpy = -wpy, wpx
				amt -= 90
			}
		case 'F':
			x += wpx * amt
			y += wpy * amt
		}
		fmt.Println(line, wpx, wpy, x, y)
	}

	fmt.Println(x, y, math.Abs(float64(x))+math.Abs(float64(y)))
}
