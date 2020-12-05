package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func procFL(min, max int) (int, int) {
	return min, min + (max-min)/2
}

func procBR(min, max int) (int, int) {
	return max - (max-min)/2, max
}

func seatId(s string) int {
	rowMin, rowMax := 0, 127
	colMin, colMax := 0, 8
	for _, c := range s {
		switch c {
		case 'F':
			rowMin, rowMax = procFL(rowMin, rowMax)
		case 'B':
			rowMin, rowMax = procBR(rowMin, rowMax)
		case 'L':
			colMin, colMax = procFL(colMin, colMax)
		case 'R':
			colMin, colMax = procBR(colMin, colMax)
		default:
			panic(c)
		}
	}
	return rowMin*8 + colMin
}

func main() {
	file, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}
	seats := map[int]bool{}
	for _, pass := range strings.Split(strings.TrimSpace(string(file)), "\n") {
		id := seatId(pass)
		seats[id] = true
	}

	for row := 0; row < 127; row++ {
		for col := 0; col < 8; col++ {
			id := row*8 + col
			if !seats[id] && seats[id-1] && seats[id+1] {
				fmt.Println(id)
			}
		}
	}

}
