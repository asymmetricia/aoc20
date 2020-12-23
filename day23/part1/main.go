package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strings"
)

var cups []int

func Move(cups []int) (result []int) {
	max := 0
	min := math.MaxInt64
	for _, c := range cups {
		if c > max {
			max = c
		}
		if c < min {
			min = c
		}
	}

	current := cups[0]
	picked := cups[1:4]
	var whichPicked = map[int]bool{}
	for _, c := range picked {
		whichPicked[c] = true
	}

	var target = current - 1
	var targetIdx int
	for whichPicked[target] || target < min {
		target--
		if target < min {
			target = max
		}
	}
	for i, c := range cups {
		if c == target {
			targetIdx = i
		}
	}

	// 0 ... i ... picked ... i+1:
	ret := []int{current}
	if targetIdx > 0 {
		ret = append(ret, cups[4:targetIdx+1]...)
	}
	ret = append(ret, picked...)
	ret = append(ret, cups[targetIdx+1:]...)
	ret = append(ret[1:], ret[0])
	return ret
}

func main() {
	input, err := ioutil.ReadFile("day23.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)
	var cups []int
	for _, c := range lines[0] {
		cups = append(cups, int(c-'0'))
	}
	for i := 0; i < 100; i++ {
		cups = Move(cups)
		fmt.Println(cups)
	}
	fmt.Println(cups)
}
