package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("day15.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	//lines = []string{"0,3,6"}
	//lines = []string{"2,1,3"}

	var heard = -1
	var currentTurn int
	var mem = map[int]int{}
	for _, num := range strings.Split(lines[0], ",") {
		num, _ := strconv.Atoi(num)
		if heard > -1 {
			mem[heard] = currentTurn
		}
		heard = num
		currentTurn++
	}

	for currentTurn < 30000000 {
		speak := 0
		if heardTurn, ok := mem[heard]; ok {
			speak = currentTurn - heardTurn
		}
		mem[heard] = currentTurn

		heard = speak
		currentTurn++
		if currentTurn == 2020 {
			fmt.Println("part 1", heard)
		}
	}
	fmt.Println("part 2", heard)
}
