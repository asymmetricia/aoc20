package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("day13.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)
	minutes, _ := strconv.Atoi(lines[0])
	busStrs := strings.Split(lines[1], ",")
	var busses []int
	for _, bus := range busStrs {
		if bus == "x" {
			continue
		}
		busId, _ := strconv.Atoi(bus)
		busses = append(busses, busId)
	}

	var best int = math.MaxInt64
	var bestBus int
	for _, bus := range busses {
		time := int(math.Ceil(float64(minutes)/float64(bus)) * float64(bus))
		if time < best {
			best = time
			bestBus = bus
		}
	}
	fmt.Println(best, bestBus)
	fmt.Println((best - minutes) * bestBus)
}
