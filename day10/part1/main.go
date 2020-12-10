package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

const demo1 = `16
10
15
5
1
11
7
19
6
12
4`

func main() {
	input, err := ioutil.ReadFile("day10.input")
	if err != nil {
		panic(err)
	}
	//var input = demo1
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)
	var jolts []int
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		jolts = append(jolts, i)
	}
	sort.Ints(jolts)
	ones := 0
	threes := 1
	for i, j := range jolts {
		var prev int
		if i > 0 {
			prev = jolts[i-1]
		}
		diff := j - prev
		if diff == 1 {
			ones++
		} else if diff == 3 {
			threes++
		} else {
			panic(diff)
		}
	}
	fmt.Println(jolts)
	fmt.Println(ones, threes)
	fmt.Println(ones * threes)
}