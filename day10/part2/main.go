package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

func main() {
	input := aoc.Input(2020, 10)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var jolts []int
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		jolts = append(jolts, i)
	}
	sort.Ints(jolts)

	fmt.Println(permutations(0, jolts))
}

var memo = map[string]int{}

func key(prev int, in []int) (ret string) {
	ret = strconv.Itoa(prev) + ","
	for _, i := range in {
		ret += strconv.Itoa(i) + ","
	}
	return ret
}

func permutations(prev int, jolts []int) (count int) {
	if len(jolts) == 1 {
		return 1
	}

	key := key(prev, jolts)
	if mem, ok := memo[key]; ok {
		return mem
	}

	accum := permutations(jolts[0], jolts[1:])

	if jolts[1]-prev <= 3 {
		accum += permutations(prev, jolts[1:])
	}

	memo[key] = accum
	return accum
}
