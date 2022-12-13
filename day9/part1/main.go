package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

func main() {
	input := aoc.Input(2020, 9)
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	nums := []int{}
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		nums = append(nums, i)
	}

outer:
	for i := 25; i < len(nums); i++ {
		for j := i - 25; j < i; j++ {
			for k := i - 25; k < i; k++ {
				if k == i {
					continue
				}
				if nums[k]+nums[j] == nums[i] {
					continue outer
				}
			}
		}
		fmt.Println(nums[i])
	}
}
