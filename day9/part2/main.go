package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("day9.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	nums := []int{}
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		nums = append(nums, i)
	}

	var invalid int
outerA:
	for i := 25; i < len(nums); i++ {
		for j := i - 25; j < i; j++ {
			for k := i - 25; k < i; k++ {
				if k == i {
					continue
				}
				if nums[k]+nums[j] == nums[i] {
					continue outerA
				}
			}
		}
		invalid = nums[i]
		break
	}

	var result []int
outerB:
	for i := 0; i < len(nums); i++ {
		accum := nums[i]
		for j := i+1; j < len(nums); j++ {
			accum += nums[j]
			if accum == invalid {
				result = nums[i:j]
				break outerB
			}
			if accum > invalid {
				continue outerB
			}
		}
	}

	if result == nil {
		panic("no contiguous set")
	}
	sort.Ints(result)
	fmt.Println(result[0] + result[len(result)-1])
}
