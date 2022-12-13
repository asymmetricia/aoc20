package main

import (
	"fmt"
	"sort"
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

	i, j := 0, 2
	accum := nums[i] + nums[j-1]
	for {
		for accum < invalid {
			j++
			accum += nums[j-1]
		}
		for accum > invalid {
			accum -= nums[i]
			i++
		}
		if accum == invalid {
			break
		}
	}
	result := nums[i:j]
	sort.Ints(result)
	fmt.Println(result[0] + result[len(result)-1])
}
