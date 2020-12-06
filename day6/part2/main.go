package main

import (
	"io/ioutil"
	"strings"
)

func main() {
	f, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(f)), "\n")

	sum := 0
	count := 0
	answers := map[rune]int{}
	for _, line := range lines {
		if line == "" {
			for _, v := range answers {
				if v == count {
					sum++
				}
			}
			count = 0
			answers = map[rune]int{}
			continue
		}
		for _, c := range line {
			answers[c] = answers[c] + 1
		}
		count++
	}
	for _, v := range answers {
		if v == count {
			sum++
		}
	}

	println(sum)
}
