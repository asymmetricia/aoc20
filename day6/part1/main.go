package main

import (
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

func main() {
	f := aoc.Input(2020, 6)
	lines := strings.Split(strings.TrimSpace(string(f)), "\n")

	sum := 0
	answers := map[rune]bool{}
	for _, line := range lines {
		if line == "" {
			sum += len(answers)
			answers = map[rune]bool{}
		}
		for _, c := range line {
			answers[c] = true
		}
	}
	sum += len(answers)

	println(sum)
}
