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
