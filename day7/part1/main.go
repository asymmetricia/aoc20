package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

type rule struct {
	n     int
	color string
}

func main() {
	input := aoc.Input(2020, 7)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	bags := map[string][]rule{}

	for _, line := range lines {
		container := strings.Split(line, " contain ")[0]
		container = strings.TrimSuffix(container, " bags")
		container = strings.TrimSuffix(container, " bag")

		insideTxt := strings.Split(line, " contain ")[1]
		insides := strings.Split(strings.TrimRight(insideTxt, "."), ", ")
		fmt.Println("container:", container)
		for _, inside := range insides {
			if inside == "no other bags" {
				bags[container] = nil
				continue
			}
			inside = strings.TrimSuffix(inside, " bags")
			inside = strings.TrimSuffix(inside, " bag")
			ncol := strings.SplitN(inside, " ", 2)
			n, err := strconv.Atoi(ncol[0])
			if err != nil {
				panic(err)
			}
			bags[container] = append(bags[container], rule{n, ncol[1]})
		}
	}

	count := 0
	for bag := range bags {
		if hasShinyGold(bag, bags) {
			count++
			fmt.Println(bag)
		}
	}
	fmt.Println(count)
}

// return true if the named bag contains shiny gold bags
func hasShinyGold(bag string, bags map[string][]rule) bool {
	for _, rule := range bags[bag] {
		if rule.color == "shiny gold" {
			return true
		}
		if hasShinyGold(rule.color, bags) {
			return true
		}
	}

	return false
}
