package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type rule struct {
	n     int
	color string
}

func main() {
	input, err := ioutil.ReadFile("day7.input")
	if err != nil {
		panic(err)
	}

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

	fmt.Println(countInside("shiny gold", bags))
}

func countInside(bag string, bags map[string][]rule) int {
	rules := bags[bag]
	if len(rules) == 0 {
		return 0
	}

	total := 0
	for _, rule := range bags[bag] {
		total += rule.n + rule.n*countInside(rule.color, bags)
	}
	return total
}
