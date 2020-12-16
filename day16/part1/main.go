package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Rule struct {
	Ranges [2][2]int
}

func (r Rule) Test(x int) bool {
	return x >= r.Ranges[0][0] && x <= r.Ranges[0][1] ||
		x >= r.Ranges[1][0] && x <= r.Ranges[1][1]
}

func main() {
	input, err := ioutil.ReadFile("day16.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var rules = map[string]Rule{}
	for _, line := range lines {
		if line == "" {
			break
		}
		name := line[:strings.Index(line, ":")]
		fmt.Println(name)
		rangea := strings.Split(
			strings.Split(line, ":")[1],
			" ")[1]
		mina, err := strconv.Atoi(strings.Split(rangea, "-")[0])
		if err != nil {
			panic(err)
		}
		maxa, err := strconv.Atoi(strings.Split(rangea, "-")[1])
		if err != nil {
			panic(err)
		}
		rangeb := strings.Split(
			strings.Split(line, ":")[1],
			" ")[3]
		minb, err := strconv.Atoi(strings.Split(rangeb, "-")[0])
		if err != nil {
			panic(err)
		}
		maxb, err := strconv.Atoi(strings.Split(rangeb, "-")[1])
		if err != nil {
			panic(err)
		}
		rules[name] = Rule{[2][2]int{{mina, maxa}, {minb, maxb}}}
	}
	lines = lines[len(rules)+1:]
	//your := lines[1]
	nearby := lines[4:]
	for name, rule := range rules {
		fmt.Printf("%s: %+v\n", name, rule)
	}

	var errRate int
	for _, nearby := range nearby {
		nums := strings.Split(nearby, ",")
	nums:
		for _, num := range nums {
			num, err := strconv.Atoi(num)
			if err != nil {
				panic(err)
			}
			for _, rule := range rules {
				if rule.Test(num) {
					continue nums
				}
			}
			errRate += num
		}
	}
	fmt.Println(errRate)

	//fmt.Println(your)
	//fmt.Println(strings.Join(nearby, "\n"))
}
