package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Rule interface {
	Match(string) []int
}

type Empty struct{}

func (e Empty) Match(s string) []int {
	if len(s) == 0 {
		return []int{0}
	}
	return nil
}

type Character uint8

func (c Character) Match(s string) []int {
	if len(s) > 0 && s[0] == uint8(c) {
		return []int{1}
	}
	return nil
}

type List []Rule

func (l List) Match(s string) []int {
	if len(l) == 1 {
		return l[0].Match(s)
	}

	opts := l[0].Match(s)
	if opts == nil {
		return nil
	}
	var ret []int
	for _, c := range opts {
		nextOpts := l[1:].Match(s[c:])
		for _, nc := range nextOpts {
			ret = append(ret, c+nc)
		}
	}
	return ret
}

type Choice [2]Rule

func (ch Choice) Match(s string) []int {
	a := ch[0].Match(s)
	b := ch[1].Match(s)
	return append(a, b...)
}

var rules = map[int]Rule{}

func parse(ruleStrings map[int]string, rules map[int]Rule, i int) Rule {
	s := ruleStrings[i]
	if quote := strings.Index(s, `"`); quote != -1 {
		rules[i] = Character(s[quote+1])
		return rules[i]
	}

	if pipe := strings.Index(s, `|`); pipe != -1 {
		a := strings.TrimSpace(s[:pipe])
		b := strings.TrimSpace(s[pipe+1:])
		ra := List{}
		rb := List{}
		rules[i] = &Choice{ra, rb}
		for _, num := range strings.Split(a, " ") {
			n, err := strconv.Atoi(num)
			if err != nil {
				panic(err)
			}
			r, ok := rules[n]
			if !ok {
				r = parse(ruleStrings, rules, n)
			}
			rules[i].(*Choice)[0] = append(rules[i].(*Choice)[0].(List), r)
		}
		for _, num := range strings.Split(b, " ") {
			n, err := strconv.Atoi(num)
			if err != nil {
				panic(err)
			}
			r, ok := rules[n]
			if !ok {
				r = parse(ruleStrings, rules, n)
			}
			rules[i].(*Choice)[1] = append(rules[i].(*Choice)[1].(List), r)
		}
		return rules[i]
	}

	rules[i] = &List{}
	for _, num := range strings.Split(s, " ") {
		n, err := strconv.Atoi(num)
		if err != nil {
			panic(err)
		}
		r, ok := rules[n]
		if !ok {
			r = parse(ruleStrings, rules, n)
		}
		*(rules[i].(*List)) = append(*(rules[i].(*List)), r)
	}

	return rules[i]
}

func main() {
	input, err := ioutil.ReadFile("day19.input")
	//input, err := ioutil.ReadFile("day19.demo")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	//lines = []string{
	//	"0: 10 2",
	//	`1: "a"`,
	//	`2: "b"`,
	//	`10: 1 | 1 10`,
	//	"",
	//	"ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb",
	//}

	var ruleStrings = map[int]string{}
	for i, line := range lines {
		if line == "" {
			lines = lines[i+1:]
			break
		}
		parts := strings.Split(line, ": ")
		num, err := strconv.Atoi(parts[0])
		if err != nil {
			panic(err)
		}
		ruleStrings[num] = parts[1]
	}
	ruleStrings[8] = "42 8 | 42"
	ruleStrings[11] = "42 11 31 | 42 31"

	rules := map[int]Rule{}
	root := List{parse(ruleStrings, rules, 0), Empty{}}

	count := 0
	for _, line := range lines {
		if root.Match(line) != nil {
			count++
		}
	}
	fmt.Println(count)
}
