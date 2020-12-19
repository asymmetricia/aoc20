package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Rule struct {
	Character uint8
	A         []int
	B         []int
}

func (r Rule) String() string {
	if r.Character != 0 {
		return fmt.Sprintf(`"%c"`, r.Character)
	}
	ret := ""
	for _, a := range r.A {
		ret = fmt.Sprintf("%s %d", ret, a)
	}
	if len(r.B) != 0 {
		ret += " |"
		for _, b := range r.B {
			ret = fmt.Sprintf("%s %d", ret, b)
		}
	}
	return ret
}

var rules = map[int]Rule{}

const debug = false

func (r Rule) Match(s string) (bool, int) {
	if r.Character != 0 {
		if len(s) == 0 || s[0] != r.Character {
			if debug {
				fmt.Println(r, s, " no")
			}
			return false, 0
		}
		if debug {
			fmt.Println(r, s, " yes")
		}
		return true, 1
	}

	matched := true
	totalConsume := 0
	for _, a := range r.A {
		match, consumed := rules[a].Match(s[totalConsume:])
		if !match {
			matched = false
			break
		}
		totalConsume += consumed
	}

	if matched {
		if debug {
			fmt.Println(r, s, " yes")
		}
		return true, totalConsume
	}

	if len(r.B) == 0 {
		if debug {
			fmt.Println(r, s, " no")
		}
		return false, 0
	}

	matched = true
	totalConsume = 0
	for _, b := range r.B {
		match, consumed := rules[b].Match(s[totalConsume:])
		if !match {
			matched = false
			break
		}
		totalConsume += consumed
	}

	if matched {
		if debug {
			fmt.Println(r, s, " yes")
		}
		return true, totalConsume
	}

	if debug {
		fmt.Println(r, s, " no")
	}
	return false, 0
}

func main() {
	input, err := ioutil.ReadFile("day19.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	//lines = []string{
	//	"0: 4 1 5",
	//	"1: 2 3 | 3 2",
	//	"2: 4 4 | 5 5",
	//	"3: 4 5 | 5 4",
	//	"4: \"a\"",
	//	"5: \"b\"",
	//	"",
	//	"ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb",
	//}
	for _, line := range lines {
		if line == "" {
			break
		}
		parts := strings.Split(line, ": ")
		num, err := strconv.Atoi(parts[0])
		if err != nil {
			panic(err)
		}
		if quote := strings.Index(parts[1], `"`); quote != -1 {
			rules[num] = Rule{Character: parts[1][quote+1]}
			continue
		}
		if pipe := strings.Index(parts[1], `|`); pipe != -1 {
			r := Rule{}
			for _, num := range strings.Split(strings.TrimSpace(parts[1][:pipe]), " ") {
				n, err := strconv.Atoi(num)
				if err != nil {
					panic(err)
				}
				r.A = append(r.A, n)
			}
			for _, num := range strings.Split(strings.TrimSpace(parts[1][pipe+1:]), " ") {
				n, err := strconv.Atoi(num)
				if err != nil {
					panic(err)
				}
				r.B = append(r.B, n)
			}
			rules[num] = r
			continue
		}
		var r Rule
		for _, subnum := range strings.Split(parts[1], " ") {
			n, err := strconv.Atoi(subnum)
			if err != nil {
				panic(err)
			}
			r.A = append(r.A, n)
		}
		rules[num] = r
	}
	fmt.Println(rules)
	lines = lines[len(rules)+1:]

	matched := 0
	for _, line := range lines {
		match, consumed := rules[0].Match(line)
		match = match && consumed == len(line)
		if match {
			matched++
		}
		fmt.Println(line, match)
	}
	fmt.Println(matched)
}
