package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func eval(s string) (result int, consumed int) {
	accum := 0
	var op = '+'
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch c {
		case ' ':
			continue
		case '+':
			fallthrough
		case '*':
			op = int32(c)
		case '(':
			v, consumed := eval(s[i+1:])
			i += consumed
			if op == '+' {
				accum += v
			} else {
				accum *= v
			}
		case ')':
			return accum, i+1
		default:
			v := int(c - '0')
			if op == '+' {
				accum += v
			} else {
				accum *= v
			}
		}
	}
	return accum, len(s)
}
func main() {
	input, err := ioutil.ReadFile("day18.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	accum := 0
	for _, line := range lines {
		v, _ := eval(line)
		accum += v
	}
	fmt.Println(accum)
}
