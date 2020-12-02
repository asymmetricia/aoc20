package main

import (
	"bufio"
	"io"
	"os"
	"strconv"
	"strings"
)

type Rule struct {
	First, Second int
	Char          byte
}

func NewRule(s string) Rule {
	parts := strings.Split(s, " ")
	if len(parts) != 2 {
		panic(s)
	}

	minmax := strings.Split(parts[0], "-")
	if len(minmax) != 2 {
		panic(s)
	}

	var ret Rule
	var err error
	ret.First, err = strconv.Atoi(minmax[0])
	if err != nil {
		panic(err)
	}
	ret.First--

	ret.Second, err = strconv.Atoi(minmax[1])
	if err != nil {
		panic(err)
	}
	ret.Second--

	if len(parts[1]) != 1 {
		panic(s)
	}
	ret.Char = parts[1][0]

	return ret
}

func (r Rule) Test(s string) bool {
	return s[r.First] == r.Char && s[r.Second] != r.Char ||
		s[r.First] != r.Char && s[r.Second] == r.Char
}

func main() {
	f, err := os.Open("input")
	if err != nil {
		panic(err)
	}
	var valids int
	rdr := bufio.NewReader(f)
	for {
		line, err := rdr.ReadString('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			panic(err)
		}
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			panic(line)
		}
		r := NewRule(parts[0])
		if r.Test(strings.TrimSpace(parts[1])) {
			valids++
		}
	}
	println(valids)
}
