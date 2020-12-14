package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("day14.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var mem = map[int]uint64{}
	var zeros, ones uint64
	for _, line := range lines {
		if line[0:4] == "mask" {
			zeros = 1<<36 - 1
			ones = 0
			for i, c := range line[7:] {
				if c == '0' {
					zeros ^= 1 << (35 - i)
				} else if c == '1' {
					ones |= 1 << (35 - i)
				}
			}
			fmt.Printf("mask:  %s\nones:  %036b\nzeros: %036b\n", line[7:], ones, zeros)
		} else {
			parts := strings.Split(line, "]")
			addrS := parts[0][4:]
			addr, _ := strconv.Atoi(addrS)
			parts = strings.Split(line, "=")
			valueS := parts[1][1:]
			value, _ := strconv.Atoi(valueS)
			mem[addr] = (uint64(value) | ones) & zeros
			fmt.Printf(" %036b\n|%036b\n&%036b\n=%036b\n", value, ones, zeros, (uint64(value)|ones)&zeros)
			fmt.Println(value, "->", mem[addr])
		}
	}
	fmt.Println(mem)

	accum := uint64(0)
	for _, v := range mem {
		accum += v
	}
	fmt.Println(accum)
}
