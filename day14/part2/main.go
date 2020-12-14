package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

var mem = map[uint64]uint64{}

func write(mask string, addr, value uint64) int {
	for i, c := range mask {
		var bv uint64 = 1 << (uint64(len(mask)) - uint64(i) - 1)
		if c == 'X' {
			newmask := mask[:i] + "0" + mask[i+1:]
			return write(newmask, addr|bv, value) +
				write(newmask, addr&^bv, value)
		}
	}
	for i, c := range mask {
		var bv uint64 = 1 << (uint64(len(mask)) - uint64(i) - 1)
		if c == '1' {
			addr |= bv
		}
	}

	fmt.Println(addr, "<-", value)
	mem[addr] = value
	return 1
}

func main() {
	input, err := ioutil.ReadFile("day14.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var mask string
	for _, line := range lines {
		if line[0:4] == "mask" {
			mask = line[7:]
		} else {
			parts := strings.Split(line, "]")
			addrS := parts[0][4:]
			addr, _ := strconv.Atoi(addrS)
			parts = strings.Split(line, "=")
			valueS := parts[1][1:]
			value, _ := strconv.Atoi(valueS)

			fmt.Println(line, write(mask, uint64(addr), uint64(value)))
		}
	}
	fmt.Println(mem)
	accum := uint64(0)
	for _, v := range mem {
		accum += v
	}
	fmt.Println(accum)
}
