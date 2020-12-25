package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func transform(subject int64, loops int) (result int64) {
	result = 1
	for i := 0; i < loops; i++ {
		result *= subject
		result %= 20201227
	}
	return
}

func untransform(subject, result int64) (loops int) {
	for result != 1 {
		loops++
		for result%subject != 0 {
			result += 20201227
		}
		result /= subject
	}
	return loops
}

func main() {
	input, err := ioutil.ReadFile("day25.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	cardKey, _ := strconv.ParseInt(lines[0], 10, 64)
	cardLoop := untransform(7, cardKey)
	doorKey, _ := strconv.ParseInt(lines[1], 10, 64)
	doorLoop := untransform(7, doorKey)

	fmt.Println(transform(doorKey, cardLoop))
	fmt.Println(transform(cardKey, doorLoop))
}
