package main

import (
	"bytes"
	"errors"
	"io"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

func main() {
	var lines []int
	reader := bytes.NewBuffer(aoc.Input(2020, 1))
	for {
		str, err := reader.ReadString('\n')
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		str = strings.TrimSpace(str)
		number, err := strconv.Atoi(str)
		if err != nil {
			log.Fatalf("could not parse %q: %w", str, err)
		}
		lines = append(lines, number)
	}

	for i, iV := range lines[:len(lines)-2] {
		for j, jV := range lines[i+1 : len(lines)-1] {
			for _, kV := range lines[j+1:] {
				if iV+jV+kV == 2020 {
					log.Printf("%d * %d * %d= %d", iV, jV, kV, iV*jV*kV)
					os.Exit(0)
				}
			}
		}
	}
}
