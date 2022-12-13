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

	for i, iV := range lines {
		for _, jV := range lines[i:] {
			if iV+jV == 2020 {
				log.Printf("%d * %d = %d", iV, jV, iV*jV)
				os.Exit(0)
			}
		}
	}
	log.Fatal("no result")
}
