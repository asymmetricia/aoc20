package main

import (
	"bufio"
	"errors"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	var lines []int
	reader := bufio.NewReader(os.Stdin)
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

	for i := range lines {
		for j := i + 1; j < len(lines); j++ {
			if lines[i]+lines[j] == 2020 {
				log.Printf("%d * %d = %d", lines[i], lines[j], lines[i]*lines[j])
				os.Exit(0)
			}
		}
	}
}
