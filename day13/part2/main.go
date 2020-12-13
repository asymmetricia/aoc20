package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"

	"github.com/pdbogen/aoc20/colors"
	"github.com/pdbogen/aoc20/term"
)

func render(busses []int, column int, t int64) {
	term.MoveCursor(1, 1)
	for dt := int64(-1); dt <= int64(len(busses)); dt++ {
		fmt.Printf("%20d ", t+dt)
		for i, bus := range busses {
			if bus != -1 && (t+dt)%int64(bus) == 0 {
				if dt == int64(i) {
					term.Color(int(colors.Green.R), int(colors.Green.G), int(colors.Green.B))
					fmt.Printf("*")
					term.ColorReset()
				} else {
					fmt.Printf("*")
				}
			} else {
				if dt == int64(i) {
					term.Color(int(colors.Red.R), int(colors.Red.G), int(colors.Red.B))
					fmt.Printf(".")
					term.ColorReset()
				} else {
					if i == column {
						term.Color(int(colors.Blue.R), int(colors.Blue.G), int(colors.Blue.B))
						fmt.Printf("|")
						term.ColorReset()
					} else {
						fmt.Printf(" ")
					}
				}
			}
		}
		fmt.Printf("\n")
		time.Sleep(time.Millisecond)
	}
}

func main() {
	input, err := ioutil.ReadFile("day13.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	busStrs := strings.Split(lines[1], ",")
	var busses []int
	for _, bus := range busStrs {
		if bus == "x" {
			busses = append(busses, -1)
			continue
		}
		busId, _ := strconv.Atoi(bus)
		busses = append(busses, busId)
	}

	term.Clear()
	term.HideCursor()
	defer term.ShowCursor()
	var lastFrame time.Time

	t := int64(0)
	delta := int64(1)
	for i, bus := range busses {
		if bus == -1 {
			continue
		}
		for {
			if time.Since(lastFrame) > time.Second/60 {
				render(busses, i, t)
				lastFrame = time.Now()
			}
			if (t+int64(i))%int64(bus) == 0 {
				break
			}
			t += delta
			time.Sleep(time.Millisecond)
			time.Sleep(2 * time.Second / time.Duration(delta))
		}
		delta *= int64(bus)
	}
	render(busses, len(busses)-1, t)

	fmt.Println(t)
}
