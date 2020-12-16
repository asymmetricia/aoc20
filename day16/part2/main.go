package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/pdbogen/aoc20/colors"
	"github.com/pdbogen/aoc20/term"
)

func intro() {
	term.Clear()
	term.MoveCursor(1, 1)
	fmt.Println("Initializing Ticket Decoder...")
	for i := 0; i < 80; i++ {
		term.MoveCursor(1, 2)
		fmt.Print("[")
		for j := 0; j < 80; j++ {
			if j <= i {
				if j%2 == 0 {
					term.ColorC(colors.Green)
				} else {
					term.ColorC(colors.Red)
				}
				fmt.Print("=")
			} else {
				fmt.Print(" ")
			}
		}
		term.ColorReset()
		fmt.Print("]")
		time.Sleep(time.Second / time.Duration(framerate))
	}
}

type Rule struct {
	Name   string
	Ranges [2][2]int
	Not    map[int]bool
	Field  int
}

func (r Rule) Test(x int) bool {
	return x >= r.Ranges[0][0] && x <= r.Ranges[0][1] ||
		x >= r.Ranges[1][0] && x <= r.Ranges[1][1]
}

var framerate = 30

func init() {
	if os.Getenv("FR") != "" {
		var err error
		framerate, err = strconv.Atoi(os.Getenv("FR"))
		if err != nil {
			panic("bad framerate: " + err.Error())
		}
	}
}

func main() {
	term.HideCursor()
	defer term.ShowCursor()

	intro()

	input, err := ioutil.ReadFile("day16.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var rules = map[string]*Rule{}
	for _, line := range lines {
		if line == "" {
			break
		}
		name := line[:strings.Index(line, ":")]
		rangea := strings.Split(
			strings.Split(line, ":")[1],
			" ")[1]
		mina, err := strconv.Atoi(strings.Split(rangea, "-")[0])
		if err != nil {
			panic(err)
		}
		maxa, err := strconv.Atoi(strings.Split(rangea, "-")[1])
		if err != nil {
			panic(err)
		}
		rangeb := strings.Split(
			strings.Split(line, ":")[1],
			" ")[3]
		minb, err := strconv.Atoi(strings.Split(rangeb, "-")[0])
		if err != nil {
			panic(err)
		}
		maxb, err := strconv.Atoi(strings.Split(rangeb, "-")[1])
		if err != nil {
			panic(err)
		}
		rules[name] = &Rule{
			Name:   name,
			Ranges: [2][2]int{{mina, maxa}, {minb, maxb}},
			Not:    map[int]bool{},
			Field:  -1,
		}
	}
	lines = lines[len(rules)+1:]
	nearby := lines[4:]
	for name, rule := range rules {
		fmt.Printf("%s: %+v\n", name, rule)
	}

	term.Clear()
	var valids [][]int
	for i, nearby := range nearby {
		term.MoveCursor(1, 2)
		term.ClearLine()
		fmt.Printf("Valid: ")
		term.ColorC(colors.Green)
		fmt.Print(len(valids))
		term.ColorReset()
		fmt.Print("/")
		term.ColorC(colors.Red)
		fmt.Print(i + 1)

		term.MoveCursor(1, 1)
		term.ClearLine()
		var ticket []int
		valid := true
		nums := strings.Split(nearby, ",")
	nums:
		for _, num := range nums {
			num, err := strconv.Atoi(num)
			ticket = append(ticket, num)
			if err != nil {
				panic(err)
			}
			for _, rule := range rules {
				if rule.Test(num) {
					term.ColorC(colors.Green)
					fmt.Printf("%d,", num)
					term.ColorReset()
					continue nums
				}
			}
			term.ColorC(colors.Red)
			fmt.Printf("%d,", num)
			term.ColorReset()
			valid = false
		}
		if valid {
			valids = append(valids, ticket)
		}
		time.Sleep(time.Second / time.Duration(framerate))
	}
	term.MoveCursor(1, 3)

	for _, v := range valids {
		for num, val := range v {
			for _, rule := range rules {
				if !rule.Test(val) {
					rule.Not[num] = true
				}
			}
		}
	}

	var toAssign []*Rule
	for _, rule := range rules {
		toAssign = append(toAssign, rule)
	}

	max := 0
	var names []string
	for name := range rules {
		if len(name) > max {
			max = len(name)
		}
		names = append(names, name)
	}
	sort.Strings(names)

	fields := len(valids[0])
	done := false
	for !done {
		done = true
		time.Sleep(time.Second / time.Duration(framerate) * 10)
		term.MoveCursor(1, 4)
		for _, name := range names {
			rule := rules[name]
			if rule.Field == -1 {
				done = false
				if len(rule.Not) == (fields - 1) {
					term.ColorC(colors.Green)
				}
			}

			fmt.Printf("%"+strconv.Itoa(max)+"s: ", rule.Name)
			for i := 0; i < fields; i++ {
				if rule.Not[i] {
					fmt.Printf("   ")
				} else {
					if len(rule.Not) == (fields - 1) {
						rule.Field = i
						for _, rule := range rules {
							if rule.Name == name {
								continue
							}
							rule.Not[i] = true
						}
					}
					fmt.Printf("%2d ", i)
				}
			}
			fmt.Print("\n")
			term.ColorReset()
		}
	}

	yourS := strings.Split(lines[1], ",")
	var your []int
	for _, num := range yourS {
		i, err := strconv.Atoi(num)
		if err != nil {
			panic(err)
		}
		your = append(your, i)
	}

	accum := 1
	for name, rule := range rules {
		term.MoveCursor(1, len(rules)+5)
		fmt.Printf("%"+strconv.Itoa(max)+"s: %d\n", "product", accum)
		if strings.HasPrefix(name, "departure ") {
			term.ColorC(colors.Green)
			accum *= your[rule.Field]
		}
		term.ClearLine()
		fmt.Printf("%"+strconv.Itoa(max)+"s: %-10d\n", name, your[rule.Field])
		term.ColorReset()
		time.Sleep(time.Second / time.Duration(framerate) * 10)
	}
	term.MoveCursor(1, len(rules)+5)
	term.ColorC(colors.Green)
	fmt.Printf("%"+strconv.Itoa(max)+"s: %d\n\n", "product", accum)
	term.ColorReset()
}
