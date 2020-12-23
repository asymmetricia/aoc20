package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"time"
)

type Game struct {
	Current *Cup
	Labels  map[int]*Cup
}

func (g *Game) Cups() []int {
	var ret = []int{g.Current.Label}
	for c := g.Current.Next; c != g.Current; c = c.Next {
		ret = append(ret, c.Label)
	}
	return ret
}

func (g *Game) AddCup(label int) {
	if g.Labels == nil {
		g.Labels = map[int]*Cup{}
	}

	cup := &Cup{Label: label}
	g.Labels[label] = cup
	if g.Current == nil {
		g.Current = cup
		cup.Next = cup
		cup.Prev = cup
	} else {
		g.Current.InsertBefore(cup)
	}
}

type Cup struct {
	Label int
	Next  *Cup
	Prev  *Cup
}

func (c *Cup) Remove() {
	c.Prev.Next = c.Next
	c.Next.Prev = c.Prev
}

func (c *Cup) InsertAfter(ins *Cup) {
	ins.Next = c.Next
	ins.Next.Prev = ins

	c.Next = ins
	ins.Prev = c
}

func (c *Cup) InsertBefore(ins *Cup) {
	ins.Next = c
	ins.Prev = c.Prev

	c.Prev.Next = ins
	c.Prev = ins
}

const min = 1
const max = 1000000

func (g *Game) Move() {
	var picked []*Cup
	var pickedLabel = map[int]bool{}
	for i := 0; i < 3; i++ {
		picked = append(picked, g.Current.Next)
		pickedLabel[g.Current.Next.Label] = true
		g.Current.Next.Remove()
	}

	var targetLabel = g.Current.Label - 1
	for pickedLabel[targetLabel] || targetLabel < min {
		targetLabel--
		if targetLabel < min {
			targetLabel = max
		}
	}

	c := g.Labels[targetLabel]
	for _, pick := range picked {
		c.InsertAfter(pick)
		c = pick
	}
	g.Current = g.Current.Next
}

func main() {
	input, err := ioutil.ReadFile("day23.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)

	game := &Game{}
	for _, c := range lines[0] {
		game.AddCup(int(c - '0'))
	}
	for i := 10; i <= 1000000; i++ {
		game.AddCup(i)
	}

	start := time.Now()
	last := time.Now()
	for i := 0; i < 10000000; i++ {
		if time.Since(last) > time.Second {
			fmt.Printf("%d (%d/s) (%ds)\n",
				i,
				i/int(time.Since(start).Seconds()),
				10000000/(i/int(time.Since(start).Seconds())))
			last = time.Now()
		}
		game.Move()
	}

	one := game.Labels[1]
	fmt.Println(one.Next.Label * one.Next.Next.Label)
}
