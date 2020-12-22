package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Card int

type Deck []Card

func (d *Deck) Draw() Card {
	c := (*d)[0]
	*d = (*d)[1:]
	return c
}

func (d *Deck) Bottom(c Card) {
	*d = append(*d, c)
}

func Play(p1, p2 *Deck) {
	c1 := p1.Draw()
	c2 := p2.Draw()
	if c1 > c2 {
		p1.Bottom(c1)
		p1.Bottom(c2)
	} else {
		p2.Bottom(c2)
		p2.Bottom(c1)
	}
}

func Score(d *Deck) int {
	m := 1
	score := 0
	for i := len(*d) - 1; i >= 0; i-- {
		score += m * int((*d)[i])
		m++
	}
	return score
}

func main() {
	input, err := ioutil.ReadFile("day22.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var p1, p2 = &Deck{}, &Deck{}
	for _, line := range lines[1:] {
		if line == "" {
			break
		}
		v, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		p1.Bottom(Card(v))
	}
	for _, line := range lines[len(*p1)+3:] {
		if line == "" {
			break
		}
		v, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		p2.Bottom(Card(v))
	}

	for len(*p1) > 0 && len(*p2) > 0 {
		Play(p1, p2)
	}
	if len(*p1) == 0 {
		fmt.Println(Score(p2))
	} else {
		fmt.Println(Score(p1))
	}
}
