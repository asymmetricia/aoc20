package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Game struct {
	States [][2]Deck
}

func (g Game) IsLoop(a, b Deck) bool {
	for _, state := range g.States {
		if a.Equal(state[0]) && b.Equal(state[1]) {
			return true
		}
	}
	return false
}

type Card int

type Deck []Card

func (d Deck) Equal(b Deck) bool {
	if len(d) != len(b) {
		return false
	}
	for i, c := range d {
		if b[i] != c {
			return false
		}
	}
	return true
}

func (d Deck) Copy() *Deck {
	ret := &Deck{}
	for _, c := range d {
		ret.Bottom(c)
	}
	return ret
}

func (d *Deck) Draw() Card {
	c := (*d)[0]
	*d = (*d)[1:]
	return c
}

func (d *Deck) Bottom(c Card) {
	*d = append(*d, c)
}

func (g *Game) Play(p1, p2 *Deck) (p1wins bool) {
	for len(*p1) > 0 && len(*p2) > 0 {
		if g.IsLoop(*p1, *p2) {
			return true
		}

		g.States = append(g.States, [2]Deck{*p1.Copy(), *p2.Copy()})

		c1 := p1.Draw()
		c2 := p2.Draw()

		var result bool
		if len(*p1) >= int(c1) && len(*p2) >= int(c2) {
			game := &Game{}
			p1 := p1.Copy()
			*p1 = (*p1)[:int(c1)]
			p2 := p2.Copy()
			*p2 = (*p2)[:int(c2)]
			result = game.Play(p1, p2)
		} else {
			result = c1 > c2
		}

		if result {
			p1.Bottom(c1)
			p1.Bottom(c2)
		} else {
			p2.Bottom(c2)
			p2.Bottom(c1)
		}
	}
	return len(*p1) > 0
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
	//input, err := ioutil.ReadFile("day22.demo")
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

	var game Game
	for len(*p1) > 0 && len(*p2) > 0 {
		game.Play(p1, p2)
	}
	if len(*p1) == 0 {
		fmt.Println(*p2)
		fmt.Println(Score(p2))
	} else {
		fmt.Println(*p1)
		fmt.Println(Score(p1))
	}
}
