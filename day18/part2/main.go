package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/pdbogen/aoc20/colors"
	"github.com/pdbogen/aoc20/term"
)

type TokenType int

const (
	Plus TokenType = iota
	Times
	Open
	Close
	Number
)

type Token struct {
	TokenType
	int
}

func (t Token) String() string {
	switch t.TokenType {
	case Plus:
		return "+"
	case Times:
		return "*"
	case Open:
		return "("
	case Close:
		return ")"
	case Number:
		return strconv.Itoa(t.int)
	}
	return ""
}

func lex(line string) []Token {
	var ret []Token
	for _, c := range line {
		switch c {
		case '(':
			ret = append(ret, Token{TokenType: Open})
		case ')':
			ret = append(ret, Token{TokenType: Close})
		case '+':
			ret = append(ret, Token{TokenType: Plus})
		case '*':
			ret = append(ret, Token{TokenType: Times})
		case ' ':
			continue
		default:
			ret = append(ret, Token{TokenType: Number, int: int(c - '0')})
		}
	}
	return ret
}

func find(tokens []Token, typ TokenType) int {
	for i, token := range tokens {
		if token.TokenType == typ {
			return i
		}
	}
	return -1
}

func findlast(tokens []Token, typ TokenType) int {
	for i := len(tokens) - 1; i >= 0; i-- {
		if tokens[i].TokenType == typ {
			return i
		}
	}
	return -1
}

func eval(tokens []Token, start, end int) int {
	if fancy {
		term.MoveCursor(1, 2)
		term.ClearLine()
		term.ColorC(colors.Red)
		fmt.Println(tokens)
		time.Sleep(delay * time.Millisecond)
	}

	consider := tokens[start:end]

	var collapsed []Token
	if open := findlast(consider, Open); open != -1 {
		close := find(consider[open:], Close)
		if close == -1 {
			panic(tokens)
		}
		close += open
		collapsed = append(collapsed, tokens[:open+start]...)
		collapsed = append(collapsed,
			Token{Number, eval(tokens, open+start+1, close+start)})
		collapsed = append(collapsed, tokens[close+1:]...)
		return eval(collapsed, 0, len(collapsed))
	}

	if plus := find(consider, Plus); plus != -1 {
		collapsed = append(collapsed, tokens[:start+plus-1]...)
		collapsed = append(collapsed,
			Token{Number, consider[plus-1].int + consider[plus+1].int})
		collapsed = append(collapsed, tokens[start+plus+2:]...)
		return eval(collapsed, start, end-2)
	}

	if times := find(consider, Times); times != -1 {
		collapsed = append(collapsed, tokens[:start+times-1]...)
		collapsed = append(collapsed,
			Token{Number, consider[times-1].int * consider[times+1].int})
		collapsed = append(collapsed, tokens[start+times+2:]...)
		return eval(collapsed, start, end-2)
	}

	if len(consider) == 1 && consider[0].TokenType == Number {
		return consider[0].int
	}

	log.Fatalf("bad token state: %v", consider)
	return 0
}

var fancy = true
var delay time.Duration = 150

func main() {
	rows, err := term.Height()
	cols, _ := term.Width()
	if err != nil {
		log.Printf("no fancy printing :sadface: : %v", err)
		fancy = false
	}

	if fancy {
		term.HideCursor()
		defer term.ShowCursor()
	}

	input, err := ioutil.ReadFile("day18.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	accum := 0
	for i, line := range lines {
		if fancy {
			term.Clear()
			term.MoveCursor(1, 1)
			term.ColorC(colors.Green)
			fmt.Println("Total:", accum)

			printed := 2
			index := i
			for printed < int(rows) && index < len(lines) {
				s := fmt.Sprintf("%v", lines[index])
				if index == i {
					term.ColorC(colors.Red)
				} else {
					term.ColorReset()
				}
				fmt.Println(s)
				index++
				printed += len(s)/int(cols) + 1
			}
			term.MoveCursor(1, 2)
		}

		lexed := lex(line)
		accum += eval(lexed, 0, len(lexed))
		delay = 8 * delay / 10
		if delay < 1 {
			delay = 1
		}
	}

	if fancy {
		term.Clear()
		term.MoveCursor(1, 1)
		term.ColorC(colors.Green)
		fmt.Println("Final Answer:", accum)
		time.Sleep(5 * time.Second)
	} else {
		fmt.Println("Final Answer:", accum)
	}
}
