package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type CPU struct {
	Accumulator int
	Pointer     int
	Program     []Operator
	seen        map[int]bool
}

func (c CPU) String() string {
	return fmt.Sprintf("A:%d P:%d", c.Accumulator, c.Pointer)
}

func (c *CPU) Run() error {
	c.Accumulator = 0
	c.Pointer = 0
	c.seen = map[int]bool{}
	for {
		if c.Pointer < 0 {
			return errors.New("negative pointer")
		}
		if c.Pointer == len(c.Program) {
			return nil
		}
		if c.Pointer > len(c.Program) {
			return errors.New("too far")
		}
		if c.seen[c.Pointer] {
			return errors.New("loop")
		}
		c.seen[c.Pointer] = true
		c.Program[c.Pointer].Operate(c)
	}
}

type Operator interface {
	Operate(cpu *CPU)
	String() string
}

type Nop int

func (Nop) Operate(cpu *CPU) {
	cpu.Pointer++
}

func (n Nop) String() string { return fmt.Sprintf("NOP %+d", n) }

type Acc int

func (a Acc) Operate(cpu *CPU) {
	cpu.Accumulator += int(a)
	cpu.Pointer++
}

func (a Acc) String() string {
	return fmt.Sprintf("ACC %+d", a)
}

type Jmp int

func (j Jmp) Operate(cpu *CPU) {
	cpu.Pointer += int(j)
}

func (j Jmp) String() string {
	return fmt.Sprintf("JMP %+d", j)
}

func main() {
	input, err := ioutil.ReadFile("day8.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	fmt.Println(lines)
	var cpu CPU
	for _, line := range lines {
		opVal := strings.SplitN(line, " ", 2)
		val, err := strconv.Atoi(opVal[1])
		if err != nil {
			panic(err)
		}
		switch strings.ToLower(opVal[0]) {
		case "nop":
			cpu.Program = append(cpu.Program, Nop(val))
		case "jmp":
			cpu.Program = append(cpu.Program, Jmp(val))
		case "acc":
			cpu.Program = append(cpu.Program, Acc(val))
		default:
			panic(opVal)
		}
	}

prog:
	for i, op := range cpu.Program {
		switch op := op.(type) {
		case Nop:
			cpu.Program[i] = Jmp(op)
			if err := cpu.Run(); err == nil {
				fmt.Println(i, op, "->", Jmp(op), cpu)
				break prog
			}
			cpu.Program[i] = op
		case Jmp:
			cpu.Program[i] = Nop(op)
			if err := cpu.Run(); err == nil {
				fmt.Println(i, op, "->", Nop(op), cpu)
				break prog
			}
			cpu.Program[i] = op
		}
	}
	fmt.Println(cpu)
}
