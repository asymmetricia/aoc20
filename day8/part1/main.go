package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type CPU struct {
	Accumulator int
	Pointer     int
	Program     []Operator
	seen        map[int]bool
}

func (c *CPU) Run() {
	c.seen = map[int]bool{}
	//in := bufio.NewReader(os.Stdin)
	for c.Pointer >= 0 && c.Pointer < len(c.Program) {
		fmt.Printf("A %d P %d\n", c.Accumulator, c.Pointer)
		fmt.Println(c.Program[c.Pointer].String())
		if c.seen[c.Pointer] {
			break
		}
		c.seen[c.Pointer] = true
		c.Program[c.Pointer].Operate(c)
		//in.ReadString('\n')
	}
	fmt.Println("loop")
	os.Exit(1)
	//panic(fmt.Sprintf("ptr %d out of range", c.Pointer))
}

type Operator interface {
	Operate(cpu *CPU)
	String() string
}

type Nop struct{}

func (Nop) Operate(cpu *CPU) {
	cpu.Pointer++
}

func (Nop) String() string { return "NOP +0" }

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
			cpu.Program = append(cpu.Program, Nop{})
		case "jmp":
			cpu.Program = append(cpu.Program, Jmp(val))
		case "acc":
			cpu.Program = append(cpu.Program, Acc(val))
		default:
			panic(opVal)
		}
	}
	cpu.Run()
}
