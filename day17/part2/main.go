package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)


type coord struct{ x, y, z, w int }

func neighbors(space map[coord]bool, pt coord) int {
	ret := 0
	for _, w := range []int{-1, 0, 1} {
		for _, x := range []int{-1, 0, 1} {
			for _, y := range []int{-1, 0, 1} {
				for _, z := range []int{-1, 0, 1} {
					if x == 0 && y == 0 && z == 0 && w == 0 {
						continue
					}
					if space[coord{pt.x + x, pt.y + y, pt.z + z, pt.w + w}] {
						ret++
					}
				}
			}
		}
	}
	return ret
}

func bounds(space map[coord]bool) [4][2]int {
	var bounds [4][2]int
	for c, v := range space {
		if !v {
			continue
		}
		if c.x < bounds[0][0] {
			bounds[0][0] = c.x
		}
		if c.x > bounds[0][1] {
			bounds[0][1] = c.x
		}
		if c.y < bounds[1][0] {
			bounds[1][0] = c.y
		}
		if c.y > bounds[1][1] {
			bounds[1][1] = c.y
		}
		if c.z < bounds[2][0] {
			bounds[2][0] = c.z
		}
		if c.z > bounds[2][1] {
			bounds[2][1] = c.z
		}
		if c.w < bounds[3][0] {
			bounds[3][0] = c.w
		}
		if c.w > bounds[3][1] {
			bounds[3][1] = c.w
		}
	}
	return bounds
}

func printSpace(space map[coord]bool) {
	bounds := bounds(space)
	for w := bounds[3][0]; w <= bounds[3][1]; w++ {
		for y := bounds[1][0]; y <= bounds[1][1]; y++ {
			for z := bounds[2][0]; z <= bounds[2][1]; z++ {
				for x := bounds[0][0]; x <= bounds[0][1]; x++ {
					if space[coord{x, y, z, w}] {
						fmt.Print("#")
					} else {
						fmt.Print(".")
					}
				}
				fmt.Print("  ")
			}
			fmt.Println()
		}
		fmt.Println()
	}
}

func main() {
	input, err := ioutil.ReadFile("day17.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	//lines = []string{".#.", "..#", "###"}
	fmt.Println(lines)
	var space = map[coord]bool{}
	for y, line := range lines {
		for x, c := range line {
			if c == '#' {
				space[coord{x, y, 0, 0}] = true
			}
		}
	}
	for i := 0; i < 6; i++ {
		printSpace(space)
		fmt.Println()
		bounds := bounds(space)
		next := map[coord]bool{}
		for x := bounds[0][0] - 1; x <= bounds[0][1]+1; x++ {
			for y := bounds[1][0] - 1; y <= bounds[1][1]+1; y++ {
				for z := bounds[2][0] - 1; z <= bounds[2][1]+1; z++ {
					for w := bounds[3][0] - 1; w <= bounds[3][1]+1; w++ {
						c := coord{x, y, z, w}
						switch neighbors(space, c) {
						case 2:
							next[c] = space[c]
						case 3:
							next[c] = true
						default:
							next[c] = false
						}
					}
				}
			}
		}
		space = next
	}
	printSpace(space)
	fmt.Println()

	total := 0
	for _, v := range space {
		if v {
			total++
		}
	}
	fmt.Println(total)
}
