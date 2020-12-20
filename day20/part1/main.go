package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/pdbogen/aoc20/term"
)

const dimX = 10
const dimY = 10

type Coord struct {
	x, y int
}

type Tile struct {
	Id     int
	Cells  map[Coord]bool
	Placed bool
}

func (t *Tile) FlipX() {
	newCells := map[Coord]bool{}
	for c := range t.Cells {
		newCells[Coord{dimX - 1 - c.x, c.y}] = true
	}
	t.Cells = newCells
}

func (t *Tile) FlipY() {
	newCells := map[Coord]bool{}
	for c := range t.Cells {
		newCells[Coord{c.x, dimY - 1 - c.y}] = true
	}
	t.Cells = newCells
}

func (t *Tile) Rotate() {
	newCells := map[Coord]bool{}
	// ....    ....    ....    ....
	// ..#. -> .... -> .... -> .#..
	// ....    ..#.    .#..    ....
	// ....    ....    ....    ....
	// 2,1  -> 2,2  -> 1,2  -> 2,1
	// x,y  ->
	for c, v := range t.Cells {
		newCells[Coord{dimY - 1 - c.y, c.x}] = v
	}

	t.Cells = newCells
}

type Direction int

const (
	North Direction = iota
	East
	South
	West
)

var Directions = []Direction{North, East, South, West}

func (t *Tile) Match(b *Tile, dir Direction) bool {
	switch dir {
	case North:
		for x := 0; x < dimX; x++ {
			if t.Cells[Coord{x, 0}] != b.Cells[Coord{x, dimY - 1}] {
				return false
			}
		}
		return true
	case South:
		return b.Match(t, North)
	case East:
		for y := 0; y < dimY; y++ {
			a := t.Cells[Coord{dimX - 1, y}]
			b := b.Cells[Coord{0, y}]
			if a != b {
				return false
			}
		}
		return true
	case West:
		return b.Match(t, East)
	}
	panic(dir)
}

func (t *Tile) PrintAt(x, y int) {
	for i := 0; i < dimY; i++ {
		term.MoveCursor(x, y+i)
		for j := 0; j < dimX; j++ {
			if t.Cells[Coord{j, i}] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
	}
}

func main() {
	term.Clear()

	input, err := ioutil.ReadFile("day20.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var tiles []*Tile
	var tile *Tile
	var y int
	for _, line := range lines {
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "Tile ") {
			if tile != nil {
				tiles = append(tiles, tile)
			}
			tile = &Tile{
				Cells: map[Coord]bool{},
			}
			y = 0

			var err error
			tile.Id, err = strconv.Atoi(line[5 : len(line)-1])
			if err != nil {
				panic(err)
			}
			continue
		}
		for x, c := range line {
			if c == '#' {
				tile.Cells[Coord{x, y}] = true
			}
		}
		y++
	}
	tiles = append(tiles, tile)

	if tiles == nil || len(tiles) < 2 {
		panic(tiles)
	}

	grid := map[Coord]*Tile{
		Coord{0, 0}: tiles[0],
	}
	tiles[0].Placed = true

	//term.MoveCursor(0, 35)
trying:
	for {
	tiles:
		for _, tile := range tiles {
			if tile.Placed {
				continue
			}
			for i := 0; i < 2; i++ {
				for j := 0; j < 2; j++ {
					if i == 1 {
						tile.FlipX()
					}
					if j == 1 {
						tile.FlipY()
					}
					for k := 0; k < 4; k++ {
						tile.Rotate()
						for _, dir := range Directions {
							for consider := range grid {
								var candidate Coord
								switch dir {
								case North:
									candidate = Coord{consider.x, consider.y - 1}
								case East:
									candidate = Coord{consider.x + 1, consider.y}
								case South:
									candidate = Coord{consider.x, consider.y + 1}
								case West:
									candidate = Coord{consider.x - 1, consider.y}
								}
								if _, ok := grid[candidate]; ok {
									continue
								}
								if grid[consider].Match(tile, dir) {
									tile.Placed = true
									grid[candidate] = tile
									continue tiles
								}
							}
						}
					}
				}
			}
		}
		for _, tile := range tiles {
			if !tile.Placed {
				continue trying
			}
		}
		break
	}

	var minx, miny int
	for c := range grid {
		if c.x < minx {
			minx = c.x
		}
		if c.y < miny {
			miny = c.y
		}
	}
	shifted := map[Coord]*Tile{}
	for c, t := range grid {
		shifted[Coord{c.x - minx, c.y - miny}] = t
	}
	for x := 0; x < 12; x++ {
		for y := 0; y < 12; y++ {
			if t, ok := shifted[Coord{x, y}]; ok {
				t.PrintAt(x*12+1, y*11+2)
			}
		}
	}
	term.MoveCursor(1, 11*12)
	fmt.Println(
		shifted[Coord{0, 0}].Id *
			shifted[Coord{11, 0}].Id *
			shifted[Coord{0, 11}].Id *
			shifted[Coord{11, 11}].Id,
	)
}
