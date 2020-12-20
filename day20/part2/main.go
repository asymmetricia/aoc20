package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math/rand"
	"strconv"
	"strings"
	"time"

	"github.com/pdbogen/aoc20/colors"
	"github.com/pdbogen/aoc20/term"
)

type Coord struct {
	x, y int
}

type Tile struct {
	Id       int
	Cells    map[Coord]bool
	Placed   bool
	Monsters []Coord
}

func (t Tile) Bounds() (max Coord) {
	for c := range t.Cells {
		if c.x > max.x {
			max.x = c.x
		}
		if c.y > max.y {
			max.y = c.y
		}
	}
	return Coord{max.x + 1, max.y + 1}
}

func (t *Tile) FlipX() {
	dimx := t.Bounds().x
	newCells := map[Coord]bool{}
	for c, v := range t.Cells {
		newCells[Coord{dimx - 1 - c.x, c.y}] = v
	}
	t.Cells = newCells
}

func (t *Tile) FlipY() {
	dimy := t.Bounds().y
	newCells := map[Coord]bool{}
	for c, v := range t.Cells {
		newCells[Coord{c.x, dimy - 1 - c.y}] = v
	}
	t.Cells = newCells
}

func (t *Tile) Rotate() {
	dimy := t.Bounds().y
	newCells := map[Coord]bool{}
	for c, v := range t.Cells {
		newCells[Coord{dimy - 1 - c.y, c.x}] = v
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
	bounds := t.Bounds()
	switch dir {
	case North:
		for x := 0; x < bounds.x; x++ {
			if t.Cells[Coord{x, 0}] != b.Cells[Coord{x, bounds.y - 1}] {
				return false
			}
		}
		return true
	case South:
		return b.Match(t, North)
	case East:
		for y := 0; y < bounds.y; y++ {
			a := t.Cells[Coord{bounds.x - 1, y}]
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
	bounds := t.Bounds()
	for ty := 0; ty < bounds.y; ty++ {
		term.MoveCursor(x, y+ty)
		for tx := 0; tx < bounds.x; tx++ {
			if t.Cells[Coord{tx, ty}] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
	}
	for _, m := range t.Monsters {
		term.ColorC(colors.Blue)
		for _, smc := range SeaMonster {
			term.MoveCursor(x+m.x+smc.x, y+m.y+smc.y)
			fmt.Print("O")
		}
		term.ColorReset()
	}
}

func parseTile(lines []string) (tile *Tile, linesConsumed int) {
	if !strings.HasPrefix(lines[0], "Tile ") {
		panic("not Tile… line: " + lines[0])
	}

	linesConsumed = 1
	tile = &Tile{Cells: map[Coord]bool{}}

	var err error
	tile.Id, err = strconv.Atoi(lines[0][5 : len(lines[0])-1])
	if err != nil {
		log.Fatalf("could not parse line ID in %q: %v", lines[0], err)
	}

	for y, line := range lines[1:] {
		if line == "" {
			return
		}
		linesConsumed++
		for x, c := range line {
			tile.Cells[Coord{x, y}] = c == '#'
		}
	}

	return
}

func Shift(grid map[Coord]*Tile, dx, dy int) map[Coord]*Tile {
	newGrid := map[Coord]*Tile{}
	for c, v := range grid {
		newGrid[Coord{c.x + dx, c.y + dy}] = v
	}
	return newGrid
}

func PrintGrid(grid map[Coord]*Tile, spaceX, spaceY int) {
	term.Clear()
	var bounds *Coord
	for c, t := range grid {
		if bounds == nil {
			bounds = &Coord{}
			*bounds = t.Bounds()
		}

		t.PrintAt((c.x+1)*(bounds.x+spaceX), (c.y+1)*(bounds.x+spaceY))
	}
}

func main() {
	rand.Seed(time.Now().UnixNano())
	term.HideCursor()
	defer term.ShowCursor()

	input, err := ioutil.ReadFile("day20.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var tiles []*Tile
	for i := 0; i < len(lines); i++ {
		t, con := parseTile(lines[i:])
		tiles = append(tiles, t)
		i += con
	}

	if tiles == nil || len(tiles) < 2 {
		panic("expected at least two tiles")
	}

	rand.Shuffle(len(tiles), func(i, j int) {
		tiles[i], tiles[j] = tiles[j], tiles[i]
	})

	grid := map[Coord]*Tile{
		Coord{0, 0}: tiles[0],
	}
	tiles[0].Placed = true

trying:
	for {
	tiles:
		for _, tile := range tiles {
			if tile.Placed {
				continue
			}
			for i := 0; i < 2; i++ {
				tile.FlipX()
				for j := 0; j < 2; j++ {
					tile.FlipY()
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
									if candidate.x < 0 {
										grid = Shift(grid, -candidate.x, 0)
										candidate.x = 0
										term.Clear()
										PrintGrid(grid, 2, 1)
									}
									if candidate.y < 0 {
										grid = Shift(grid, 0, -candidate.y)
										candidate.y = 0
										term.Clear()
										PrintGrid(grid, 2, 1)
									}
									term.ColorC(colors.Green)
									bounds := tile.Bounds()
									tile.PrintAt(
										(candidate.x+1)*(bounds.x+2),
										(candidate.y+1)*(bounds.y+1))
									term.ColorReset()
									continue tiles
								} else {
									if rand.Intn(100) <= 40 {
										bounds := tile.Bounds()
										term.ColorC(colors.Red)
										tile.PrintAt(
											(candidate.x+1)*(bounds.x+2),
											(candidate.y+1)*(bounds.y+1))
										term.ColorReset()
									}
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

	time.Sleep(300 * time.Millisecond)
	PrintGrid(grid, 2, 1)
	time.Sleep(300 * time.Millisecond)
	PrintGrid(grid, 0, 0)
	time.Sleep(300 * time.Millisecond)
	term.Clear()

	var image = &Tile{
		Cells: map[Coord]bool{},
	}
	for tc, tile := range grid {
		bounds := tile.Bounds()
		for c, v := range tile.Cells {
			if c.x == 0 || c.x == bounds.x-1 ||
				c.y == 0 || c.y == bounds.y-1 {
				continue
			}
			image.Cells[Coord{8*tc.x + c.x - 1, 8*tc.y + c.y - 1}] = v
			if rand.Intn(100) <= 5 {
				image.PrintAt(0, 0)
			}
		}
	}

	imageBounds := image.Bounds()
	for _, xform := range []func(){
		image.Rotate,
		image.Rotate,
		image.Rotate,
		image.FlipX,
		image.Rotate,
		image.Rotate,
		image.Rotate,
		image.FlipY,
		image.Rotate,
		image.Rotate,
		image.Rotate,
		image.FlipX,
		image.Rotate,
		image.Rotate,
		image.Rotate,
	} {
		for y := 0; y < imageBounds.y; y++ {
			image.PrintAt(0, 0)
			term.MoveCursor(1, y+1)
			term.ColorC(colors.Blue)
			for x := 0; x < imageBounds.x; x++ {
				fmt.Print("-")
				if IsMonster(Coord{x, y}, image.Cells) {
					image.Monsters = append(image.Monsters, Coord{x, y})
				}
			}
			term.ColorReset()
			time.Sleep(10 * time.Millisecond)
		}
		if len(image.Monsters) != 0 {
			break
		}
		xform()
	}

	term.MoveCursor(1, image.Bounds().y+2)

	active := 0
	for _, v := range image.Cells {
		if v {
			active++
		}
	}
	fmt.Println("Part 2: ", active-len(image.Monsters)*len(SeaMonster))
}

var SeaMonster = []Coord{
	{0, 0},
	{1, 1},
	{4, 1},
	{5, 0},
	{6, 0},
	{7, 1},
	{10, 1},
	{11, 0},
	{12, 0},
	{13, 1},
	{16, 1},
	{17, 0},
	{18, -1},
	{18, 0},
	{19, 0},
}

func IsMonster(c Coord, image map[Coord]bool) bool {
	for _, dc := range SeaMonster {
		if !image[Coord{
			c.x + dc.x,
			c.y + dc.y,
		}] {
			return false
		}
	}
	return true
}
