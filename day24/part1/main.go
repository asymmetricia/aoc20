package main

import (
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/gif"
	"io/ioutil"
	"math"
	"os"
	"strings"

	"github.com/pdbogen/pencil"

	"github.com/pdbogen/aoc20/colors"
)

type Coord struct {
	X, Y int
}

func (c Coord) East() Coord {
	return Coord{c.X + 2, c.Y}
}
func (c Coord) West() Coord {
	return Coord{c.X - 2, c.Y}
}
func (c Coord) NorthEast() Coord {
	return Coord{c.X + 1, c.Y + 1}
}
func (c Coord) SouthEast() Coord {
	return Coord{c.X + 1, c.Y - 1}
}
func (c Coord) NorthWest() Coord {
	return Coord{c.X - 1, c.Y + 1}
}
func (c Coord) SouthWest() Coord {
	return Coord{c.X - 1, c.Y - 1}
}

func (c Coord) Execute(steps []string) Coord {
	for _, step := range steps {
		switch step {
		case "e":
			c = c.East()
		case "w":
			c = c.West()
		case "ne":
			c = c.NorthEast()
		case "nw":
			c = c.NorthWest()
		case "se":
			c = c.SouthEast()
		case "sw":
			c = c.SouthWest()
		default:
			panic(step)
		}
	}
	return c
}

func parseMove(line string) []string {
	var move []string
	for i := 0; i < len(line); i++ {
		switch line[i] {
		case 'e':
			move = append(move, "e")
		case 'w':
			move = append(move, "w")
		case 'n':
			fallthrough
		case 's':
			move = append(move, line[i:i+2])
			i++
		default:
			panic(line[i:])
		}
	}
	return move
}

func (c Coord) Plus(a Coord) Coord {
	return Coord{c.X + a.X, c.Y + a.Y}
}

var adjacency = []Coord{
	{-1, 1}, {1, 1},
	{-2, 0}, {2, 0},
	{-1, -1}, {1, -1},
}

// Number of flipped tiles
func (c Coord) Neighbors(flipped map[Coord]bool) int {
	count := 0
	for _, adj := range adjacency {
		if flipped[c.Plus(adj)] {
			count++
		}
	}
	return count
}

func live(flipped map[Coord]bool) map[Coord]bool {
	newFlipped := map[Coord]bool{}
	for c := range flipped {
		n := c.Neighbors(flipped)
		if n == 1 || n == 2 {
			newFlipped[c] = true
		}
		for _, adj := range adjacency {
			c := c.Plus(adj)
			if flipped[c] {
				// handled separately
				continue
			}
			n := c.Neighbors(flipped)
			if n == 2 {
				newFlipped[c] = true
			}
		}
	}
	return newFlipped
}

func main() {
	input, err := ioutil.ReadFile("day24.input")
	//input, err := ioutil.ReadFile("day24.demo")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var moves [][]string
	for _, line := range lines {
		moves = append(moves, parseMove(line))
	}
	flipped := map[Coord]bool{}
	for _, move := range moves {
		c := Coord{}.Execute(move)
		if flipped[c] {
			delete(flipped, c)
		} else {
			flipped[c] = true
		}
	}
	start := flipped
	fmt.Println("Part 1:", len(flipped))
	for day := 0; day < 100; day++ {
		flipped = live(flipped)
	}
	fmt.Println("Part 2:", len(flipped))

	minx, maxx := 0, 0
	miny, maxy := 0, 0
	for c := range flipped {
		if c.X < minx {
			minx = c.X
		}
		if c.X > maxx {
			maxx = c.X
		}
		if c.Y < miny {
			miny = c.Y
		}
		if c.Y > maxy {
			maxy = c.Y
		}
	}

	const dps = 4
	const fps = 48
	flipped = start
	anim := &gif.GIF{
		Image:    []*image.Paletted{render(flipped, minx, maxx, miny, maxy)},
		Delay:    []int{100 / fps},
		Disposal: []byte{gif.DisposalNone},
	}
	for day := 0; day < 100; day++ {
		n := 0
		next := live(flipped)
		changes := map[Coord]bool{}
		for c := range flipped {
			if !next[c] {
				changes[c] = false
			}
		}
		for c := range next {
			if !flipped[c] {
				changes[c] = true
			}
		}

		intermediate := map[Coord]bool{}
		for c := range flipped {
			intermediate[c] = true
		}

		perFrame := len(changes) / (fps / dps)
		for c, ch := range changes {
			if ch {
				intermediate[c] = true
			} else {
				delete(intermediate, c)
			}
			n++
			if n%perFrame == 0 {
				anim.Image = append(anim.Image, render(intermediate, minx, maxx, miny, maxy))
				anim.Delay = append(anim.Delay, 100/fps)
				anim.Disposal = append(anim.Disposal, gif.DisposalNone)
				n = 0
			}
		}
		if n > 0 {
			anim.Image = append(anim.Image, render(intermediate, minx, maxx, miny, maxy))
			anim.Delay = append(anim.Delay, 100/fps)
			anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		}
		flipped = next
		//anim.Image = append(anim.Image, render(flipped, minx, maxx, miny, maxy))
		//anim.Delay = append(anim.Delay, 100/fps)
		//anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		fmt.Println(day)
	}
	f, err := os.OpenFile("day24.gif", os.O_CREATE|os.O_WRONLY|os.O_TRUNC, os.FileMode(0644))
	if err != nil {
		panic(err)
	}
	optimize(anim.Image)
	if err := gif.EncodeAll(f, anim); err != nil {
		panic(err)
	}
	f.Close()
}

func optimize(imgs []*image.Paletted) {
	if len(imgs) < 2 {
		return
	}
	accum := image.NewPaletted(imgs[0].Rect, imgs[0].Palette)
	draw.Draw(accum, accum.Rect, image.NewUniform(color.Transparent), image.Point{}, draw.Over)

	tr := imgs[0].Palette.Index(color.Transparent)
	for _, img := range imgs[1:] {
		for i, v := range img.Pix {
			if v == accum.Pix[i] {
				img.Pix[i] = uint8(tr)
			} else {
				accum.Pix[i] = img.Pix[i]
			}
		}
	}
}

const radius = 5
const gap = 1

var offsets [][2]float64
var slope float64 // dx/dy

func init() {
	for _, i := range []float64{1, 3, 5, 7, 9, 11} {
		offsets = append(offsets, [2]float64{
			radius * math.Cos(i*math.Pi/6),
			radius * math.Sin(i*math.Pi/6),
		})
	}
	slope = math.Sin(math.Pi/6) / math.Cos(math.Pi/6)
}

func ScreenFromWorld(x, y int) (int, int) {
	return int(float64(x) * 2 * (radius + gap) * math.Cos(math.Pi/3)),
		int(float64(y) * 2 * (radius + gap) * math.Sin(math.Pi/3))
}

func render(flipped map[Coord]bool, minx, maxx, miny, maxy int) *image.Paletted {
	brx, bry := ScreenFromWorld(maxx-minx, maxy-miny)
	img := image.NewPaletted(
		image.Rect(0, 0, brx, bry),
		color.Palette{
			color.Black, colors.Green, colors.Red, color.Transparent})

	drawn := map[Coord]bool{}
	for c := range flipped {
		DrawHex(img, c.X-minx, c.Y-miny, colors.Red)
		for _, adj := range adjacency {
			ac := c.Plus(adj)
			if drawn[ac] || flipped[ac] {
				continue
			}
			DrawHex(img, ac.X-minx, ac.Y-miny, colors.Green)
			drawn[ac] = true
		}
	}
	return img
}

func DrawHex(img draw.Image, worldx, worldy int, color color.Color) {
	sx, sy := ScreenFromWorld(worldx, worldy)
	var points []image.Point
	for _, offset := range offsets {
		points = append(points, image.Point{
			int(float64(sx) + offset[0]),
			int(float64(sy) + offset[1]),
		})
	}
	for i := range points {
		pencil.Line(img, points[i], points[(i+1)%len(points)], color)
	}

	xLeft := float64(sx) - radius*math.Cos(math.Pi/6)
	xRight := float64(sx) + radius*math.Cos(math.Pi/6)
	yTop := float64(sy) - radius*math.Sin(math.Pi/6)
	yBottom := float64(sy) + radius*math.Sin(math.Pi/6)
	for x := xLeft; x < float64(sx); x++ {
		pencil.Line(img,
			image.Point{int(x), int(yTop - slope*(x-xLeft))},
			image.Point{int(x), int(yBottom + slope*(x-xLeft))},
			color)
	}
	for x := float64(sx); x <= xRight; x++ {
		pencil.Line(img,
			image.Point{int(x), int(yTop - slope*(xRight-x))},
			image.Point{int(x), int(yBottom + slope*(xRight-x))},
			color)
	}
}
