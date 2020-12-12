package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	gif2 "image/gif"
	"io/ioutil"
	"os"
	"time"

	"github.com/pdbogen/aoc20/colors"
)

const demo1 = `L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL`

func render(lines [][]byte) *image.Paletted {
	const mul = 10
	const inset = 1
	ret := image.NewPaletted(image.Rect(0, 0, len(lines[0])*mul, len(lines)*mul), color.Palette{
		color.Black, colors.Red, colors.Green,
	})

	red := image.NewUniform(colors.Red)
	green := image.NewUniform(colors.Green)

	for y, line := range lines {
		for x, c := range line {
			img := image.Black
			if c == '#' {
				img = red
			}
			if c == 'L' {
				img = green
			}
			draw.Draw(
				ret,
				image.Rect(x*mul+inset, y*mul+inset, (x+1)*mul-1-inset, (y+1)*mul-1-inset),
				img,
				image.Point{},
				draw.Over)
		}
	}

	return ret
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

func main() {
	input, err := ioutil.ReadFile("day11.input")
	if err != nil {
		panic(err)
	}
	lines := bytes.Split(bytes.TrimSpace(input), []byte("\n"))
	//lines = bytes.Split(bytes.TrimSpace([]byte(demo1)), []byte("\n"))

	var changes int
	gif := &gif2.GIF{
		Image:    nil,
		Delay:    nil,
		Disposal: nil,
	}
	for {
		gif.Image = append(gif.Image, render(lines))
		gif.Delay = append(gif.Delay, 1)
		gif.Disposal = append(gif.Disposal, gif2.DisposalNone)
		lines, changes = run(lines)
		lines, changes = run(lines)
		if changes == 0 {
			break
		}
	}
	start := time.Now()
	optimize(gif.Image)
	fmt.Printf("optimization took %0.2fs\n", time.Since(start).Seconds())

	fmt.Println(bytes.Count(bytes.Join(lines, nil), []byte("#")))

	out, err := os.OpenFile("day11pt2.gif", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, os.FileMode(0644))
	if err != nil {
		panic(err)
	}
	if err := gif2.EncodeAll(out, gif); err != nil {
		panic(err)
	}
	out.Close()
}

func run(lines [][]byte) ([][]byte, int) {
	var changes int
	ret := make([][]byte, len(lines))
	for i, line := range lines {
		ret[i] = make([]byte, len(line))
		copy(ret[i], line)
	}
	for y, line := range lines {
		for x, c := range line {
			if c == '.' {
				continue
			}
			count := 0
			for dx := -1; dx <= 1; dx++ {
			directions:
				for dy := -1; dy <= 1; dy++ {
					if dx == 0 && dy == 0 {
						continue
					}

					seeX := x
					seeY := y
					for {
						seeX += dx
						seeY += dy
						if seeX < 0 || seeX >= len(line) {
							continue directions
						}

						if seeY < 0 || seeY >= len(lines) {
							continue directions
						}

						if lines[seeY][seeX] == 'L' {
							break
						}
						if lines[seeY][seeX] == '#' {
							count++
							break
						}
					}
				}
			}
			if c == 'L' && count == 0 {
				ret[y][x] = '#'
				changes++
			}
			if c == '#' && count >= 5 {
				ret[y][x] = 'L'
				changes++
			}
		}
	}
	return ret, changes
}
