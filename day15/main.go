package main

import (
	"fmt"
	image "image"
	"image/color"
	"image/draw"
	"image/gif"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	input, err := ioutil.ReadFile("day15.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	//lines = []string{"0,3,6"}
	//lines = []string{"2,1,3"}

	var pixels []struct {
		x int
		c color.Color
	}

	var heard = -1
	var currentTurn int
	var mem = map[int]int{}
	for _, num := range strings.Split(lines[0], ",") {
		num, _ := strconv.Atoi(num)
		if heard > -1 {
			mem[heard] = currentTurn
		}
		heard = num
		currentTurn++
	}

	max := 0
	for currentTurn < 30000000 {
		speak := 0
		if heardTurn, ok := mem[heard]; ok {
			speak = currentTurn - heardTurn
		}
		mem[heard] = currentTurn
		pixels = append(pixels, struct {
			x int
			c color.Color
		}{x: heard, c: color.White})
		if heard > max {
			max = heard
		}

		heard = speak
		currentTurn++
		if currentTurn == 2020 {
			fmt.Println("part 1", heard)
		}
	}

	const scale = 16
	const fps = 30
	const delay = 100 / fps
	const seconds = 5
	dim := int(math.Ceil(math.Sqrt(float64(max / scale))))
	ppf := int(math.Ceil(float64(len(pixels)) / (fps * seconds)))

	bg := image.NewPaletted(image.Rect(0, 0, dim, dim), color.Palette{
		color.Black, color.White, color.Transparent,
	})
	img := &gif.GIF{
		Image:    []*image.Paletted{bg},
		Delay:    []int{delay},
		Disposal: []byte{gif.DisposalNone},
	}
	for i := 0; i < len(pixels); i += ppf {
		fmt.Printf("\rrendering %d/%d (%d%%)", i, len(pixels), i*100/len(pixels))
		frame := image.NewPaletted(bg.Rect, bg.Palette)
		draw.Draw(frame, frame.Rect, image.NewUniform(color.Transparent), image.Point{}, draw.Over)
		for j := i; j < i+ppf && j < len(pixels); j++ {
			x := pixels[j].x / scale
			frame.SetColorIndex(x%dim, x/dim, 1)
		}
		img.Image = append(img.Image, frame)
		img.Delay = append(img.Delay, delay)
		img.Disposal = append(img.Disposal, gif.DisposalNone)
	}
	fmt.Printf("\rrendering %d/%d (100%%)\n", len(pixels), len(pixels))

	file, err := os.OpenFile("day15pt2.gif", os.O_CREATE|os.O_TRUNC|os.O_WRONLY, os.FileMode(0644))
	if err != nil {
		panic(err)
	}
	if err := gif.EncodeAll(file, img); err != nil {
		panic(err)
	}
	file.Close()
	fmt.Println("part 2", heard)
}
