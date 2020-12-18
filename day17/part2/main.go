package main

import (
	"fmt"
	"image"
	"image/color"
	"image/color/palette"
	"image/draw"
	"image/gif"
	"io/ioutil"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/fogleman/fauxgl"

	"github.com/pdbogen/aoc20/colors"
)

var width, height = 1200, 300
var eye = fauxgl.V(0, 30, 60)
var matrix = fauxgl.
	LookAt(eye, fauxgl.V(0, 0, 0), fauxgl.V(0, 1, 0)).
	Perspective(30, float64(width)/float64(height), 1, 100)

func render(space map[coord]bool) *image.Paletted {
	context := fauxgl.NewContext(width, height)
	context.ClearColorBufferWith(fauxgl.MakeColor(color.Black))

	red := fauxgl.NewPhongShader(matrix, fauxgl.V(-0.75, 1, 0.25).Normalize(), eye)
	red.ObjectColor = fauxgl.MakeColor(colors.Red)

	green := fauxgl.NewPhongShader(matrix, fauxgl.V(-0.75, 1, 0.25).Normalize(), eye)
	green.ObjectColor = fauxgl.MakeColor(colors.Green)

	bounds := bounds(space)
	for pt := range space {
		context.Shader = red
		if -pt.w%2 == 1 || pt.w%2 == 1 {
			context.Shader = green
		}
		context.DrawMesh(fauxgl.NewVoxelMesh([]fauxgl.Voxel{
			{
				X: pt.x + pt.w*(bounds[1][1]-bounds[1][0]+5),
				Y: pt.y,
				Z: pt.z,
			},
		}))
	}

	img := context.Image()
	pi := image.NewPaletted(img.Bounds(), palette.WebSafe)
	draw.Draw(pi, pi.Bounds(), img, image.Point{}, draw.Over)
	return pi
}

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

	workWg := &sync.WaitGroup{}
	writeWg := &sync.WaitGroup{}
	var worker = func(req <-chan coord, res chan<- coord) {
		for c := range req {
			switch neighbors(space, c) {
			case 2:
				if space[c] {
					res <- c
				}
			case 3:
				res <- c
			}
		}
		workWg.Done()
	}
	var writer = func(res <-chan coord, next map[coord]bool) {
		for c := range res {
			next[c] = true
		}
		writeWg.Done()
	}

	start := time.Now()
	anim := &gif.GIF{}
	for i := 0; i < 30; i++ {
		next := map[coord]bool{}
		req := make(chan coord)
		res := make(chan coord)
		for i := 0; i < 100; i++ {
			workWg.Add(1)
			go worker(req, res)
		}

		writeWg.Add(1)
		go writer(res, next)

		fmt.Printf("frame %d, %0.2fs\n", i, time.Since(start).Seconds())
		start = time.Now()
		anim.Image = append(anim.Image, render(space))
		anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		anim.Delay = append(anim.Delay, 100)
		bounds := bounds(space)
		for x := bounds[0][0] - 1; x <= bounds[0][1]+1; x++ {
			for y := bounds[1][0] - 1; y <= bounds[1][1]+1; y++ {
				for z := bounds[2][0] - 1; z <= bounds[2][1]+1; z++ {
					for w := bounds[3][0] - 1; w <= bounds[3][1]+1; w++ {
						req <- coord{x, y, z, w}
					}
				}
			}
		}

		close(req)
		workWg.Wait()
		close(res)
		writeWg.Wait()
		space = next

		if i == 5 {
			total := 0
			for _, v := range space {
				if v {
					total++
				}
			}
			fmt.Println(total)
		}
	}
	render(space)

	f, err := os.OpenFile("day17pt2.gif", os.O_CREATE|os.O_TRUNC|os.O_WRONLY, os.FileMode(0644))
	if err != nil {
		panic(err)
	}
	if err := gif.EncodeAll(f, anim); err != nil {
		panic(err)
	}
	f.Close()
}
