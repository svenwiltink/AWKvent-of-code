package main

import (
	"fmt"
	"image"
	"image/color"
	"image/png"
	"log"
	"os"
	"strings"
)

var frame = 0

type paper struct {
	contents   map[int]map[int]struct{}
	maxX, maxY int
}

func (p *paper) fold(f fold) {
	curX, curY := p.maxX, p.maxY
	p.DrawFrame(curX, curY)

	switch f.Direction {
	case 'x':
		p.foldVertical(f.Position)
	case 'y':
		p.foldHorizontal(f.Position)
	}

	p.DrawFrame(curX, curY)
}

func (p *paper) DrawFrame(x, y int) {
	upLeft := image.Point{0, 0}
	lowRight := image.Point{x + 1, y + 1}

	img := image.NewRGBA(image.Rectangle{upLeft, lowRight})
	for y := 0; y <= p.maxY; y++ {
		for x := 0; x <= p.maxX; x++ {
			if _, exists := p.contents[x][y]; exists {
				img.Set(x, y, color.White)
				continue
			}
			img.Set(x, y, color.Black)
		}
	}

	f, _ := os.Create(fmt.Sprintf("frames/%02d.png", frame))

	encoder := png.Encoder{CompressionLevel: png.NoCompression}
	encoder.Encode(f, img)

	frame++
}

func (p *paper) foldHorizontal(position int) {
	for x := 0; x <= p.maxX; x++ {
		for y := 0; y < position; y++ {
			_, currentExists := p.contents[x][y]
			if currentExists {
				continue
			}

			diff := position - y
			_, newExists := p.contents[x][position+diff]
			if newExists {
				p.contents[x][y] = struct{}{}
			}
		}
	}

	p.maxY = position - 1
}

func (p *paper) foldVertical(position int) {
	for x := 0; x < position; x++ {
		for y := 0; y <= p.maxY; y++ {
			_, currentExists := p.contents[x][y]
			if currentExists {
				continue
			}

			diff := position - x
			_, newExists := p.contents[position+diff][y]
			if newExists {
				if _, exists := p.contents[x]; !exists {
					p.contents[x] = make(map[int]struct{})
				}
				p.contents[x][y] = struct{}{}
			}
		}
	}

	p.maxX = position - 1
}

func (p *paper) countDots() int {
	total := 0
	for x := 0; x <= p.maxX; x++ {
		for y := 0; y <= p.maxY; y++ {
			if _, exists := p.contents[x][y]; exists {
				total++
			}
		}
	}

	return total
}

func (p paper) GoString() string {

	builder := new(strings.Builder)

	for y := 0; y <= p.maxY; y++ {
		var row string
		for x := 0; x <= p.maxX; x++ {
			if _, exists := p.contents[x][y]; exists {
				row += "#"
				continue
			}

			row += "."
		}

		builder.WriteString(row)
		builder.WriteRune('\n')
	}

	return builder.String()
}

type fold struct {
	Direction rune
	Position  int
}

func main() {
	p, folds := parseInput()
	firstfold, remaining := folds[0], folds[1:]

	p.fold(firstfold)
	fmt.Println(p.countDots())

	for _, fold := range remaining {
		p.fold(fold)
	}

	fmt.Printf("%#v\n", p)
}

func parseInput() (paper, []fold) {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	parts := strings.Split(string(file), "\n\n")
	p := paper{
		contents: make(map[int]map[int]struct{}),
	}

	folds := make([]fold, 0)
	for _, coords := range strings.Split(parts[0], "\n") {
		var x, y int
		_, err = fmt.Sscanf(coords, "%d,%d", &x, &y)
		if err != nil {
			log.Fatal(err)
		}

		_, exists := p.contents[x]
		if !exists {
			p.contents[x] = make(map[int]struct{})
		}

		p.contents[x][y] = struct{}{}

		if x > p.maxX {
			p.maxX = x
		}

		if y > p.maxY {
			p.maxY = y
		}
	}

	for _, foldstring := range strings.Split(parts[1], "\n") {
		var direction rune
		var position int
		_, err = fmt.Sscanf(foldstring, "fold along %c=%d", &direction, &position)
		if err != nil {
			log.Fatal(err)
		}

		folds = append(folds, fold{Direction: direction, Position: position})
	}

	return p, folds
}
