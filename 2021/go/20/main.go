package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type algorithm []byte

type coords [2]int

type image struct {
	algorithm algorithm
	contents  map[coords]byte

	min coords
	max coords
}

func (i *image) reduce(iteration int) {
	newContents := make(map[coords]byte)

	// increase bounds by 1 as that is the max overlap of the 3x3 area
	i.min = coords{i.min[0] - 1, i.min[1] - 1}
	i.max = coords{i.max[0] + 1, i.max[1] + 1}

	for x := i.min[0]; x <= i.max[0]; x++ {
		for y := i.min[1]; y <= i.max[1]; y++ {

			pixelbuf := make([]byte, 0, 9)
			for yd := -1; yd <= 1; yd++ {
				for xd := -1; xd <= 1; xd++ {
					pixel, exists := i.contents[coords{x + xd, y + yd}]
					if !exists {
						if iteration%2 == 1 { // hack because the input enhancement makes the background flash
							pixel = '#'
						} else {
							pixel = '.'
						}
					}

					if pixel == '#' {
						pixel = '1'
					} else {
						pixel = '0'
					}

					pixelbuf = append(pixelbuf, pixel)
				}
			}

			value, err := strconv.ParseInt(string(pixelbuf), 2, 64)
			if err != nil {
				panic(err)
			}

			char := i.algorithm[value]
			newContents[coords{x, y}] = char

		}
	}

	i.contents = newContents
}

func (i image) Lit() int {
	lit := 0
	for y := i.min[1]; y <= i.max[1]; y++ {
		for x := i.min[0]; x <= i.max[0]; x++ {
			if i.contents[coords{x, y}] == '#' {
				lit++
			}
		}
	}

	return lit
}

func (i image) GoString() string {
	var builder strings.Builder

	for y := i.min[1]; y <= i.max[1]; y++ {
		for x := i.min[0]; x <= i.max[0]; x++ {
			builder.WriteByte(i.contents[coords{x, y}])
		}
		builder.WriteByte('\n')
	}

	return builder.String()
}

func main() {
	image := parseInput()

	for i := 0; i < 50; i++ {
		image.reduce(i)

		if i == 1 {
			fmt.Println(image.Lit())
		}
	}

	fmt.Println(image.Lit())
}

func parseInput() image {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}

	scanner := bufio.NewScanner(f)

	if !scanner.Scan() {
		panic(scanner.Err())
	}

	img := image{
		contents: make(map[coords]byte),
	}

	img.algorithm = algorithm(scanner.Text())

	scanner.Scan() //skip line

	y := 0
	maxx := 0
	for scanner.Scan() {
		line := scanner.Text()
		for x, r := range []byte(line) {
			img.contents[coords{x, y}] = r

			if x > maxx {
				maxx = x
			}
		}
		y++
	}

	img.max = coords{maxx, y - 1}

	return img
}
