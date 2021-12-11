package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/fatih/color"
)

type Octopuses map[int]map[int]*Octopus

func (o Octopuses) AddOctopus(x, y, level int) {
	_, exists := o[x]
	if !exists {
		o[x] = make(map[int]*Octopus)
	}

	o[x][y] = &Octopus{Level: level}
}

func (o Octopuses) ResetFlash() {
	for _, column := range o {
		for _, octo := range column {
			octo.Flashed = false
		}
	}
}

func (o Octopuses) Count() int {
	return len(o[0]) * len(o)
}

var step = 0

func (o Octopuses) Tick() int {
	step++
	o.ResetFlash()

	flashes := 0
	for x, column := range o {
		for y, octo := range column {
			if octo.Flashed {
				continue
			}

			octo.Level++

			if octo.Level > 9 {
				flashes += o.FlashOcto(x, y)
			}
		}
	}

	time.Sleep(100 * time.Millisecond)
	return flashes
}

func (o Octopuses) FlashOcto(Ox, Oy int) int {
	o[Ox][Oy].Flashed = true
	o[Ox][Oy].Level = 0

	fmt.Printf("step: %d\n%#v\n\n", step, o)
	time.Sleep(20 * time.Millisecond)

	totalFlashes := 1
	for x := -1; x <= 1; x++ {
		for y := -1; y <= 1; y++ {
			nx := Ox + x
			ny := Oy + y
			octo, exists := o[nx][ny]

			if !exists {
				continue
			}

			if octo.Flashed {
				continue
			}

			octo.Level++

			if octo.Level > 9 {
				totalFlashes += o.FlashOcto(nx, ny)
			}
		}
	}

	return totalFlashes
}

func (o Octopuses) GoString() string {
	var maxX, maxY int
	for x, yvalues := range o {
		if x > maxX {
			maxX = x
		}

		for y := range yvalues {
			if y > maxY {
				maxY = y
			}
		}
	}

	red := color.New(color.FgHiRed)
	var builder strings.Builder
	for y := 0; y <= maxY; y++ {
		var row string
		for x := 0; x <= maxX; x++ {
			value := o[x][y]
			strval := strconv.Itoa(value.Level)
			if value.Flashed {
				row += red.Sprint(strval)
				continue
			}

			row += strval
		}

		builder.WriteString(row)
		builder.WriteRune('\n')
	}

	return builder.String()
}

type Octopus struct {
	Level   int
	Flashed bool
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	octos := parseInput(f)

	flashes := 0
	for i := 0; i < 100; i++ {
		flashes += octos.Tick()
	}

	fmt.Println(flashes)

	octoCount := octos.Count()

	step := 100
	for octos.Tick() != octoCount {
		step++
	}

	fmt.Printf("step: %d\n%#v\n\n", step, octos)

	fmt.Println(step + 1)
}

func parseInput(r io.Reader) Octopuses {
	scanner := bufio.NewScanner(r)

	result := make(Octopuses)
	y := 0
	for scanner.Scan() {
		line := scanner.Text()
		for x, num := range line {
			level, err := strconv.Atoi(string([]rune{num}))
			if err != nil {
				log.Fatalln(err)
			}
			result.AddOctopus(x, y, level)
		}

		y++
	}

	return result
}
