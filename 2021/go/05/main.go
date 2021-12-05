package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type Coord struct {
	X, Y int
}

type Edge struct {
	From, To Coord
}

type OceanFloor map[int]map[int]int

func (o OceanFloor) Increase(x, y int) {
	_, exists := o[x]
	if !exists {
		o[x] = make(map[int]int)
	}

	o[x][y]++
}

func (o OceanFloor) SafestRoute() int {
	points := 0
	for _, column := range o {
		for _, value := range column {
			if value >= 2 {
				points++
			}
		}
	}

	return points
}

func (o OceanFloor) GoString() string {
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

	var builder strings.Builder
	for y := 0; y <= maxY; y++ {
		var row string
		for x := 0; x <= maxX; x++ {
			value := o[x][y]
			if value == 0 {
				row += "."
				continue
			}

			row += strconv.Itoa(value)
		}

		builder.WriteString(row)
		builder.WriteRune('\n')
	}

	return builder.String()
}

func (o OceanFloor) DrawStraightEdge(e Edge) {
	startx := e.From.X
	starty := e.From.Y
	endx := e.To.X
	endy := e.To.Y

	shouldDraw := false
	if e.From.X == e.To.X { // vertical line
		shouldDraw = true
		if e.To.Y < e.From.Y {
			starty = e.To.Y
			endy = e.From.Y
		}
	}

	if e.From.Y == e.To.Y { // horizonatal line
		shouldDraw = true
		if e.To.X < e.From.X {
			startx = e.To.X
			endx = e.From.X
		}
	}

	if !shouldDraw {
		return
	}

	for x := startx; x <= endx; x++ {
		for y := starty; y <= endy; y++ {
			o.Increase(x, y)
		}
	}
}

func (o OceanFloor) DrawDiag(e Edge) {
	if math.Abs(float64(e.To.Y-e.From.Y)) != math.Abs(float64(e.To.X-e.From.X)) {
		return
	}

	from := e.From
	to := e.To

	if to.X < from.X { // swap so we always start with lower x
		from, to = to, from
	}

	direction := 1 // go up by default
	if to.Y < from.Y {
		direction = -1 // go down instead
	}

	y := from.Y
	for x := from.X; x <= to.X; x++ {
		o.Increase(x, y)
		y += direction
	}
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	edges := parseInput(f)

	floor := make(OceanFloor)

	for _, edge := range edges {
		floor.DrawStraightEdge(edge)
	}

	fmt.Println(floor.SafestRoute())

	for _, edge := range edges {
		floor.DrawDiag(edge)
	}

	fmt.Println(floor.SafestRoute())
}

func parseInput(r io.Reader) []Edge {
	scanner := bufio.NewScanner(r)

	edges := make([]Edge, 0)

	for scanner.Scan() {
		line := scanner.Text()

		var x1, y1, x2, y2 int
		_, err := fmt.Sscanf(line, "%d,%d -> %d,%d", &x1, &y1, &x2, &y2)
		if err != nil {
			log.Panicln(err)
		}

		edges = append(edges, Edge{
			From: Coord{x1, y1},
			To:   Coord{x2, y2},
		})
	}

	return edges
}
