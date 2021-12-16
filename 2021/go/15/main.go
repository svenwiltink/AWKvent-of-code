package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type cave map[coord]*node

func (c cave) search(start *node) {
	queue := make(map[coord]*node)

	neighbours := c.getNeighbours(start)
	for _, neighbour := range neighbours {
		queue[neighbour.coords] = neighbour
	}

	for len(queue) > 0 {
		newQeueu := make(map[coord]*node)
		for _, item := range queue {
			newCost := item.pathCost + item.height
			neighbours := c.getNeighbours(item)
			for _, neighbour := range neighbours {
				if neighbour.visited && neighbour.pathCost < newCost {
					continue
				}

				neighbour.visited = true
				neighbour.pathCost = newCost
				newQeueu[neighbour.coords] = neighbour
			}

		}
		queue = newQeueu
	}
}

func (c cave) getNeighbours(current *node) []*node {
	result := make([]*node, 0)

	curCoords := current.coords
	if left, exists := c[coord{curCoords[0] - 1, curCoords[1]}]; exists {
		result = append(result, left)
	}
	if right, exists := c[coord{curCoords[0] + 1, curCoords[1]}]; exists {
		result = append(result, right)
	}
	if up, exists := c[coord{curCoords[0], curCoords[1] - 1}]; exists {
		result = append(result, up)
	}
	if down, exists := c[coord{curCoords[0], curCoords[1] + 1}]; exists {
		result = append(result, down)
	}

	return result
}

type coord [2]int

type node struct {
	coords coord
	height int

	pathCost int
	visited  bool
}

func main() {
	m, max := parseInput()

	start := m[coord{0, 0}]
	start.visited = true

	m.search(start)
	end := *m[coord{max[0] - 1, max[1] - 1}]
	fmt.Println(end.height + end.pathCost)

	m, max = parseInput()
	start = m[coord{0, 0}]
	start.visited = true

	columns := max[0]
	rows := max[1]
	for x := 0; x < columns*5; x++ {
		for y := 0; y < rows*5; y++ {
			if x < max[0] && y < max[1] {
				continue
			}

			coords := coord{x, y}
			height := m[coord{x % (columns), y % (rows)}].height
			height += x / columns
			height += y / rows

			for height > 9 {
				height -= 9
			}

			m[coords] = &node{
				coords: coords,
				height: height,
			}
		}
	}

	m.search(start)

	start = m[coord{0, 0}]
	start.visited = true
	m.search(start)
	end = *m[coord{max[0]*5 - 1, max[1]*5 - 1}]
	fmt.Println(end.height + end.pathCost)
}

func parseInput() (cave, coord) {
	f, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	cave := make(cave)

	lines := strings.Split(string(f), "\n")

	maxX := len(lines[0])
	maxY := len(lines)

	for y, line := range lines {
		for x, h := range line {
			height, err := strconv.Atoi(string(h))
			if err != nil {
				log.Fatal(err)
			}

			coord := coord{x, y}
			cave[coord] = &node{
				coords: coord,
				height: height,
			}
		}
	}

	return cave, coord{maxX, maxY}
}
