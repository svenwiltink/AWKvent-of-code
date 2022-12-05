package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

func main() {
	crane, moves := parseInput()
	part1(crane, moves)
	crane, moves = parseInput()
	part2(crane, moves)
}

func part1(c Crane, m []Move) {
	for _, move := range m {
		c.MoveN(move.From, move.To, move.Count)
	}
	fmt.Println(c.GetTop())
}

func part2(c Crane, m []Move) {
	for _, move := range m {
		c.MoveStack(move.From, move.To, move.Count)
	}
	fmt.Println(c.GetTop())
}

func parseInput() (Crane, []Move) {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	parts := strings.Split(string(data), "\n\n")

	crane := parseCrane(parts[0])
	moves := parseMoves(parts[1])
	return crane, moves
}

func parseCrane(data string) Crane {
	lines := strings.Split(data, "\n")

	nums := strings.NewReader(lines[len(lines)-1])

	var cols int
	for {
		_, err := fmt.Fscanf(nums, "%d", &cols)
		if err == io.EOF {
			break
		}
	}

	crane := make(Crane, cols+1)

	for i := len(lines) - 2; i >= 0; i-- {
		line := lines[i]

		for j := 0; j < cols; j++ {
			crane.Add(j+1, string(line[j*4+1]))
		}
	}

	return crane
}

func parseMoves(data string) []Move {
	lines := strings.Split(data, "\n")

	var moves []Move
	for _, line := range lines {
		if line == "" {
			break
		}

		var move Move
		_, err := fmt.Sscanf(line, "move %d from %d to %d", &move.Count, &move.From, &move.To)
		if err != nil {
			panic(err)
		}

		moves = append(moves, move)
	}

	return moves
}

type Move struct {
	From, To, Count int
}

type Stack []string

type Crane []Stack

func (c Crane) Add(column int, num string) {
	if num == " " {
		return
	}

	c[column] = append(c[column], num)
}

func (c Crane) Pop(from int) (block string) {
	c[from], block = c[from][:len(c[from])-1], c[from][len(c[from])-1]
	return
}

func (c Crane) Move(from, to int) {
	block := c.Pop(from)
	c.Add(to, block)
}

func (c Crane) MoveN(from, to, count int) {
	for i := 0; i < count; i++ {
		c.Move(from, to)
	}
}

func (c Crane) MoveStack(from, to, count int) {
	amount := len(c[from]) - count
	var s Stack
	c[from], s = c[from][:amount], c[from][amount:]
	c[to] = append(c[to], s...)
}

func (c Crane) GetTop() string {
	parts := make([]string, len(c))
	for i := 1; i < len(c); i++ {
		parts[i] = c[i][len(c[i])-1]
	}

	return strings.Join(parts, "")
}
