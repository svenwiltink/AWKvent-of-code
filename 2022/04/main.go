package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	assignments := getAssignments()
	part1(assignments)
	part2(assignments)
}

func part1(assignments []Assignment) {
	count := 0
	for _, a := range assignments {
		if a.ContainsOther() {
			count++
		}
	}

	fmt.Println(count)
}

func part2(assignments []Assignment) {
	count := 0
	for _, a := range assignments {
		if a.Overlaps() {
			count++
		}
	}

	fmt.Println(count)
}

func getAssignments() []Assignment {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	res := make([]Assignment, 0)
	scan := bufio.NewScanner(f)
	for scan.Scan() {
		line := scan.Text()

		var a Assignment
		fmt.Sscanf(line, "%d-%d,%d-%d", &a.Upper.Left, &a.Upper.Right, &a.Lower.Left, &a.Lower.Right)
		res = append(res, a)
	}

	return res
}

type Assignment struct {
	Upper Plot
	Lower Plot
}

func (a Assignment) ContainsOther() bool {
	return a.Lower.Contains(a.Upper) || a.Upper.Contains(a.Lower)
}

func (a Assignment) Overlaps() bool {
	return !(a.Lower.Outside(a.Upper) || a.Upper.Outside(a.Lower))
}

type Plot struct {
	Left  int
	Right int
}

func (p Plot) Contains(o Plot) bool {
	return p.Left <= o.Left && p.Right >= o.Right
}

func (p Plot) Outside(o Plot) bool {
	return o.Right < p.Left || o.Left > p.Right
}
