package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

// Perm calls f with each permutation of a.
func Perm(a []rune, f func([]rune) bool) {
	perm(a, f, 0)
}

// Permute the values at index i to len(a)-1.
// return early when f return true
func perm(a []rune, f func([]rune) bool, i int) bool {
	if i > len(a) {
		return f(a)
	}

	if perm(a, f, i+1) {
		return true
	}

	for j := i + 1; j < len(a); j++ {
		a[i], a[j] = a[j], a[i]
		if perm(a, f, i+1) {
			return true
		}
		a[i], a[j] = a[j], a[i]
	}

	return false
}

type Row struct {
	Input  []string
	Output []string

	KnownNumbers map[int]string
}

func (r Row) EasyNumbers() int {
	sum := 0
	for _, num := range r.Output {
		length := len(num)
		switch length {
		case 2:
			fallthrough
		case 3:
			fallthrough
		case 7:
			fallthrough
		case 4:
			sum++
		}
	}

	return sum
}

func (r Row) MapKnownNumbers() {
	for _, num := range r.Input {
		switch len(num) {
		case 2:
			r.KnownNumbers[1] = num
		case 3:
			r.KnownNumbers[7] = num
		case 7:
			r.KnownNumbers[8] = num
		case 4:
			r.KnownNumbers[4] = num
		}
	}
}

// layout is [TL, T, TR, M, BL, B, BR]
//
//          11
//         0  2
//         0  2
//          33
//         4  6
//         4  6
//          55
func (r Row) checkDisplay(layout []rune) bool {
	numbers := [][]rune{
		0: {layout[0], layout[1], layout[2], layout[4], layout[5], layout[6]},
		1: {layout[2], layout[6]},
		2: {layout[1], layout[2], layout[3], layout[4], layout[5]},
		3: {layout[1], layout[2], layout[3], layout[6], layout[5]},
		4: {layout[0], layout[2], layout[3], layout[6]},
		5: {layout[1], layout[0], layout[3], layout[6], layout[5]},
		6: {layout[1], layout[0], layout[3], layout[4], layout[5], layout[6]},
		7: {layout[1], layout[2], layout[6]},
		8: layout,
		9: {layout[1], layout[0], layout[2], layout[3], layout[5], layout[6]},
	}

	for i, segments := range numbers {
		numberExists := false
		for _, number := range r.Input {
			if len(segments) == len(number) && sliceSubset([]rune(number), segments) {
				numberExists = true
				r.KnownNumbers[i] = number
				break
			}
		}

		if !numberExists {
			return false
		}
	}
	return true
}

func (r Row) OutputNumber() int {
	output := 0
	for _, o := range r.Output {
		output *= 10
		output += r.SegmentToNumber(o)
	}

	return output
}

func (r Row) SegmentToNumber(segment string) int {
	for number, knownsegments := range r.KnownNumbers {
		if len(knownsegments) == len(segment) && sliceSubset([]rune(segment), []rune(knownsegments)) {
			return number
		}
	}

	return 0
}

func (r Row) FindMapping() {
	r.MapKnownNumbers()
	Perm([]rune("abcdefg"), func(a []rune) bool {
		if r.checkDisplay(a) {
			return true
		}
		return false
	})
}

func sliceSubset(subset, set []rune) bool {
	for _, item := range subset {
		inset := false
		for _, i := range set {
			if i == item {
				inset = true
				break
			}
		}

		if inset == false {
			return false
		}
	}

	return true
}

type display []rune

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	rows := parseInput(f)

	total := 0
	for _, row := range rows {
		total += row.EasyNumbers()
	}

	fmt.Println(total)

	total = 0
	for _, row := range rows {
		row.FindMapping()
		total += row.OutputNumber()
	}

	fmt.Println(total)
}

func parseInput(r io.Reader) []Row {
	scanner := bufio.NewScanner(r)

	rows := make([]Row, 0)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, "|")

		rows = append(rows, Row{
			Input:        strings.Fields(parts[0]),
			Output:       strings.Fields(parts[1]),
			KnownNumbers: make(map[int]string),
		})
	}

	return rows
}
