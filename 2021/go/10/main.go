package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strings"
)

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	lines := parseInput(f)

	opposites := map[rune]rune{
		')': '(',
		']': '[',
		'}': '{',
		'>': '<',
	}

	points := map[rune]int{
		')': 3,
		']': 57,
		'}': 1197,
		'>': 25137,

		'(': 1,
		'[': 2,
		'{': 3,
		'<': 4,
	}

	part1score := 0
	part2scores := make([]int, 0)

	for _, line := range lines {
		left := make([]rune, 0, len(line))
		validline := true

	checkline:
		for _, char := range line {
			switch char {
			case '{', '[', '<', '(':
				left = append(left, char)
			case '}', ']', '>', ')':
				last := left[len(left)-1]

				if last != opposites[char] {
					part1score += points[char]
					validline = false
					break checkline
				}

				left = left[:len(left)-1]
			}
		}

		if !validline {
			continue
		}

		linescore := 0

		for i := len(left) - 1; i >= 0; i-- {
			linescore *= 5
			linescore += points[left[i]]
		}

		part2scores = append(part2scores, linescore)
	}

	fmt.Println(part1score)

	sort.Ints(part2scores)
	fmt.Println((part2scores[len(part2scores)/2]))
}

func parseInput(r io.Reader) []string {
	data, err := io.ReadAll(r)
	if err != nil {
		log.Fatal(err)
	}

	return strings.Split(string(data), "\n")
}
