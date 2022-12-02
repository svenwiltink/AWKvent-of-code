package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	matches := getMatches()
	fmt.Println(totalScore(matches))
	fmt.Println(manipulatedScore(matches))
}

func totalScore(m []Match) int {
	score := 0
	for _, match := range m {
		score += match.Score()
	}

	return score
}

func manipulatedScore(m []Match) int {
	score := 0
	for _, match := range m {
		match.Manipulate()
		score += match.Score()
	}

	return score
}

func getMatches() []Match {
	matches := make([]Match, 0)
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		text := scanner.Text()

		var match Match
		_, err := fmt.Sscanf(text, "%c %c", &match.Other, &match.Me)
		if err != nil {
			panic(err)
		}
		match.Normalise()
		matches = append(matches, match)
	}

	return matches
}

type Match struct {
	Me    rune
	Other rune
}

const (
	rock = iota + 1
	paper
	scissors
)

const (
	lose = iota + 1
	draw
	win
)

func (m *Match) Normalise() {
	m.Me = m.Me - 'X' + 1
	m.Other = m.Other - 'A' + 1
}

func (m *Match) Score() int {
	me := m.Me
	other := m.Other

	if me == other {
		return int(me + 3)
	}

	switch {
	case me == rock && other == scissors:
		fallthrough
	case me == paper && other == rock:
		fallthrough
	case me == scissors && other == paper:
		return int(me + 6)
	}

	return int(me)
}

func (m *Match) Manipulate() {
	other := m.Other
	output := m.Me

	switch {
	case output == draw:
		m.Me = other
	case output == lose && other == scissors:
		fallthrough
	case output == win && other == rock:
		m.Me = paper
	case output == lose && other == rock:
		fallthrough
	case output == win && other == paper:
		m.Me = scissors
	case output == lose && other == paper:
		fallthrough
	case output == win && other == scissors:
		m.Me = rock
	}
}
