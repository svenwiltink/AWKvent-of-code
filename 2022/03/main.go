package main

import (
	"bufio"
	"fmt"
	"os"
	"unicode"

	"golang.org/x/exp/slices"
)

func main() {
	sacks := getRuckSacks()
	part1(sacks)
	part2(sacks)
}

func part1(sacks []rucksack) {
	sum := 0
	for _, sack := range sacks {
		sum += getPriority(sack.InBoth())
	}

	fmt.Println(sum)
}

func part2(sacks []rucksack) {
	result := 0
	for i := 0; i < len(sacks); i += 3 {
		data := sacks[i].Unique()
		data = append(data, sacks[i+1].Unique()...)
		data = append(data, sacks[i+2].Unique()...)

		count := make(map[rune]int)

		for _, r := range data {
			count[r]++
		}

		for i, c := range count {
			if c == 3 {
				result += getPriority(i)
			}
		}
	}

	fmt.Println(result)
}

func getPriority(r rune) int {
	if unicode.IsUpper(r) {
		return int(r-'A') + 27
	}

	return int(r-'a') + 1
}

func getRuckSacks() []rucksack {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	sacks := make([]rucksack, 0)
	scan := bufio.NewScanner(f)
	for scan.Scan() {
		line := scan.Text()
		sacks = append(sacks, rucksack(line))
	}

	return sacks
}

type rucksack []rune

func (r rucksack) One() []rune {
	return r[0 : len(r)/2]
}

func (r rucksack) Two() []rune {
	return r[len(r)/2:]
}

func (r rucksack) String() string {
	return fmt.Sprintf("\none: %s\ntwo: %s\n", string(r.One()), string(r.Two()))
}

func (r rucksack) Unique() []rune {
	slices.Sort(r)
	return slices.Compact(r)
}

func (r rucksack) InBoth() rune {
	for _, a := range r.One() {
		for _, b := range r.Two() {
			if a == b {
				return a
			}
		}
	}

	return 0
}
