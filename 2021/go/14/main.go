package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	start, mapping, counts := parseInput()

	for i := 0; i < 10; i++ {
		start, counts = tick(start, mapping, counts)
	}

	min, max := minmax(counts)
	fmt.Println(max - min)

	for i := 0; i < 30; i++ {
		start, counts = tick(start, mapping, counts)
	}

	min, max = minmax(counts)
	fmt.Println(max - min)
}

func minmax(counts map[rune]uint64) (uint64, uint64) {
	var min, max uint64

	for _, num := range counts {
		if num > max {
			max = num
		}

		if min == 0 || num < min {
			min = num
		}
	}

	return min, max
}

func tick(pairs map[[2]rune]uint64, t template, counts map[rune]uint64) (map[[2]rune]uint64, map[rune]uint64) {

	newpairs := make(map[[2]rune]uint64)
	for pair, amount := range pairs {
		m, exists := t[pair]
		if !exists {
			fmt.Printf("no mapping found for %s\n", string(pair[:]))
			continue
		}

		newpairs[[2]rune{pair[0], m}] += amount
		newpairs[[2]rune{m, pair[1]}] += amount

		counts[m] += amount
	}

	return newpairs, counts
}

type template map[[2]rune]rune

func parseInput() (map[[2]rune]uint64, template, map[rune]uint64) {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(f)

	if !scanner.Scan() {
		log.Fatal(scanner.Err())
	}

	pairs := make(map[[2]rune]uint64)
	start := []rune(scanner.Text())
	counts := make(map[rune]uint64)

	var curpair [2]rune
	for i := 0; i < len(start)-1; i++ {
		curpair[0] = start[i]
		curpair[1] = start[i+1]

		pairs[curpair]++
	}

	for _, rune := range start {
		counts[rune]++
	}

	t := make(template)

	if !scanner.Scan() {
		log.Fatal(scanner.Err())
	}

	for scanner.Scan() {
		line := scanner.Text()
		var source string
		var destination rune

		_, err = fmt.Sscanf(line, "%s -> %c", &source, &destination)
		if err != nil {
			log.Fatal(err)
		}

		t[*(*[2]rune)([]rune(source))] = destination
	}

	return pairs, t, counts
}
