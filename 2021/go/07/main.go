package main

import (
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	pos := parseInput(f)
	fmt.Println(pos.minFuels())
}

type positions map[int]int

func (p positions) maxPos() int {
	max := 0
	for i := range p {
		if i > max {
			max = i
		}
	}

	return max
}

func (p positions) minFuels() (int, int) {
	constantBurn := -1
	nonConstantBurn := -1

	for position := 0; position <= p.maxPos(); position++ {
		curFuel := p.calculateConstantBurn(position)
		if constantBurn == -1 || curFuel < constantBurn {
			constantBurn = curFuel
		}

		curFuel = p.calculateNonConstantBurn(position)
		if nonConstantBurn == -1 || curFuel < nonConstantBurn {
			nonConstantBurn = curFuel
		}
	}

	return constantBurn, nonConstantBurn
}

func (p positions) calculateConstantBurn(loc int) int {
	fuel := 0

	for position, amount := range p {
		fuel += int(math.Abs(float64(loc-position))) * amount
	}

	return fuel
}

func (p positions) calculateNonConstantBurn(loc int) int {
	fuel := 0

	for position, amount := range p {
		diff := int(math.Abs(float64(loc - position)))
		fuel += (diff * (diff + 1)) / 2 * amount
	}

	return fuel
}

func parseInput(r io.Reader) positions {
	file, err := io.ReadAll(r)
	if err != nil {
		log.Fatal(err)
	}

	nums := make(positions)

	for _, n := range strings.Split(string(file), ",") {
		num, err := strconv.Atoi(n)
		if err != nil {
			log.Fatal(err)
		}
		nums[num]++
	}

	return nums
}
