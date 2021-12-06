package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"time"
)

func main() {
	start := time.Now()
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	fish := parseInput(f)

	fmt.Println("input parsing", time.Since(start))
	start = time.Now()

	fish = breed(fish, 80)
	fmt.Println("duration", time.Since(start))
	start = time.Now()
	fmt.Println(fishcount(fish))

	fish = breed(fish, 256-80)
	fmt.Println("duration", time.Since(start))
	fmt.Println(fishcount(fish))
}

func breed(fish [9]int, days int) [9]int {
	newfish := [9]int{}
	for day := 0; day < days; day++ {
		for i := 0; i <= 8; i++ {
			if i == 0 {
				newfish[8] = fish[0]
				newfish[6] = fish[0]
				continue
			}

			if i == 7 {
				newfish[i-1] += fish[i]
				continue
			}

			newfish[i-1] = fish[i]
		}

		fish = newfish
	}

	return fish
}

func fishcount(fish [9]int) int {
	sum := 0
	for i := 0; i <= 8; i++ {
		sum += fish[i]
	}
	return sum
}

func parseInput(r io.Reader) [9]int {
	nums := make([]int, 0)

	line, err := io.ReadAll(r)
	if err != nil {
		log.Fatal(err)
	}

	parts := strings.Split(strings.TrimSpace(string(line)), ",")
	for _, n := range parts {
		number, err := strconv.Atoi(n)
		if err != nil {
			log.Fatal(err)
		}

		nums = append(nums, number)
	}

	fish := [9]int{}
	for _, num := range nums {
		fish[num]++
	}

	return fish
}
