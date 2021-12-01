package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
)

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Panic(err)
	}

	defer f.Close()

	nums := parseNumbers(f)

	part1(nums)
	part2(nums)
}

func part1(nums []int) {
	count := 0
	previous := nums[0]

	for _, num := range nums {
		if num > previous {
			count++
		}
		previous = num
	}

	fmt.Println(count)
}

func part2(nums []int) {
	count := 0
	previous := 0

	for i := 0; i < len(nums)-2; i++ {
		sum := nums[i]
		sum += nums[i+1]
		sum += nums[i+2]
		if i > 0 && sum > previous {
			count++
		}
		previous = sum
	}

	fmt.Println(count)
}

func parseNumbers(r io.Reader) []int {
	scanner := bufio.NewScanner(r)

	nums := make([]int, 0)

	for scanner.Scan() {
		txt := scanner.Text()
		num, err := strconv.Atoi(txt)
		if err != nil {
			log.Fatal(err)
		}

		nums = append(nums, num)
	}

	err := scanner.Err()
	if err != nil {
		log.Fatal(err)
	}

	return nums
}
