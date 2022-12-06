package main

import (
	"fmt"
	"os"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println(getMarker(data, 4))
	fmt.Println(getMarker(data, 14))
}

func getMarker(data []byte, length int) int {
	for i := 0; i < len(data)-length; i++ {
		cur := data[i : i+length]
		if isUnique(cur, length) {
			return i + length
		}
	}

	return -1
}

func isUnique(data []byte, length int) bool {
	seen := make(map[byte]struct{})

	for _, d := range data {
		seen[d] = struct{}{}
	}

	return len(seen) == length
}
