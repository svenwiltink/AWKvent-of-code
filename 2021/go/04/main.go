package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

type BingoCard struct {
	Score [5][5]BingoNumber
}

func (b *BingoCard) PlayNumber(num int) {
	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			if b.Score[i][j].Number == num {
				b.Score[i][j].Hit = true
			}
		}
	}
}

func (b *BingoCard) Sum() int {
	sum := 0
	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			if b.Score[i][j].Hit == false {
				sum += b.Score[i][j].Number
			}
		}
	}

	return sum
}

func (b *BingoCard) HasWon() bool {
	return b.checkColumns() || b.checkRows()
}

func (b *BingoCard) checkRows() bool {
	for i := 0; i < 5; i++ {
		columnwon := true
		for j := 0; j < 5; j++ {
			if b.Score[i][j].Hit == false {
				columnwon = false
				break
			}
		}

		if columnwon {
			return true
		}
	}

	return false
}

func (b *BingoCard) checkColumns() bool {
	for i := 0; i < 5; i++ {
		columnwon := true
		for j := 0; j < 5; j++ {
			if b.Score[j][i].Hit == false {
				columnwon = false
				break
			}
		}

		if columnwon {
			return true
		}
	}

	return false
}

type BingoNumber struct {
	Number int
	Hit    bool
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	nums, cards := parseCards(f)

	part1(nums, cards)
	part2(nums, cards)
}

func parseCards(f io.Reader) ([]int, []*BingoCard) {
	scanner := bufio.NewScanner(f)

	// scan numbers
	scanner.Scan()
	numline := scanner.Text()
	numstrings := strings.Split(numline, ",")

	nums := make([]int, len(numstrings))
	for i, numstring := range numstrings {
		var err error
		nums[i], err = strconv.Atoi(numstring)
		if err != nil {
			log.Fatal(err)
		}
	}

	cards := make([]*BingoCard, 0)
	for {
		card := BingoCard{}
		if !scanner.Scan() {
			break // skip empty newline or break when EOF
		}

		for i := 0; i < 5; i++ {
			scanner.Scan()
			row := scanner.Text()

			for wi, word := range strings.Fields(row) {
				num, err := strconv.Atoi(word)
				if err != nil {
					log.Fatal(err)
				}

				card.Score[i][wi] = BingoNumber{
					Number: num,
				}
			}
		}

		cards = append(cards, &card)
	}

	return nums, cards
}

func part1(nums []int, cards []*BingoCard) {
	for _, num := range nums {
		for _, card := range cards {
			card.PlayNumber(num)
			if card.HasWon() {
				fmt.Println(card.Sum() * num)
				return
			}
		}
	}
}

func part2(nums []int, cards []*BingoCard) {
	for _, num := range nums {
		newcards := make([]*BingoCard, 0)
		for _, card := range cards {
			card.PlayNumber(num)
			if !card.HasWon() {
				newcards = append(newcards, card)
				continue
			}

			if card.HasWon() {
				if len(cards) == 1 {
					fmt.Println(card.Sum() * num)
					return
				}
			}
		}

		cards = newcards
	}
}
