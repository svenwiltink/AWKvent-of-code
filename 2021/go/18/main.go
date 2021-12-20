package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
)

// this day is a mess. It decodes the input in a hot loop so it is slow. It runs though :D
func main() {
	nodes := parseInput()

	curNode := nodes[0]

	for i := 0; i < len(nodes); i++ {
		if i < len(nodes)-1 {
			curNode = curNode.add(nodes[i+1])
		}
		for curNode.reduce() {
		}
	}

	fmt.Println(curNode.magnitude())

	maxSum := 0
	for i := 0; i < len(nodes); i++ {
		for j := 0; j < len(nodes); j++ {
			if i == j {
				continue
			}

			nodes := parseInput()

			curNode = nodes[i]
			for curNode.reduce() {
			}
			curNode = curNode.add(nodes[j])
			for curNode.reduce() {
			}
			sum := curNode.magnitude()
			if sum > maxSum {
				maxSum = sum
			}

			nodes = parseInput()
			curNode = nodes[i]
			for curNode.reduce() {
			}
			curNode = curNode.add(nodes[j])
			for curNode.reduce() {
			}
			sum = curNode.magnitude()
			if sum > maxSum {
				maxSum = sum
			}
		}
	}

	fmt.Println(maxSum)
}

func parseInput() []*node {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(data), "\n")

	result := make([]*node, 0)
	for _, line := range lines {
		buf := bufio.NewReader(strings.NewReader(line))
		node := parseNode(buf, true)
		node.left.parent = node
		node.right.parent = node
		result = append(result, node)
	}

	return result
}

type node struct {
	parent *node

	left  *node
	right *node

	value int
}

func (n *node) magnitude() int {
	if n.isValue() {
		return n.value
	}

	return 3*n.left.magnitude() + 2*n.right.magnitude()
}

func (n *node) add(right *node) *node {
	parent := &node{
		left:  n,
		right: right,
	}

	n.parent = parent
	right.parent = parent

	return parent
}

func (n *node) reduce() bool {
	if n.explode() {
		return true
	}

	if n.split() {
		return true
	}

	return false
}

func (n *node) split() bool {
	high := n.findHighNumber()
	if high == nil {
		return false
	}

	left := &node{
		parent: high,
		value:  int(math.Floor(float64(high.value) / 2)),
	}

	right := &node{
		parent: high,
		value:  int(math.Ceil(float64(high.value) / 2)),
	}

	high.value = 0
	high.left = left
	high.right = right
	return true
}

func (n *node) explode() bool {
	pair, level := n.findNestedPair(0)
	if level != 4 {
		return false
	}

	left := pair.left
	right := pair.right

	previous := pair
	parent := pair.parent
	for parent != nil {
		if parent.left.isValue() {
			parent.left.value += left.value
			break
		} else {
			if parent.left != previous && parent.left.addRight(left.value) {
				break
			}
		}

		previous = parent
		parent = parent.parent
	}

	previous = pair
	parent = pair.parent
	for parent != nil {
		if parent.right.isValue() {
			parent.right.value += right.value
			break
		} else {
			if parent.right != previous && parent.right.addLeft(right.value) {
				break
			}
		}
		previous = parent
		parent = parent.parent
	}

	pair.left = nil
	pair.right = nil
	pair.value = 0

	return true
}

func (n *node) addLeft(value int) bool {
	if n.left.isValue() {
		n.left.value += value
		return true
	}

	if n.left.addLeft(value) {
		return true
	}

	if n.right.isValue() {
		n.right.value += value
		return true
	}

	return n.right.addLeft(value)
}

func (n *node) addRight(value int) bool {
	if n.right.isValue() {
		n.right.value += value
		return true
	}

	if n.right.addRight(value) {
		return true
	}

	if n.left.isValue() {
		n.left.value += value
		return true
	}

	return n.left.addRight(value)
}

func (n *node) String() string {
	if n.isValue() {
		return fmt.Sprintf("%d", n.value)
	}

	return fmt.Sprintf("[%s,%s]", n.left, n.right)
}

func (n *node) isValue() bool {
	return n.left == nil && n.right == nil
}

func (n *node) isPair() bool {
	return n.left != nil && n.right != nil && n.left.isValue() && n.right.isValue()
}

func (n *node) findHighNumber() *node {
	if n.isValue() {
		if n.value >= 10 {
			return n
		}
	}

	if n.left != nil {
		node := n.left.findHighNumber()
		if node != nil {
			return node
		}
	}

	if n.right != nil {
		node := n.right.findHighNumber()
		if node != nil {
			return node
		}
	}

	return nil
}

func (n *node) findNestedPair(curlevel int) (*node, int) {

	if n.isPair() {
		return n, curlevel
	}

	curlevel++

	if n.left != nil {
		deepest, level := n.left.findNestedPair(curlevel)
		if deepest != nil && level == 4 {
			return deepest, level
		}
	}

	if n.right != nil {
		deepest, level := n.right.findNestedPair(curlevel)
		if deepest != nil && level == 4 {
			return deepest, level
		}
	}

	return nil, curlevel
}

func parseNode(r *bufio.Reader, left bool) *node {
	token, err := r.Peek(1)
	if err != nil {
		panic(err)
	}

	switch token[0] {
	case '[':
		_, err = r.Discard(1) // discard opening bracket
		if err != nil {
			panic(err)
		}

		left := parseNode(r, true)

		if left.left != nil {
			left.left.parent = left
		}

		if left.right != nil {
			left.right.parent = left
		}

		// the parsing may or may not have dropped the , between the left and right node. Peek and
		// discard as needed.
		peek, err := r.Peek(1)
		if err != nil {
			panic(err)
		}

		if peek[0] == ',' {
			_, err = r.Discard(1)
			if err != nil {
				panic(err)
			}
		}

		right := parseNode(r, false)

		if right.left != nil {
			right.left.parent = right
		}

		if right.right != nil {
			right.right.parent = right
		}

		// the parsing may or may not have dropped the ] after a right node. Peek and
		// discard as needed. EOF is fine, it just means the end has been reached.
		peek, err = r.Peek(1)
		if err != nil && !errors.Is(err, io.EOF) {
			panic(err)
		}

		if err == nil && peek[0] == ']' {
			_, err = r.Discard(1)
			if err != nil {
				panic(err)
			}
		}

		return &node{
			left:  left,
			right: right,
		}
	default:

		delim := ']'
		if left {
			delim = ','
		}

		bytes, err := r.ReadBytes(byte(delim))
		if err != nil {
			panic(err)
		}

		// drop last byte
		value, err := strconv.Atoi(string(bytes[:len(bytes)-1]))
		if err != nil {
			panic(err)
		}
		return &node{value: value}
	}
}
