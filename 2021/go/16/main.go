package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strconv"
)

const (
	IDValue = 4
)

func main() {
	transmission := parseInput()
	packets := readPacket(transmission.reader, 1)
	fmt.Printf("%#v\n", packets[0].VersionSum())
	fmt.Printf("%#v\n", packets[0].Value())
}

func readPacket(r io.Reader, subpacket int) []packet {
	subpackets := make([]packet, 0)
	for i := 0; i < subpacket; i++ {
		var p basepacket
		var header [3]byte
		_, err := io.ReadFull(r, header[:])
		if errors.Is(err, io.ErrUnexpectedEOF) {
			return subpackets
		}

		if errors.Is(err, io.EOF) {
			return subpackets
		}

		p.version, err = strconv.ParseInt(string(header[:]), 2, 32)
		if err != nil {
			panic(err)
		}

		io.ReadFull(r, header[:])
		p.id, err = strconv.ParseInt(string(header[:]), 2, 32)
		if err != nil {
			panic(err)
		}

		switch p.id {
		case IDValue:
			v := LiteralValue(r)
			subpackets = append(subpackets, valuepacket{
				basepacket: p,
				value:      v,
			})
		default:
			var lengthType [1]byte
			_, err = io.ReadFull(r, lengthType[:])
			if err != nil {
				panic(err)
			}

			if lengthType[0] == '0' {
				bits := make([]byte, 15)
				_, err := io.ReadFull(r, bits)
				if errors.Is(err, io.EOF) {
					return subpackets
				}

				numberOfBits, err := strconv.ParseInt(string(bits), 2, 32)
				if err != nil {
					panic(err)
				}

				subpackets = append(subpackets, operatorpacket{
					basepacket: p,
					subpackets: readPacket(io.LimitReader(r, numberOfBits), math.MaxInt),
				})

			} else {
				bits := make([]byte, 11)
				_, err = io.ReadFull(r, bits)
				numberOfPackets, err := strconv.ParseInt(string(bits), 2, 32)
				if err != nil {
					panic(err)
				}

				subpackets = append(subpackets, operatorpacket{
					basepacket: p,
					subpackets: readPacket(r, int(numberOfPackets)),
				})
			}
		}
	}

	return subpackets
}

type transmission struct {
	data   []byte
	offset int

	reader io.Reader
}

func LiteralValue(r io.Reader) int64 {
	var section [5]byte

	_, err := io.ReadFull(r, section[:])
	if err != nil {
		panic(err)
	}

	data := make([]byte, 0)
	for {
		data = append(data, section[1:]...)
		if section[0] == '0' {
			break
		}

		_, err := io.ReadFull(r, section[:])
		if err != nil {
			panic(err)
		}
	}

	value, err := strconv.ParseInt(string(data), 2, 64)
	return value
}

type packet interface {
	Value() int64
	VersionSum() int64
}

type basepacket struct {
	version int64
	id      int64
}

type valuepacket struct {
	basepacket
	value int64
}

func (v valuepacket) Value() int64 {
	return v.value
}

func (v valuepacket) VersionSum() int64 {
	return v.version
}

type operatorpacket struct {
	basepacket
	subpackets []packet
}

func (o operatorpacket) Value() int64 {
	switch o.id {
	case 0:
		return o.sum()
	case 1:
		return o.product()
	case 2:
		return o.min()
	case 3:
		return o.max()
	case 5:
		return o.greater()
	case 6:
		return o.less()
	case 7:
		return o.equal()
	}

	panic("may never happen")
}

func (o operatorpacket) sum() int64 {
	sum := int64(0)
	for _, child := range o.subpackets {
		sum += child.Value()
	}

	return sum
}

func (o operatorpacket) product() int64 {
	product := int64(1)
	for _, child := range o.subpackets {
		product *= child.Value()
	}

	return product
}

func (o operatorpacket) min() int64 {
	min := int64(0)
	for _, child := range o.subpackets {
		v := child.Value()
		if min == 0 || v < min {
			min = v
		}
	}

	return min
}

func (o operatorpacket) max() int64 {
	max := int64(0)
	for _, child := range o.subpackets {
		v := child.Value()
		if max == 0 || v > max {
			max = v
		}
	}

	return max
}

func (o operatorpacket) greater() int64 {
	p1v := o.subpackets[0].Value()
	p2v := o.subpackets[1].Value()

	if p1v > p2v {
		return 1
	}

	return 0
}

func (o operatorpacket) less() int64 {
	p1v := o.subpackets[0].Value()
	p2v := o.subpackets[1].Value()

	if p1v < p2v {
		return 1
	}

	return 0
}

func (o operatorpacket) equal() int64 {
	p1v := o.subpackets[0].Value()
	p2v := o.subpackets[1].Value()

	if p1v == p2v {
		return 1
	}

	return 0
}

func (v operatorpacket) VersionSum() int64 {
	sum := v.version

	for _, child := range v.subpackets {
		sum += child.VersionSum()
	}

	return sum
}

func parseInput() transmission {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	instructions := make([]byte, 0, len(data)*4)

	for _, b := range data {
		i, err := strconv.ParseInt(string(b), 16, 64)
		if err != nil {
			log.Fatal(err)
		}

		instructions = append(instructions, []byte(fmt.Sprintf("%04b", i))...)
	}

	return transmission{data: instructions, reader: bytes.NewReader(instructions)}
}
