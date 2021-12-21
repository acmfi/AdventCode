package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"math"
	"os"
	"strconv"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

var hexToBinary = map[string]string{
	"0": "0000",
	"1": "0001",
	"2": "0010",
	"3": "0011",
	"4": "0100",
	"5": "0101",
	"6": "0110",
	"7": "0111",
	"8": "1000",
	"9": "1001",
	"A": "1010",
	"B": "1011",
	"C": "1100",
	"D": "1101",
	"E": "1110",
	"F": "1111",
}

var (
	padding = true
	input   = flag.String("f", "input", "Puzzle input file")
)

type packetReader struct {
	*bytes.Reader
}

type packet struct {
	version      int
	typeID       int
	lengthTypeID int
	value        int
	subpackets   []packet
}

func (p packet) sumVersion() int {
	versions := p.version
	for _, packet := range p.subpackets {
		versions += packet.sumVersion()
	}
	return versions
}

func (p packet) evaluate() int {
	result := 0
	if p.typeID == 4 {
		result = p.value
	}
	if p.typeID == 0 {
		for _, packet := range p.subpackets {
			result += packet.evaluate()
		}
	}
	if p.typeID == 1 {
		result = 1
		for _, packet := range p.subpackets {
			result *= packet.evaluate()
		}
	}
	if p.typeID == 2 {
		result = math.MaxInt
		for _, packet := range p.subpackets {
			value := packet.evaluate()
			if value < result {
				result = value
			}
		}
	}
	if p.typeID == 3 {
		result = 0
		for _, packet := range p.subpackets {
			value := packet.evaluate()
			if value > result {
				result = value
			}
		}
	}
	if p.typeID == 5 {
		if p.subpackets[0].evaluate() > p.subpackets[1].evaluate() {
			result = 1
		}
		result = 0
	}
	if p.typeID == 6 {
		if p.subpackets[0].evaluate() < p.subpackets[1].evaluate() {
			result = 1
		}
		result = 0
	}
	if p.typeID == 7 {
		if p.subpackets[0].evaluate() == p.subpackets[1].evaluate() {
			result = 1
		}
		result = 0
	}
	return result
}

func binary2Decimal(binary string) int {
	decimal, err := strconv.ParseInt(binary, 2, 64)
	check(err)
	return int(decimal)
}

func increaseBitPointer(bit *int, increase int) {
	*bit += increase
}

func packetVersion(transmission string, bit *int) int {
	binaryVersion := transmission[*bit : *bit+3]
	version := binary2Decimal(binaryVersion)
	increaseBitPointer(bit, 3)
	return int(version)
}

func packetTypeID(transmission string, bit *int) int {
	binaryTypeID := transmission[*bit : *bit+3]
	typeID := binary2Decimal(binaryTypeID)
	increaseBitPointer(bit, 3)
	return int(typeID)
}

func packetLiteral(transmission string, bit *int) int {
	binaryLiteralValue := ""
	for {
		fmt.Println("\tTransmission left:", transmission[*bit:])

		nextBits := transmission[*bit : *bit+5]
		fmt.Println("\tNext bits:", nextBits)
		// Advance five bits
		increaseBitPointer(bit, 5)
		// Last group, end of packet
		if string(nextBits[0]) == "0" {
			binaryLiteralValue += nextBits[1:5]
			fmt.Println("New literal:", binaryLiteralValue)
			break
		} else {
			binaryLiteralValue += nextBits[1:5]
			fmt.Println("New literal:", binaryLiteralValue)
		}
	}
	return binary2Decimal(binaryLiteralValue)
}

func packetLengthTypeID(transmission string, bit *int) int {
	binaryLengthTypeID := string(transmission[*bit])
	increaseBitPointer(bit, 1)
	return binary2Decimal(binaryLengthTypeID)
}

func subPacketsLength(transmission string, bit *int) {
	increaseBitPointer(bit, 15)
}

func subPacketsNumber(transmission string, bit *int) int {
	binarySubpacketsNumber := transmission[*bit : *bit+11]
	increaseBitPointer(bit, 11)
	return binary2Decimal(binarySubpacketsNumber)
}

func checkPadding(transmission string, bit *int) bool {
	for _, char := range transmission[*bit:] {
		if string(char) != "0" {
			padding = false
			break
		}
		padding = true
	}
	return padding
}

func newPacket(transmission string, bit *int) packet {
	fmt.Println("Transmission left:", transmission[*bit:])

	// New packet
	p := packet{}

	// Check padding
	padding = checkPadding(transmission, bit)
	if padding {
		*bit += len(transmission[*bit:])
		return p
	}

	// First three bits are the packet version
	p.version = packetVersion(transmission, bit)

	// Second three bits are the packet type ID
	p.typeID = packetTypeID(transmission, bit)

	// Literal value
	if p.typeID == 4 {
		p.value = packetLiteral(transmission, bit)
		return p
	}

	// Operator packet
	p.lengthTypeID = packetLengthTypeID(transmission, bit)

	/* If the length type ID is 0, then the next 15 bits are a number that
	represents the total length in bits of the sub-packets contained by this packet. */
	if p.lengthTypeID == 0 {
		subPacketsLength(transmission, bit)
		for len(transmission)-*bit >= 8 {
			// Check padding
			padding = checkPadding(transmission, bit)
			if padding {
				*bit += len(transmission[*bit:])
				return p
			}
			// An operator packet contains one or more packets.
			newPacket := newPacket(transmission, bit)
			p.subpackets = append(p.subpackets, newPacket)
		}

	}

	/* If the length type ID is 1, then the next 11 bits are a number
	that represents the number of sub-packets immediately contained by this packet. */
	if p.lengthTypeID == 1 {
		subpacketsNumber := subPacketsNumber(transmission, bit)
		for i := 0; i < subpacketsNumber; i++ {
			// Check padding
			padding = checkPadding(transmission, bit)
			if padding {
				*bit += len(transmission[*bit:])
				return p
			}
			// An operator packet contains one or more packets.
			newPacket := newPacket(transmission, bit)
			p.subpackets = append(p.subpackets, newPacket)
		}
	}
	return p
}

func main() {
	flag.Parse()

	// Input transmission
	transmission := parseInput(*input)
	// Pointer
	bit := 0
	// Find packet
	packet := newPacket(transmission, &bit)
	fmt.Printf("Star 1: %d\n", packet.sumVersion())
	fmt.Printf("Star 1: %d\n", packet.evaluate())
}

func parseInput(inputPath string) string {
	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Println("Can't open the input file for reading")
		os.Exit(1)
	}
	defer func() {
		err := file.Close()
		if err != nil {
			fmt.Println("Can't close file")
			os.Exit(1)
		}
	}()

	transmission := ""
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		for _, hexChar := range scanner.Text() {
			if binaryValue, ok := hexToBinary[string(hexChar)]; ok {
				transmission += binaryValue
			}
		}
	}
	return transmission
}
