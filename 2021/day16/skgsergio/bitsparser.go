package main

import (
	"fmt"
)

const (
	tSum packetType = iota
	tProd
	tMin
	tMax
	tLit
	tGt
	tLt
	tEq
)

type packetType int

func (pt packetType) String() string {
	str := "Unknown"

	switch pt {
	case tSum:
		str = "Sum"
	case tProd:
		str = "Product"
	case tMin:
		str = "Minimum"
	case tMax:
		str = "Maximum"
	case tGt:
		str = "Greater Than"
	case tLt:
		str = "Less Than"
	case tEq:
		str = "Equal To"
	}

	return str
}

// Packet interface

type Packet interface {
	Version() int
	Type() string
	VersionSum() int
	Eval() int
}

// Header Packet

type Header struct {
	version int
	typeID  packetType
}

func (hdr Header) Version() int {
	return hdr.version
}

func (hdr Header) Type() string {
	return hdr.typeID.String()
}

// Literal Packet

type Literal struct {
	Header
	value int
}

func (lit Literal) VersionSum() int {
	return lit.version
}

func (lit Literal) Eval() int {
	return lit.value
}

// Operator Packet

type Operator struct {
	Header
	packets []Packet
}

func (op Operator) VersionSum() int {
	sum := op.version

	for _, pkt := range op.packets {
		sum += pkt.VersionSum()
	}

	return sum
}

func (op Operator) Eval() int {
	result := 0

	switch op.typeID {
	case tSum:
		for _, pkt := range op.packets {
			result += pkt.Eval()
		}

	case tProd:
		result = 1
		for _, pkt := range op.packets {
			result *= pkt.Eval()
		}

	case tMin:
		for i, pkt := range op.packets {
			if i == 0 {
				result = op.packets[0].Eval()
				continue
			}

			value := pkt.Eval()
			if value < result {
				result = value
			}
		}

	case tMax:
		for _, pkt := range op.packets {
			value := pkt.Eval()

			if value > result {
				result = value
			}
		}

	case tGt:
		if op.packets[0].Eval() > op.packets[1].Eval() {
			result = 1
		}

	case tLt:
		if op.packets[0].Eval() < op.packets[1].Eval() {
			result = 1
		}

	case tEq:
		if op.packets[0].Eval() == op.packets[1].Eval() {
			result = 1
		}

	default:
		panic(fmt.Errorf("can't evaluate packet with type %d (%s)", op.typeID, op.typeID))
	}

	return result
}

type BITSParser struct {
	bytes      []byte
	byteOffset int
	bitOffset  int
}

func NewBITSParser(bytes []byte) *BITSParser {
	return &BITSParser{bytes, 0, 0}
}

func (bp *BITSParser) Parse() (Packet, error) {
	pkt, _, err := bp.parsePacket()
	return pkt, err
}

func (bp *BITSParser) readBits(nbits int) (int, error) {
	result := 0

	for i := 0; i < nbits; i++ {
		if bp.byteOffset >= len(bp.bytes) {
			return result, fmt.Errorf("read error: bytes lenght = %d; byte offset = %d; bit offset = %d", len(bp.bytes), bp.byteOffset, bp.bitOffset)
		}

		// Read a new bit
		result <<= 1
		result |= int((bp.bytes[bp.byteOffset] >> (7 - bp.bitOffset)) & 0b1)

		// Each 8 bits we've read a byte, reset bitOffset and increase byteOffset.
		bp.bitOffset++

		if bp.bitOffset == 8 {
			bp.bitOffset = 0
			bp.byteOffset++
		}
	}

	return result, nil
}

func (bp *BITSParser) parsePacket() (Packet, int, error) {
	version, err := bp.readBits(3)
	if err != nil {
		return nil, 0, err
	}

	typeID, err := bp.readBits(3)
	if err != nil {
		return nil, 3, err
	}

	hdr := Header{version, packetType(typeID)}

	var packet Packet = nil
	packetBytes := 0

	if hdr.typeID == tLit {
		packet, packetBytes, err = bp.parseLiteral(hdr)
	} else {
		packet, packetBytes, err = bp.parseOperator(hdr)
	}

	return packet, 6 + packetBytes, err
}

func (bp *BITSParser) parseLiteral(hdr Header) (*Literal, int, error) {
	totalRead := 0

	lit := &Literal{
		Header: hdr,
		value:  0,
	}

	lastGroup := false
	for !lastGroup {
		// Read group prefix: 1 == more data available, 0 == last group
		prefix, err := bp.readBits(1)
		if err != nil {
			return nil, totalRead, err
		}
		lastGroup = prefix == 0b0

		// Read 4 bit group
		group, err := bp.readBits(4)
		if err != nil {
			return nil, totalRead, err
		}
		totalRead += 5

		// Shift value 4 bits and add current group.
		lit.value <<= 4
		lit.value |= group
	}

	return lit, totalRead, nil
}

func (bp *BITSParser) parseOperator(hdr Header) (*Operator, int, error) {
	totalRead := 0

	op := &Operator{
		Header:  hdr,
		packets: nil,
	}

	lengthTypeID, err := bp.readBits(1)
	if err != nil {
		return nil, totalRead, err
	}
	totalRead += 1

	// If the lengthTypeID is 0 the next 15 lengthBits is packets total length to read
	// if is 1 the next 11 lengthBits is the number of packets to read.
	lengthBits := 15
	if lengthTypeID == 0b1 {
		lengthBits = 11
	}

	pktsLength, err := bp.readBits(lengthBits)
	if err != nil {
		return nil, totalRead, err
	}
	totalRead += lengthBits

	// Read packets until the condition is met
	for pktCount := 0; (lengthTypeID == 0b0 && totalRead-lengthBits-1 < pktsLength) ||
		(lengthTypeID == 0b1 && pktCount < pktsLength); pktCount++ {
		pkt, readBytes, err := bp.parsePacket()
		if err != nil {
			return nil, totalRead, err
		}
		totalRead += readBytes

		op.packets = append(op.packets, pkt)
	}

	return op, totalRead, nil
}
