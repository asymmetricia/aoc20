package main

import (
	"strings"

	"github.com/asymmetricia/aoc20/aoc"
)

type Passport struct {
	Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid *string
}

func (p Passport) Valid() bool {
	return p.Byr != nil && p.Iyr != nil && p.Eyr != nil && p.Hgt != nil && p.Hcl != nil && p.Ecl != nil && p.Pid != nil
}

func main() {
	data := aoc.Input(2020, 4)

	var pports []Passport
	var port Passport
	for _, line := range strings.Split(string(data), "\n") {
		if strings.TrimSpace(line) == "" {
			pports = append(pports, port)
			port = Passport{}
			continue
		}
		kvs := strings.Split(line, " ")
		for _, kv := range kvs {
			kvparts := strings.Split(kv, ":")
			if len(kvparts) != 2 {
				panic(kv)
			}
			key, value := kvparts[0], kvparts[1]
			var dst **string
			switch key {
			case "byr":
				dst = &port.Byr
			case "iyr":
				dst = &port.Iyr
			case "eyr":
				dst = &port.Eyr
			case "hgt":
				dst = &port.Hgt
			case "hcl":
				dst = &port.Hcl
			case "ecl":
				dst = &port.Ecl
			case "pid":
				dst = &port.Pid
			case "cid":
				dst = &port.Cid
			default:
				panic(key)
			}
			*dst = new(string)
			**dst = value
		}
	}

	count := 0
	for _, port := range pports {
		if port.Valid() {
			count++
		}
	}
	println(count)
}
