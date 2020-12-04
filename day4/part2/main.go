package main

import (
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
	"strings"
)

type Passport struct {
	Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid *string
}

func intValid(s *string, min, max int) bool {
	if s == nil {
		return false
	}
	v, err := strconv.Atoi(*s)
	if err != nil {
		return false
	}
	return v >= min && v <= max
}

func (p Passport) Valid() bool {
	if !intValid(p.Byr, 1920, 2002) {
		return false
	}
	if !intValid(p.Iyr, 2010, 2020) {
		return false
	}
	if !intValid(p.Eyr, 2020, 2030) {
		return false
	}

	if p.Hgt == nil {
		return false
	}
	hgt := *p.Hgt
	unit := hgt[len(hgt)-2:]
	hgtNum := hgt[:len(hgt)-2]
	hgtInt, err := strconv.Atoi(hgtNum)
	if err != nil {
		log.Printf("%v: %v", hgtNum, err)
		return false
	}

	if unit == "cm" && (hgtInt < 150 || hgtInt > 193) {
		return false
	}
	if unit == "in" && (hgtInt < 59 || hgtInt > 76) {
		return false
	}
	if unit != "cm" && unit != "in" {
		return false
	}

	colRe := regexp.MustCompile(`^#[0-9a-fA-F]{6}$`)
	if p.Hcl == nil {
		return false
	}
	if !colRe.MatchString(*p.Hcl) {
		return false
	}

	if p.Ecl == nil {
		return false
	}
	if !map[string]bool{"amb": true, "blu": true, "brn": true, "gry": true, "grn": true, "hzl": true, "oth": true}[*p.Ecl] {
		return false
	}

	pidRe := regexp.MustCompile(`^[0-9]{9}$`)
	if p.Pid == nil {
		return false
	}
	if !pidRe.MatchString(*p.Pid) {
		return false
	}

	return true
}

func main() {
	data, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}

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
