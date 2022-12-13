package aoc

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
)

func Input(year int, day int) []byte {
	cacheFile := fmt.Sprintf(".input.%d.%d", year, day)
	cache, err := os.ReadFile(cacheFile)
	if err == nil {
		return cache
	}

	session, err := os.ReadFile("aoc.session")
	if err != nil {
		panic(err)
	}
	session = bytes.TrimSpace(session)

	req, err := http.NewRequest(
		"GET",
		fmt.Sprintf("https://adventofcode.com/%d/day/%d/input", year, day),
		nil)
	if err != nil {
		panic(err)
	}
	req.AddCookie(&http.Cookie{
		Name:  "session",
		Value: string(session),
	})

	req.Header.Set("user-agent", "tricia-adventofcode@cernu.us")

	res, err := http.DefaultClient.Do(req)
	if err != nil {
		panic(err)
	}
	if res.StatusCode != 200 {
		panic(res.Status)
	}

	input, err := io.ReadAll(res.Body)
	if err != nil {
		panic(err)
	}

	if err := os.WriteFile(cacheFile, input, 0644); err != nil {
		panic(err)
	}

	return input
}
