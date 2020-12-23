package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMove(t *testing.T) {
	game := &Game{}
	for _, i := range []int{3, 8, 9, 1, 2, 5, 4, 6, 7} {
		game.AddCup(i)
	}

	game.Move()
	require.ElementsMatch(t, game.Cups(),
		[]int{2, 8, 9, 1, 5, 4, 6, 7, 3},
	)

	game = &Game{}
	for _, i := range []int{1, 8, 7, 4, 6, 3, 5, 2, 9} {
		game.AddCup(i)
	}
	require.ElementsMatch(t,
		game.Cups(),
		[]int{6, 3, 5, 2, 9, 8, 7, 4, 1})
}
