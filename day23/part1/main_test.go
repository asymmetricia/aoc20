package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMove(t *testing.T) {
	require.ElementsMatch(t,
		Move([]int{3, 8, 9, 1, 2, 5, 4, 6, 7}),
		[]int{2, 8, 9, 1, 5, 4, 6, 7, 3},
	)
	require.ElementsMatch(t,
		Move([]int{1, 8, 7, 4, 6, 3, 5, 2, 9}),
		[]int{6, 3, 5, 2, 9, 8, 7, 4, 1})
}
