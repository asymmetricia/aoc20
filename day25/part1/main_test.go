package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestUntransform(t *testing.T) {
	require.Equal(t, 5764801, transform(7, 8))
	require.Equal(t, 8, untransform(7, 5764801))
}
