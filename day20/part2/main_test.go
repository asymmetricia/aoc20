package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestTile_Bounds(t *testing.T) {
	require.Equal(t,
		Coord{1, 1},
		Tile{Cells: map[Coord]bool{Coord{0, 0}: true}}.Bounds(),
	)
	require.Equal(t,
		Coord{2, 2},
		Tile{Cells: map[Coord]bool{Coord{1, 1}: true}}.Bounds(),
	)
}

//   0 1 2 3 4 5 6 7 8 9
// 0 . . . . . . . . . .
// 1 . . 1 . . . . . . .
// 2 . . . . . . . . 2 .
// 3 . . . . . . . . . .
// 4 . . . . . . . . . .
// 5 . . . . . . . . . .
// 6 . . . . . . . . . .
// 7 . 4 . . . . . . . .
// 8 . . . . . . . 3 . .
// 9 . . . . . . . . . .
func TestTile_Rotate(t *testing.T) {
	tile := &Tile{
		Cells: map[Coord]bool{
			Coord{2, 1}: true,
			Coord{0, 0}: false,
			Coord{9, 0}: false,
			Coord{0, 9}: false,
			Coord{9, 9}: false,
		},
	}

	tile.Rotate()
	require.Equal(t, map[Coord]bool{
		Coord{8, 2}: true,
		Coord{0, 0}: false,
		Coord{9, 0}: false,
		Coord{0, 9}: false,
		Coord{9, 9}: false,
	}, tile.Cells)
	tile.Rotate()
	require.Equal(t, map[Coord]bool{
		Coord{7, 8}: true,
		Coord{0, 0}: false,
		Coord{9, 0}: false,
		Coord{0, 9}: false,
		Coord{9, 9}: false,
	}, tile.Cells)
	tile.Rotate()
	require.Equal(t, map[Coord]bool{
		Coord{1, 7}: true,
		Coord{0, 0}: false,
		Coord{9, 0}: false,
		Coord{0, 9}: false,
		Coord{9, 9}: false,
	}, tile.Cells)
	tile.Rotate()
	require.Equal(t, map[Coord]bool{
		Coord{2, 1}: true,
		Coord{0, 0}: false,
		Coord{9, 0}: false,
		Coord{0, 9}: false,
		Coord{9, 9}: false,
	}, tile.Cells)
}

func TestTile_Match(t *testing.T) {
	a := &Tile{
		Cells: map[Coord]bool{
			Coord{9, 0}: true,
			Coord{9, 1}: true,
			Coord{9, 2}: true,
		},
	}
	b := &Tile{
		Cells: map[Coord]bool{
			Coord{0, 0}: true,
			Coord{0, 1}: true,
			Coord{0, 2}: true,
		},
	}
	require.True(t, a.Match(b, East))
}
