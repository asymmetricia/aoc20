package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestCharacter_Match(t *testing.T) {
	r := Character('a')
	opts := r.Match("a")
	require.ElementsMatch(t, opts, []int{1})

	require.Nil(t, r.Match("b"))
}

func TestList_Match(t *testing.T) {
	l := List{Character('a'), Character('b')}
	require.ElementsMatch(t, l.Match("ab"), []int{2})
	require.Nil(t, l.Match("aa"))
	//	m, c := l.Match("ab")
	//	require.True(t, m)
	//	require.Equal(t, 2, c)
	//
	//	m, c = l.Match("ba")
	//	require.False(t, m)
	//	require.Equal(t, 0, c)
}

func TestChoice_Match(t *testing.T) {
	type test struct {
		Choice
		string
		opts []int
	}
	for _, tt := range []test{
		{Choice{Character('a'), Character('b')}, "a", []int{1}},
		{Choice{Character('a'), Character('b')}, "b", []int{1}},
		{Choice{Character('a'), Character('b')}, "c", nil},
		{Choice{List{Character('a'), Character('b')}, Character('c')}, "a", nil},
		{Choice{List{Character('a'), Character('b')}, Character('c')}, "ab", []int{2}},
		{Choice{List{Character('a'), Character('b')}, Character('c')}, "c", []int{1}},
		{Choice{List{Character('a'), Character('a')}, Character('a')}, "aa", []int{1, 2}},
	} {
		require.ElementsMatch(t, tt.Choice.Match(tt.string), tt.opts)
	}
}

func TestChoice_MatchLoop(t *testing.T) {
	ch := &Choice{}
	ch[0] = List{Character('a'), ch}
	ch[1] = Character('a')
	//fmt.Printf("%p %+v", ch, ch)
	//root := List{ch, Character('a')}

	require.ElementsMatch(t, ch.Match("a"), []int{1})
	require.ElementsMatch(t, ch.Match("aa"), []int{1,2})
	require.ElementsMatch(t, ch.Match("aaa"), []int{1,2,3})
	//for _, match := range []string{"aa", "aaa", "aaaa"} {
	//	t.Run(match, func(t *testing.T) {
	//		require.NotNil(t, root.Match(match))
	//	})
	//}
}
