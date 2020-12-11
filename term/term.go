package term

import "fmt"

func Clear() {
	print("\x1b[2J")
}

func MoveCursor(x, y int) {
	print("\x1b[", y, ";", x, "H")
}

func HideCursor() {
	print("\x1b[?25l")
}

func ShowCursor() {
	print("\x1b[?25h")
}

func Color(r, g, b int) {
	print(Scolor(r, g, b))
}
func Scolor(r, g, b int) string {
	return fmt.Sprint("\x1b[38;2;", r, ";", g, ";", b, "m")
}

func ColorReset() {
	print(ScolorReset())
}

func ScolorReset() string {
	return "\x1b[0m"
}
