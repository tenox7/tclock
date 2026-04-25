package main

import (
	"flag"
	"image/color"
	"log"
	"time"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/examples/resources/fonts"
	"github.com/hajimehoshi/ebiten/v2/text"
	"golang.org/x/image/font"
	"golang.org/x/image/font/opentype"
)

const (
	screenWidth  = 640
	screenHeight = 480
)

var (
	fontSize       int
	backgroundColor string
	clockFont      font.Face
	bgColor        color.Color
)

type Game struct {
	currentTime string
	lastUpdate  time.Time
}

func (g *Game) Update() error {
	now := time.Now()
	if now.Sub(g.lastUpdate) >= time.Second {
		g.currentTime = now.Format("15:04:05")
		g.lastUpdate = now
	}
	return nil
}

func (g *Game) Draw(screen *ebiten.Image) {
	screen.Fill(bgColor)
	
	bounds := text.BoundString(clockFont, g.currentTime)
	x := (screenWidth - bounds.Dx()) / 2
	y := (screenHeight + bounds.Dy()) / 2

	text.Draw(screen, g.currentTime, clockFont, x, y, color.White)
}

func (g *Game) Layout(outsideWidth, outsideHeight int) (int, int) {
	return screenWidth, screenHeight
}

func init() {
	flag.IntVar(&fontSize, "font-size", 72, "Font size for the clock")
	flag.StringVar(&backgroundColor, "bg", "black", "Background color (black, white, red, green, blue)")
	flag.Parse()

	tt, err := opentype.Parse(fonts.MPlus1pRegular_ttf)
	if err != nil {
		log.Fatal(err)
	}

	clockFont, err = opentype.NewFace(tt, &opentype.FaceOptions{
		Size:    float64(fontSize),
		DPI:     72,
		Hinting: font.HintingFull,
	})
	if err != nil {
		log.Fatal(err)
	}

	switch backgroundColor {
	case "black":
		bgColor = color.Black
	case "white":
		bgColor = color.White
	case "red":
		bgColor = color.RGBA{255, 0, 0, 255}
	case "green":
		bgColor = color.RGBA{0, 255, 0, 255}
	case "blue":
		bgColor = color.RGBA{0, 0, 255, 255}
	default:
		log.Fatalf("Unknown background color: %s", backgroundColor)
	}
}

func main() {
	ebiten.SetWindowSize(screenWidth, screenHeight)
	ebiten.SetWindowTitle("Digital Clock")
	ebiten.SetMaxTPS(10)  // Limit updates to 10 times per second

	game := &Game{
		currentTime: time.Now().Format("15:04:05"),
		lastUpdate:  time.Now(),
	}

	if err := ebiten.RunGame(game); err != nil {
		log.Fatal(err)
	}
}
