// tenox clock in go
// todo:
// systray menu - maybe https://github.com/fyne-io/systray ?
// configuration params?

package main

import (
	"fmt"
	"image/color"
	"log"
	"os"
	"time"

	"github.com/flopp/go-findfont"
	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/ebitenutil"
	"github.com/hajimehoshi/ebiten/v2/text"
	"golang.org/x/image/font"
	"golang.org/x/image/font/opentype"
)

var (
	size     float64 = 32
	margin           = 10
	fontName         = "tahoma bold.ttf"
	fontFace font.Face
	debug    = false
	format   = "03:04:05 PM"
)

type app struct{}

func (g *app) Update() error {
	return nil
}

func (g *app) Draw(screen *ebiten.Image) {
	screen.Fill(color.Gray16{0x8000})

	text.Draw(screen, time.Now().Format(format), fontFace, margin, int(size)+margin, color.White)

	if ebiten.IsFocused() {
		ebiten.SetWindowDecorated(true)
	} else {
		ebiten.SetWindowDecorated(false)
	}

	if !debug {
		return
	}
	ebitenutil.DebugPrint(screen, fmt.Sprintf("TPS: %0.2f\nFPS: %0.2f\nTime: %v",
		ebiten.ActualTPS(),
		ebiten.ActualFPS(),
		time.Now()),
	)
}

func (g *app) Layout(outsideWidth, outsideHeight int) (int, int) {
	s := ebiten.DeviceScaleFactor()
	return int(float64(outsideWidth) * s), int(float64(outsideHeight) * s)
}

func ticker() {
	ticker := time.NewTicker(1 * time.Second)
	for {
		select {
		case <-ticker.C:
			ebiten.ScheduleFrame()
		}
	}
}

func main() {
	ff, err := findfont.Find(fontName)
	if err != nil {
		log.Fatal(err)
	}
	f, err := os.ReadFile(ff)
	if err != nil {
		log.Fatal(err)
	}
	tt, err := opentype.Parse(f)
	if err != nil {
		log.Fatal(err)
	}
	fontFace, err = opentype.NewFace(tt, &opentype.FaceOptions{
		Size:    size,
		DPI:     100, // TODO: set actual DPI from ebiten
		Hinting: font.HintingFull,
	})
	if err != nil {
		log.Fatal(err)
	}

	b := text.BoundString(fontFace, format)
	ebiten.SetWindowSize(b.Dx()+(margin*2), b.Dy()+(margin*2))
	ebiten.SetWindowDecorated(false)
	ebiten.DeviceScaleFactor()
	ebiten.SetWindowResizingMode(ebiten.WindowResizingModeDisabled)
	ebiten.SetWindowTitle("TClock")
	ebiten.SetTPS(ebiten.SyncWithFPS)
	ebiten.SetFPSMode(ebiten.FPSModeVsyncOffMinimum)
	go ticker()
	if err := ebiten.RunGame(&app{}); err != nil {
		log.Fatal(err)
	}
}
