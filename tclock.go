// tenox clock in go
// todo:
// systray menu - maybe https://github.com/fyne-io/systray ?
// configuration params?

package main

import (
	"flag"
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
	fontSize = flag.Float64("font_size", 32, "font size")
	fontName = flag.String("font_name", "tahoma bold.ttf", "font name, eg arial.ttf")
	margin   = flag.Int("margin", 10, "window margin")
	debug    = flag.Bool("debug", false, "debug output")
	format   = flag.String("time_format", "03:04:05 PM", "https://pkg.go.dev/time#pkg-constants")
	bgColor  = flag.String("bg", "#808080", "background color")
	fgColor  = flag.String("fg", "#ffea40", "foreground color")
	bgRgba   color.RGBA
	fgRgba   color.RGBA
	fontFace font.Face
)

func hexToRgba(s string) color.RGBA {
	var c color.RGBA
	c.A = 0xff

	if s[0] != '#' {
		return c
	}

	hexToByte := func(b byte) byte {
		switch {
		case b >= '0' && b <= '9':
			return b - '0'
		case b >= 'a' && b <= 'f':
			return b - 'a' + 10
		case b >= 'A' && b <= 'F':
			return b - 'A' + 10
		}
		log.Fatal("invalid color format")
		return 0
	}

	switch len(s) {
	case 7:
		c.R = hexToByte(s[1])<<4 + hexToByte(s[2])
		c.G = hexToByte(s[3])<<4 + hexToByte(s[4])
		c.B = hexToByte(s[5])<<4 + hexToByte(s[6])
	case 4:
		c.R = hexToByte(s[1]) * 17
		c.G = hexToByte(s[2]) * 17
		c.B = hexToByte(s[3]) * 17
	default:
		log.Fatal("invalid color format")
	}
	return c
}

type app struct{}

func (g *app) Update() error {
	return nil
}

func (g *app) Draw(screen *ebiten.Image) {
	screen.Fill(bgRgba)

	text.Draw(screen, time.Now().Format(*format), fontFace, *margin, int(*fontSize)+*margin, fgRgba)

	if ebiten.IsFocused() {
		ebiten.SetWindowDecorated(true)
	} else {
		ebiten.SetWindowDecorated(false)
	}

	if !*debug {
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
	flag.Parse()
	bgRgba = hexToRgba(*bgColor)
	fgRgba = hexToRgba(*fgColor)
	ff, err := findfont.Find(*fontName)
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
		Size:    *fontSize,
		DPI:     100, // TODO: set actual DPI from ebiten
		Hinting: font.HintingFull,
	})
	if err != nil {
		log.Fatal(err)
	}

	b := text.BoundString(fontFace, *format)
	ebiten.SetWindowSize(b.Dx()+(*margin*2), b.Dy()+(*margin*2))
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
