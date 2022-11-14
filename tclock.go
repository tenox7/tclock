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

	"fyne.io/systray"
)

var (
	size     float64 = 32
	margin           = 10
	fontName         = "tahoma bold.ttf"
	fontFace font.Face
	debug    = false
	format   = "03:04:05 PM"
)

type tclock struct{}

func (g *tclock) Update() error {
	return nil
}

func (g *tclock) Draw(screen *ebiten.Image) {
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

func (g *tclock) Layout(outsideWidth, outsideHeight int) (int, int) {
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

func menu() {
	systray.SetTitle("TClock")
	ampm := systray.AddMenuItemCheckbox("AM/PM Clock", "12 hour AM/PM format", true)
	secs := systray.AddMenuItemCheckbox("Seconds", "", true)
	atop := systray.AddMenuItemCheckbox("Always On Top", "", false)
	titl := systray.AddMenuItemCheckbox("Title Bar", "", false)

	quit := systray.AddMenuItem("Quit", "Exit the app...")
	quit.Enable()

	for {
		select {
		case <-ampm.ClickedCh:
		case <-secs.ClickedCh:
		case <-atop.ClickedCh:
		case <-titl.ClickedCh:
			if titl.Checked() {
				titl.Uncheck()
				ebiten.SetWindowFloating(false)
			} else {
				titl.Check()
				ebiten.SetWindowFloating(true)
			}
		case <-quit.ClickedCh:
			systray.Quit()
			os.Exit(0)
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
	//ebiten.SetWindowFloating(false)
	ebiten.DeviceScaleFactor()
	ebiten.SetWindowResizingMode(ebiten.WindowResizingModeDisabled)
	ebiten.SetWindowTitle("TClock")
	ebiten.SetTPS(ebiten.SyncWithFPS)

	go systray.Run(menu, nil)
	go ticker()
	if err := ebiten.RunGame(&tclock{}); err != nil {
		log.Fatal(err)
	}
}
