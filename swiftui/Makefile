APP_NAME = tclock
SCHEME = tclock
BUILD_DIR = build
DMG_NAME = $(APP_NAME).dmg

.PHONY: all build clean dmg

all: build

build:
	xcodebuild -scheme $(SCHEME) -configuration Release -derivedDataPath $(BUILD_DIR) build

clean:
	rm -rf $(BUILD_DIR) $(DMG_NAME)
	xcodebuild -scheme $(SCHEME) clean

dmg: build
	rm -f $(DMG_NAME)
	hdiutil create -volname $(APP_NAME) -srcfolder $(BUILD_DIR)/Build/Products/Release/$(APP_NAME).app -ov -format UDZO $(DMG_NAME)
