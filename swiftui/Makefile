-include .env

APP_NAME = tclock
SCHEME = tclock
BUILD_DIR = build
APP = $(BUILD_DIR)/Build/Products/Release/$(APP_NAME).app
DMG_NAME = $(APP_NAME).dmg

.PHONY: all build clean dmg release

all: build

build:
	xcodebuild -scheme $(SCHEME) -configuration Release -derivedDataPath $(BUILD_DIR) build

clean:
	rm -rf $(BUILD_DIR) $(DMG_NAME)
	xcodebuild -scheme $(SCHEME) clean

dmg: build
	rm -f $(DMG_NAME)
	hdiutil create -volname $(APP_NAME) -srcfolder $(APP) -ov -format UDZO $(DMG_NAME)

release: build
	@test -n "$(DEV_ID)" || { echo "DEV_ID not set — copy .env.example to .env and fill in"; exit 1; }
	@test -n "$(NOTARY_PROFILE)" || { echo "NOTARY_PROFILE not set — copy .env.example to .env and fill in"; exit 1; }
	codesign --force --options runtime --timestamp --entitlements tclock/tclock.entitlements --sign "$(DEV_ID)" $(APP)
	rm -f $(DMG_NAME)
	hdiutil create -volname $(APP_NAME) -srcfolder $(APP) -ov -format UDZO $(DMG_NAME)
	codesign --force --timestamp --sign "$(DEV_ID)" $(DMG_NAME)
	xcrun notarytool submit $(DMG_NAME) --keychain-profile "$(NOTARY_PROFILE)" --wait
	xcrun stapler staple $(DMG_NAME)
	@echo "Signed + notarized: $(DMG_NAME)"
