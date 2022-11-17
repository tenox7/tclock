release:
	GOOS=windows GOARCH=amd64 go build -o tclock_windows_amd64.exe .
	GOOS=windows GOARCH=arm64 go build -o tclock_windows_arm64.exe .
	CGO_ENABLED=1 GOOS=darwin GOARCH=amd64 go build -o tclock_macos_amd64 .
	CGO_ENABLED=1 GOOS=darwin GOARCH=arm64 go build -o tclock_macos_arm64 .
	CGO_ENABLED=1 GOOS=linux GOARCH=amd64 go build -o tclock_linux_amd64 .
	CGO_ENABLED=1 GOOS=linux GOARCH=arm64 go build -o tclock_linux_arm64 .

clean:
	rm -f tclock_