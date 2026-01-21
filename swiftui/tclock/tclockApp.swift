import SwiftUI

struct ClockView: View {
    @State private var currentTime: String = ""
    private let timer = Timer.publish(every: 1, on: .main, in: .common).autoconnect()

    var body: some View {
        Text(currentTime)
            .font(.system(size: 48))
            .monospacedDigit()
            .fontWeight(.bold)
            .foregroundColor(.secondary)
            .background(.clear)
            .onReceive(timer) { _ in
                let formatter = DateFormatter()
                formatter.dateFormat = "HH:mm:ss"
                currentTime = formatter.string(from: Date())
            }
    }
}

class AppDelegate: NSObject, NSApplicationDelegate {
    @AppStorage("alwaysOnTop") var alwaysOnTop: Bool = false

    func applicationDidFinishLaunching(_ notification: Notification) {
        updateWindowLevel()
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        return true
    }

    func updateWindowLevel() {
        guard let window = NSApplication.shared.windows.first else { return }
        window.level = alwaysOnTop ? .floating : .normal
    }

    @objc func toggleAlwaysOnTop() {
        alwaysOnTop.toggle()
        updateWindowLevel()
    }
}

@main
struct ClockApp: App {
    @NSApplicationDelegateAdaptor(AppDelegate.self) var appDelegate

    var body: some Scene {
        WindowGroup {
            ClockView()
                .frame(width: 260, height: 60)
        }
        .windowResizability(.contentSize)
        .commands {
            CommandGroup(after: .windowArrangement) {
                Toggle("Always on Top", isOn: Binding(
                    get: { appDelegate.alwaysOnTop },
                    set: { newValue in
                        appDelegate.alwaysOnTop = newValue
                        appDelegate.updateWindowLevel()
                    }
                ))
                .keyboardShortcut("t", modifiers: [.command, .shift])
            }
        }
    }
}
