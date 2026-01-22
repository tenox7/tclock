import SwiftUI

struct ClockView: View {
    @ObservedObject private var settings = AppSettings.shared
    @State private var currentTime: String = {
        let formatter = DateFormatter()
        formatter.dateFormat = "HH:mm:ss"
        return formatter.string(from: Date())
    }()
    private let timer = Timer.publish(every: 1, on: .main, in: .common).autoconnect()

    var body: some View {
        Text(currentTime)
            .font(.system(size: 48))
            .monospacedDigit()
            .fontWeight(.bold)
            .foregroundColor(settings.fontColor)
            .onReceive(timer) { _ in
                let formatter = DateFormatter()
                formatter.dateFormat = "HH:mm:ss"
                currentTime = formatter.string(from: Date())
            }
    }
}

class AppDelegate: NSObject, NSApplicationDelegate {
    func applicationDidFinishLaunching(_ notification: Notification) {
        AppSettings.shared.updateWindowLevel()
        AppSettings.shared.updateWindowStyle()
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        return true
    }
}

@main
struct ClockApp: App {
    @NSApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @ObservedObject private var settings = AppSettings.shared

    var body: some Scene {
        WindowGroup {
            ClockView()
                .frame(width: 260, height: 60)
                .background(settings.borderless ? Color.clear : Color(nsColor: .windowBackgroundColor))
        }
        .windowResizability(.contentSize)
        .commands {
            CommandGroup(after: .windowArrangement) {
                Toggle("Always on Top", isOn: $settings.alwaysOnTop)
                    .keyboardShortcut("t", modifiers: [.command, .shift])
                Toggle("Borderless", isOn: $settings.borderless)
                    .keyboardShortcut("b", modifiers: [.command, .shift])
                Divider()
                Button("Font Color...") {
                    AppSettings.shared.showColorPanel()
                }
                .keyboardShortcut("c", modifiers: [.command, .shift])
            }
        }
    }
}

class AppSettings: NSObject, ObservableObject {
    static let shared = AppSettings()

    @Published var alwaysOnTop: Bool = UserDefaults.standard.bool(forKey: "alwaysOnTop") {
        didSet {
            UserDefaults.standard.set(alwaysOnTop, forKey: "alwaysOnTop")
            updateWindowLevel()
        }
    }

    @Published var borderless: Bool = UserDefaults.standard.bool(forKey: "borderless") {
        didSet {
            UserDefaults.standard.set(borderless, forKey: "borderless")
            updateWindowStyle()
        }
    }

    @Published var fontColor: Color = {
        guard let data = UserDefaults.standard.data(forKey: "fontColor"),
              let nsColor = try? NSKeyedUnarchiver.unarchivedObject(ofClass: NSColor.self, from: data)
        else { return Color.secondary }
        return Color(nsColor: nsColor)
    }() {
        didSet {
            guard let data = try? NSKeyedArchiver.archivedData(withRootObject: NSColor(fontColor), requiringSecureCoding: false)
            else { return }
            UserDefaults.standard.set(data, forKey: "fontColor")
        }
    }

    func showColorPanel() {
        let panel = NSColorPanel.shared
        panel.color = NSColor(fontColor)
        panel.setTarget(self)
        panel.setAction(#selector(colorChanged(_:)))
        panel.makeKeyAndOrderFront(nil)
        panel.isContinuous = true
    }

    @objc func colorChanged(_ sender: NSColorPanel) {
        fontColor = Color(nsColor: sender.color)
    }

    func updateWindowLevel() {
        guard let window = NSApplication.shared.windows.first else { return }
        window.level = alwaysOnTop ? .floating : .normal
    }

    func updateWindowStyle() {
        guard let window = NSApplication.shared.windows.first else { return }
        if borderless {
            window.styleMask = [.borderless]
            window.isMovableByWindowBackground = true
            window.backgroundColor = .clear
            window.isOpaque = false
            window.hasShadow = false
            window.contentView?.wantsLayer = true
            window.contentView?.layer?.backgroundColor = .clear
        } else {
            window.styleMask = [.titled, .closable, .miniaturizable]
            window.isMovableByWindowBackground = false
            window.backgroundColor = .windowBackgroundColor
            window.isOpaque = true
            window.hasShadow = true
        }
    }
}
