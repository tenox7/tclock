#include <windows.h>
#include <stdio.h>

HFONT hFont;
HWND hLabel;


LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
	char buffer[16];
	SYSTEMTIME st;
    switch (uMsg) {
        case WM_CREATE:
            hLabel = CreateWindow("STATIC", "", WS_CHILD | WS_VISIBLE | SS_CENTER,
                                  0, 0, 200, 50, hwnd, NULL, NULL, NULL);
            hFont = CreateFont(48, 0, 0, 0, FW_BOLD, FALSE, FALSE, FALSE,
                               ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                               CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                               DEFAULT_PITCH | FF_DONTCARE, "Arial");
            SendMessage(hLabel, WM_SETFONT, (WPARAM)hFont, TRUE);            
            SetTimer(hwnd, 1, 1000, NULL);
            break;
        case WM_TIMER:
			GetLocalTime(&st);
			sprintf(buffer, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond);
			SetWindowText(hLabel, buffer);
            break;
        case WM_DESTROY:
            KillTimer(hwnd, 1);
            DeleteObject(hFont);
            PostQuitMessage(0);
            break;
        default:
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    HWND hwnd;
    MSG msg;
    WNDCLASS wc;
    wc.style = 0;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = "ClockAppClass";

    if (!RegisterClass(&wc)) {
        MessageBox(NULL, "Window Registration Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    hwnd = CreateWindow("ClockAppClass", "TClock", WS_OVERLAPPEDWINDOW & ~WS_THICKFRAME,
                             CW_USEDEFAULT, CW_USEDEFAULT, 200, 70,
                             NULL, NULL, hInstance, NULL);
    if (hwnd == NULL) {
        MessageBox(NULL, "Window Creation Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    ShowWindow(hwnd, nCmdShow);
    UpdateWindow(hwnd);

    while (GetMessage(&msg, NULL, 0, 0)){
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (int)msg.wParam;
}
