// NO RUN
import "windows"

const wnd_proc = fn (hwnd: HWND, msg: UINT, wp: WPARAM, lp: LPARAM): LRESULT => {
    match msg {
        WM_DESTROY => {
            PostQuitMessage(0)
        }
    }
    return DefWindowProcA(hwnd, msg, wp, lp)
}

const main = fn () => {
    var class_name ="my_class"

    var class: WNDCLASSEXA
    class.cbSize = 80
    class.lpfnWndProc = wnd_proc as *None
    class.hInstance = GetModuleHandleA(none)
    class.lpszClassName = class_name
    RegisterClassExA(&class as *let WNDCLASSEXA)
    CreateWindowExA(0, class_name, "My Window", WS_OVERLAPPEDWINDOW | WS_VISIBLE, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, none, none, class.hInstance, none)

    var msg: MSG
    while true {
        while PeekMessageA(&msg, none, 0, 0, PM_REMOVE) {
            TranslateMessage(&msg)
            DispatchMessageA(&msg)

            println(msg.message as S64)
            if msg.message == WM_QUIT
                return 0
        }
    }
    return 0
}

