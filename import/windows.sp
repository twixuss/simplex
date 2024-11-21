
import "c_types"
const BOOL = Bool
const LONG = S32
const UINT = U32
const DWORD = U32

const WPARAM = U64
const LPARAM = S64
const LRESULT = S64

const HWND = *None
const LPCSTR = *U8
const HMENU = *None
const HMODULE = *None
const HINSTANCE = *None
const HCURSOR = *None
const HICON = *None
const HBRUSH = *None
const LPVOID = *None
const WNDPROC = *None

const WNDCLASSEXA = struct {
    cbSize:        UINT
    style:         UINT
    lpfnWndProc:   WNDPROC
    cbClsExtra:    int
    cbWndExtra:    int
    hInstance:     HINSTANCE
    hIcon:         HICON
    hCursor:       HCURSOR
    hbrBackground: HBRUSH
    lpszMenuName:  LPCSTR
    lpszClassName: LPCSTR
    hIconSm:       HICON
}

const POINT = struct {
    x: LONG
    y: LONG
}

const MSG = struct {
    hwnd:    HWND
    message: UINT
    wParam:  WPARAM
    lParam:  LPARAM
    time:    DWORD
    pt:      POINT
}

const WM_NULL                         = 0x0000
const WM_CREATE                       = 0x0001
const WM_DESTROY                      = 0x0002
const WM_MOVE                         = 0x0003
const WM_SIZE                         = 0x0005
const WM_ACTIVATE                     = 0x0006
const WM_SETFOCUS                     = 0x0007
const WM_KILLFOCUS                    = 0x0008
const WM_ENABLE                       = 0x000A
const WM_SETREDRAW                    = 0x000B
const WM_SETTEXT                      = 0x000C
const WM_GETTEXT                      = 0x000D
const WM_GETTEXTLENGTH                = 0x000E
const WM_PAINT                        = 0x000F
const WM_CLOSE                        = 0x0010
const WM_QUERYENDSESSION              = 0x0011
const WM_QUERYOPEN                    = 0x0013
const WM_ENDSESSION                   = 0x0016
const WM_QUIT                         = 0x0012
const WM_ERASEBKGND                   = 0x0014
const WM_SYSCOLORCHANGE               = 0x0015
const WM_SHOWWINDOW                   = 0x0018
const WM_WININICHANGE                 = 0x001A
const WM_SETTINGCHANGE                = WM_WININICHANGE
const WM_DEVMODECHANGE                = 0x001B
const WM_ACTIVATEAPP                  = 0x001C
const WM_FONTCHANGE                   = 0x001D
const WM_TIMECHANGE                   = 0x001E
const WM_CANCELMODE                   = 0x001F
const WM_SETCURSOR                    = 0x0020
const WM_MOUSEACTIVATE                = 0x0021
const WM_CHILDACTIVATE                = 0x0022
const WM_QUEUESYNC                    = 0x0023
const WM_GETMINMAXINFO                = 0x0024
const WM_PAINTICON                    = 0x0026
const WM_ICONERASEBKGND               = 0x0027
const WM_NEXTDLGCTL                   = 0x0028
const WM_SPOOLERSTATUS                = 0x002A
const WM_DRAWITEM                     = 0x002B
const WM_MEASUREITEM                  = 0x002C
const WM_DELETEITEM                   = 0x002D
const WM_VKEYTOITEM                   = 0x002E
const WM_CHARTOITEM                   = 0x002F
const WM_SETFONT                      = 0x0030
const WM_GETFONT                      = 0x0031
const WM_SETHOTKEY                    = 0x0032
const WM_GETHOTKEY                    = 0x0033
const WM_QUERYDRAGICON                = 0x0037
const WM_COMPAREITEM                  = 0x0039
const WM_GETOBJECT                    = 0x003D
const WM_COMPACTING                   = 0x0041
const WM_COMMNOTIFY                   = 0x0044  /* no longer suported */
const WM_WINDOWPOSCHANGING            = 0x0046
const WM_WINDOWPOSCHANGED             = 0x0047
const WM_POWER                        = 0x0048
const WM_COPYDATA                     = 0x004A
const WM_CANCELJOURNAL                = 0x004B
const WM_NOTIFY                       = 0x004E
const WM_INPUTLANGCHANGEREQUEST       = 0x0050
const WM_INPUTLANGCHANGE              = 0x0051
const WM_TCARD                        = 0x0052
const WM_HELP                         = 0x0053
const WM_USERCHANGED                  = 0x0054
const WM_NOTIFYFORMAT                 = 0x0055
const WM_CONTEXTMENU                  = 0x007B
const WM_STYLECHANGING                = 0x007C
const WM_STYLECHANGED                 = 0x007D
const WM_DISPLAYCHANGE                = 0x007E
const WM_GETICON                      = 0x007F
const WM_SETICON                      = 0x0080
const WM_NCCREATE                     = 0x0081
const WM_NCDESTROY                    = 0x0082
const WM_NCCALCSIZE                   = 0x0083
const WM_NCHITTEST                    = 0x0084
const WM_NCPAINT                      = 0x0085
const WM_NCACTIVATE                   = 0x0086
const WM_GETDLGCODE                   = 0x0087
const WM_SYNCPAINT                    = 0x0088
const WM_NCMOUSEMOVE                  = 0x00A0
const WM_NCLBUTTONDOWN                = 0x00A1
const WM_NCLBUTTONUP                  = 0x00A2
const WM_NCLBUTTONDBLCLK              = 0x00A3
const WM_NCRBUTTONDOWN                = 0x00A4
const WM_NCRBUTTONUP                  = 0x00A5
const WM_NCRBUTTONDBLCLK              = 0x00A6
const WM_NCMBUTTONDOWN                = 0x00A7
const WM_NCMBUTTONUP                  = 0x00A8
const WM_NCMBUTTONDBLCLK              = 0x00A9
const WM_NCXBUTTONDOWN                = 0x00AB
const WM_NCXBUTTONUP                  = 0x00AC
const WM_NCXBUTTONDBLCLK              = 0x00AD
const WM_INPUT_DEVICE_CHANGE          = 0x00FE
const WM_INPUT                        = 0x00FF
const WM_KEYFIRST                     = 0x0100
const WM_KEYDOWN                      = 0x0100
const WM_KEYUP                        = 0x0101
const WM_CHAR                         = 0x0102
const WM_DEADCHAR                     = 0x0103
const WM_SYSKEYDOWN                   = 0x0104
const WM_SYSKEYUP                     = 0x0105
const WM_SYSCHAR                      = 0x0106
const WM_SYSDEADCHAR                  = 0x0107
const WM_UNICHAR                      = 0x0109
const WM_KEYLAST                      = 0x0109
const WM_IME_STARTCOMPOSITION         = 0x010D
const WM_IME_ENDCOMPOSITION           = 0x010E
const WM_IME_COMPOSITION              = 0x010F
const WM_IME_KEYLAST                  = 0x010F
const WM_INITDIALOG                   = 0x0110
const WM_COMMAND                      = 0x0111
const WM_SYSCOMMAND                   = 0x0112
const WM_TIMER                        = 0x0113
const WM_HSCROLL                      = 0x0114
const WM_VSCROLL                      = 0x0115
const WM_INITMENU                     = 0x0116
const WM_INITMENUPOPUP                = 0x0117
const WM_GESTURE                      = 0x0119
const WM_GESTURENOTIFY                = 0x011A
const WM_MENUSELECT                   = 0x011F
const WM_MENUCHAR                     = 0x0120
const WM_ENTERIDLE                    = 0x0121
const WM_MENURBUTTONUP                = 0x0122
const WM_MENUDRAG                     = 0x0123
const WM_MENUGETOBJECT                = 0x0124
const WM_UNINITMENUPOPUP              = 0x0125
const WM_MENUCOMMAND                  = 0x0126
const WM_CHANGEUISTATE                = 0x0127
const WM_UPDATEUISTATE                = 0x0128
const WM_QUERYUISTATE                 = 0x0129
const WM_CTLCOLORMSGBOX               = 0x0132
const WM_CTLCOLOREDIT                 = 0x0133
const WM_CTLCOLORLISTBOX              = 0x0134
const WM_CTLCOLORBTN                  = 0x0135
const WM_CTLCOLORDLG                  = 0x0136
const WM_CTLCOLORSCROLLBAR            = 0x0137
const WM_CTLCOLORSTATIC               = 0x0138
const WM_MOUSEFIRST                   = 0x0200
const WM_MOUSEMOVE                    = 0x0200
const WM_LBUTTONDOWN                  = 0x0201
const WM_LBUTTONUP                    = 0x0202
const WM_LBUTTONDBLCLK                = 0x0203
const WM_RBUTTONDOWN                  = 0x0204
const WM_RBUTTONUP                    = 0x0205
const WM_RBUTTONDBLCLK                = 0x0206
const WM_MBUTTONDOWN                  = 0x0207
const WM_MBUTTONUP                    = 0x0208
const WM_MBUTTONDBLCLK                = 0x0209
const WM_MOUSEWHEEL                   = 0x020A
const WM_XBUTTONDOWN                  = 0x020B
const WM_XBUTTONUP                    = 0x020C
const WM_XBUTTONDBLCLK                = 0x020D
const WM_MOUSEHWHEEL                  = 0x020E
const WM_MOUSELAST                    = 0x020E
const WM_PARENTNOTIFY                 = 0x0210
const WM_ENTERMENULOOP                = 0x0211
const WM_EXITMENULOOP                 = 0x0212
const WM_NEXTMENU                     = 0x0213
const WM_SIZING                       = 0x0214
const WM_CAPTURECHANGED               = 0x0215
const WM_MOVING                       = 0x0216
const WM_POWERBROADCAST               = 0x0218
const WM_DEVICECHANGE                 = 0x0219
const WM_MDICREATE                    = 0x0220
const WM_MDIDESTROY                   = 0x0221
const WM_MDIACTIVATE                  = 0x0222
const WM_MDIRESTORE                   = 0x0223
const WM_MDINEXT                      = 0x0224
const WM_MDIMAXIMIZE                  = 0x0225
const WM_MDITILE                      = 0x0226
const WM_MDICASCADE                   = 0x0227
const WM_MDIICONARRANGE               = 0x0228
const WM_MDIGETACTIVE                 = 0x0229
const WM_MDISETMENU                   = 0x0230
const WM_ENTERSIZEMOVE                = 0x0231
const WM_EXITSIZEMOVE                 = 0x0232
const WM_DROPFILES                    = 0x0233
const WM_MDIREFRESHMENU               = 0x0234
const WM_POINTERDEVICECHANGE          = 0x238
const WM_POINTERDEVICEINRANGE         = 0x239
const WM_POINTERDEVICEOUTOFRANGE      = 0x23A
const WM_TOUCH                        = 0x0240
const WM_NCPOINTERUPDATE              = 0x0241
const WM_NCPOINTERDOWN                = 0x0242
const WM_NCPOINTERUP                  = 0x0243
const WM_POINTERUPDATE                = 0x0245
const WM_POINTERDOWN                  = 0x0246
const WM_POINTERUP                    = 0x0247
const WM_POINTERENTER                 = 0x0249
const WM_POINTERLEAVE                 = 0x024A
const WM_POINTERACTIVATE              = 0x024B
const WM_POINTERCAPTURECHANGED        = 0x024C
const WM_TOUCHHITTESTING              = 0x024D
const WM_POINTERWHEEL                 = 0x024E
const WM_POINTERHWHEEL                = 0x024F
const WM_POINTERROUTEDTO              = 0x0251
const WM_POINTERROUTEDAWAY            = 0x0252
const WM_POINTERROUTEDRELEASED        = 0x0253
const WM_IME_SETCONTEXT               = 0x0281
const WM_IME_NOTIFY                   = 0x0282
const WM_IME_CONTROL                  = 0x0283
const WM_IME_COMPOSITIONFULL          = 0x0284
const WM_IME_SELECT                   = 0x0285
const WM_IME_CHAR                     = 0x0286
const WM_IME_REQUEST                  = 0x0288
const WM_IME_KEYDOWN                  = 0x0290
const WM_IME_KEYUP                    = 0x0291
const WM_MOUSEHOVER                   = 0x02A1
const WM_MOUSELEAVE                   = 0x02A3
const WM_NCMOUSEHOVER                 = 0x02A0
const WM_NCMOUSELEAVE                 = 0x02A2
const WM_WTSSESSION_CHANGE            = 0x02B1
const WM_TABLET_FIRST                 = 0x02c0
const WM_TABLET_LAST                  = 0x02df
const WM_DPICHANGED                   = 0x02E0
const WM_DPICHANGED_BEFOREPARENT      = 0x02E2
const WM_DPICHANGED_AFTERPARENT       = 0x02E3
const WM_GETDPISCALEDSIZE             = 0x02E4
const WM_CUT                          = 0x0300
const WM_COPY                         = 0x0301
const WM_PASTE                        = 0x0302
const WM_CLEAR                        = 0x0303
const WM_UNDO                         = 0x0304
const WM_RENDERFORMAT                 = 0x0305
const WM_RENDERALLFORMATS             = 0x0306
const WM_DESTROYCLIPBOARD             = 0x0307
const WM_DRAWCLIPBOARD                = 0x0308
const WM_PAINTCLIPBOARD               = 0x0309
const WM_VSCROLLCLIPBOARD             = 0x030A
const WM_SIZECLIPBOARD                = 0x030B
const WM_ASKCBFORMATNAME              = 0x030C
const WM_CHANGECBCHAIN                = 0x030D
const WM_HSCROLLCLIPBOARD             = 0x030E
const WM_QUERYNEWPALETTE              = 0x030F
const WM_PALETTEISCHANGING            = 0x0310
const WM_PALETTECHANGED               = 0x0311
const WM_HOTKEY                       = 0x0312
const WM_PRINT                        = 0x0317
const WM_PRINTCLIENT                  = 0x0318
const WM_APPCOMMAND                   = 0x0319
const WM_THEMECHANGED                 = 0x031A
const WM_CLIPBOARDUPDATE              = 0x031D
const WM_DWMCOMPOSITIONCHANGED        = 0x031E
const WM_DWMNCRENDERINGCHANGED        = 0x031F
const WM_DWMCOLORIZATIONCOLORCHANGED  = 0x0320
const WM_DWMWINDOWMAXIMIZEDCHANGE     = 0x0321
const WM_DWMSENDICONICTHUMBNAIL           = 0x0323
const WM_DWMSENDICONICLIVEPREVIEWBITMAP   = 0x0326
const WM_GETTITLEBARINFOEX            = 0x033F
const WM_HANDHELDFIRST                = 0x0358
const WM_HANDHELDLAST                 = 0x035F
const WM_AFXFIRST                     = 0x0360
const WM_AFXLAST                      = 0x037F
const WM_PENWINFIRST                  = 0x0380
const WM_PENWINLAST                   = 0x038F
const WM_APP                          = 0x8000
const WM_USER                         = 0x0400


const WS_OVERLAPPED       : DWORD = 0x00000000
const WS_POPUP            : DWORD = 0x80000000
const WS_CHILD            : DWORD = 0x40000000
const WS_MINIMIZE         : DWORD = 0x20000000
const WS_VISIBLE          : DWORD = 0x10000000
const WS_DISABLED         : DWORD = 0x08000000
const WS_CLIPSIBLINGS     : DWORD = 0x04000000
const WS_CLIPCHILDREN     : DWORD = 0x02000000
const WS_MAXIMIZE         : DWORD = 0x01000000
const WS_BORDER           : DWORD = 0x00800000
const WS_DLGFRAME         : DWORD = 0x00400000
const WS_VSCROLL          : DWORD = 0x00200000
const WS_HSCROLL          : DWORD = 0x00100000
const WS_SYSMENU          : DWORD = 0x00080000
const WS_THICKFRAME       : DWORD = 0x00040000
const WS_GROUP            : DWORD = 0x00020000
const WS_TABSTOP          : DWORD = 0x00010000
const WS_MINIMIZEBOX      : DWORD = 0x00020000
const WS_MAXIMIZEBOX      : DWORD = 0x00010000
const WS_CAPTION          : DWORD = WS_BORDER | WS_DLGFRAME
const WS_OVERLAPPEDWINDOW : DWORD = WS_OVERLAPPED     |
    WS_CAPTION        |
    WS_SYSMENU        |
    WS_THICKFRAME     |
    WS_MINIMIZEBOX    |
    WS_MAXIMIZEBOX
    
const CW_USEDEFAULT       : int = 0x80000000

const PM_NOREMOVE         = 0x0000
const PM_REMOVE           = 0x0001
const PM_NOYIELD          = 0x0002

#extern "user32"
const RegisterClassExA = (class: *WNDCLASSEXA): *None => #extern

const CreateWindowExA = (
    dwExStyle: DWORD,
    lpClassName: LPCSTR,
    lpWindowName: LPCSTR,
    dwStyle: DWORD,
    X: int,
    Y: int,
    nWidth: int,
    nHeight: int,
    hWndParent: HWND,
    hMenu: HMENU,
    hInstance: HINSTANCE,
    lpParam: LPVOID,
): HWND => #extern

const DefWindowProcA = (
    hWnd: HWND,
    Msg: UINT,
    wParam: WPARAM,
    lParam: LPARAM,
): LRESULT => #extern;
    
const PostQuitMessage = (exit_code: int): None => #extern

const PeekMessageA = (
    lpMsg: *var MSG,
    hWnd: HWND,
    wMsgFilterMin: UINT,
    wMsgFilterMax: UINT,
    wRemoveMsg: UINT,
): BOOL => #extern
    
// FIXME lpMsg is const in c
const TranslateMessage = (lpMsg: *var MSG): BOOL => #extern
// FIXME lpMsg is const in c
const DispatchMessageA = (lpMsg: *var MSG): LRESULT => #extern;

#extern "kernel32"
const GetModuleHandleA = (module_name: LPCSTR): HMODULE => #extern
