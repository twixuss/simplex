
import "c_types"

const VOID = None
const PVOID = *var None
const LPVOID = *var None
const LPCVOID = *None

const BOOL = Bool

const CHAR = U8
const UCHAR = U8
const PUCHAR = *var U8
const LPSTR = *var U8
const LPCSTR = *U8

const WCHAR = U16
const LPWCH = *var U16
const PWSTR = *var U16
const LPWSTR = *var U16
const LPCWSTR = *U16

const DWORD = U32
const PDWORD = *var U32
const LPDWORD = *var U32

const UINT = U32

const ULONG = U32
const PULONG = *var U32

const LONG = S32
const PLONG = *var S32

const SIZE_T = U64

const ULONGLONG = U64

const ULONG_PTR = U64

const ULARGE_INTEGER = U64
const PULARGE_INTEGER = *var U64

const SSIZE_T = S64

const LONGLONG = S64

const LARGE_INTEGER = S64
const PLARGE_INTEGER = *var S64


const WPARAM = U64
const LPARAM = S64
const LRESULT = S64
const HRESULT = S32

const HWND = *None
const HANDLE = *None
const LPHANDLE = *var *None
const HMENU = *None
const HMODULE = *None
const HINSTANCE = *None
const HCURSOR = *None
const HICON = *None
const HBRUSH = *None
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

const MAX_PATH = 260

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


const WS_OVERLAPPED       = 0x00000000
const WS_POPUP            = 0x80000000
const WS_CHILD            = 0x40000000
const WS_MINIMIZE         = 0x20000000
const WS_VISIBLE          = 0x10000000
const WS_DISABLED         = 0x08000000
const WS_CLIPSIBLINGS     = 0x04000000
const WS_CLIPCHILDREN     = 0x02000000
const WS_MAXIMIZE         = 0x01000000
const WS_BORDER           = 0x00800000
const WS_DLGFRAME         = 0x00400000
const WS_VSCROLL          = 0x00200000
const WS_HSCROLL          = 0x00100000
const WS_SYSMENU          = 0x00080000
const WS_THICKFRAME       = 0x00040000
const WS_GROUP            = 0x00020000
const WS_TABSTOP          = 0x00010000
const WS_MINIMIZEBOX      = 0x00020000
const WS_MAXIMIZEBOX      = 0x00010000
const WS_CAPTION          = WS_BORDER | WS_DLGFRAME
const WS_OVERLAPPEDWINDOW = WS_OVERLAPPED     |
    WS_CAPTION        |
    WS_SYSMENU        |
    WS_THICKFRAME     |
    WS_MINIMIZEBOX    |
    WS_MAXIMIZEBOX
    
const CW_USEDEFAULT = 0x80000000

const PM_NOREMOVE         = 0x0000
const PM_REMOVE           = 0x0001
const PM_NOYIELD          = 0x0002


const PAGE_NOACCESS           = 0x01    
const PAGE_READONLY           = 0x02    
const PAGE_READWRITE          = 0x04    
const PAGE_WRITECOPY          = 0x08    
const PAGE_EXECUTE            = 0x10    
const PAGE_EXECUTE_READ       = 0x20    
const PAGE_EXECUTE_READWRITE  = 0x40    
const PAGE_EXECUTE_WRITECOPY  = 0x80    
const PAGE_GUARD             = 0x100    
const PAGE_NOCACHE           = 0x200    
const PAGE_WRITECOMBINE      = 0x400    
const PAGE_GRAPHICS_NOACCESS           = 0x0800    
const PAGE_GRAPHICS_READONLY           = 0x1000    
const PAGE_GRAPHICS_READWRITE          = 0x2000    
const PAGE_GRAPHICS_EXECUTE            = 0x4000    
const PAGE_GRAPHICS_EXECUTE_READ       = 0x8000    
const PAGE_GRAPHICS_EXECUTE_READWRITE = 0x10000    
const PAGE_GRAPHICS_COHERENT          = 0x20000    
const PAGE_ENCLAVE_THREAD_CONTROL = 0x80000000  
const PAGE_REVERT_TO_FILE_MAP     = 0x80000000  
const PAGE_TARGETS_NO_UPDATE      = 0x40000000  
const PAGE_TARGETS_INVALID        = 0x40000000  
const PAGE_ENCLAVE_UNVALIDATED    = 0x20000000  
const PAGE_ENCLAVE_DECOMMIT       = 0x10000000  
const MEM_COMMIT                      = 0x00001000  
const MEM_RESERVE                     = 0x00002000  
const MEM_REPLACE_PLACEHOLDER         = 0x00004000  
const MEM_RESERVE_PLACEHOLDER         = 0x00040000  
const MEM_RESET                       = 0x00080000  
const MEM_TOP_DOWN                    = 0x00100000  
const MEM_WRITE_WATCH                 = 0x00200000  
const MEM_PHYSICAL                    = 0x00400000  
const MEM_ROTATE                      = 0x00800000  
const MEM_DIFFERENT_IMAGE_BASE_OK     = 0x00800000  
const MEM_RESET_UNDO                  = 0x01000000  
const MEM_LARGE_PAGES                 = 0x20000000  
const MEM_4MB_PAGES                   = 0x80000000  
const MEM_64K_PAGES                   = MEM_LARGE_PAGES | MEM_PHYSICAL
const MEM_UNMAP_WITH_TRANSIENT_BOOST  = 0x00000001  
const MEM_COALESCE_PLACEHOLDERS       = 0x00000001  
const MEM_PRESERVE_PLACEHOLDER        = 0x00000002  
const MEM_DECOMMIT                    = 0x00004000  
const MEM_RELEASE                     = 0x00008000  
const MEM_FREE                        = 0x00010000  

// begin_wdm
//
//  The following are masks for the predefined standard access types
//

const DELETE                           = (0x00010000)
const READ_CONTROL                     = (0x00020000)
const WRITE_DAC                        = (0x00040000)
const WRITE_OWNER                      = (0x00080000)
const SYNCHRONIZE                      = (0x00100000)

const STANDARD_RIGHTS_REQUIRED         = (0x000F0000)

const STANDARD_RIGHTS_READ             = (READ_CONTROL)
const STANDARD_RIGHTS_WRITE            = (READ_CONTROL)
const STANDARD_RIGHTS_EXECUTE          = (READ_CONTROL)

const STANDARD_RIGHTS_ALL              = (0x001F0000)

const SPECIFIC_RIGHTS_ALL              = (0x0000FFFF)

//
// AccessSystemAcl access type
//

const ACCESS_SYSTEM_SECURITY           = (0x01000000)

//
// MaximumAllowed access type
//

const MAXIMUM_ALLOWED                  = (0x02000000)

//
//  These are the generic rights.
//

const GENERIC_READ    = 0x80000000
const GENERIC_WRITE   = 0x40000000
const GENERIC_EXECUTE = 0x20000000
const GENERIC_ALL     = 0x10000000

const FILE_READ_DATA            = ( 0x0001 )    // file & pipe
const FILE_LIST_DIRECTORY       = ( 0x0001 )    // directory

const FILE_WRITE_DATA           = ( 0x0002 )    // file & pipe
const FILE_ADD_FILE             = ( 0x0002 )    // directory

const FILE_APPEND_DATA          = ( 0x0004 )    // file
const FILE_ADD_SUBDIRECTORY     = ( 0x0004 )    // directory
const FILE_CREATE_PIPE_INSTANCE = ( 0x0004 )    // named pipe


const FILE_READ_EA              = ( 0x0008 )    // file & directory

const FILE_WRITE_EA             = ( 0x0010 )    // file & directory

const FILE_EXECUTE              = ( 0x0020 )    // file
const FILE_TRAVERSE             = ( 0x0020 )    // directory

const FILE_DELETE_CHILD         = ( 0x0040 )    // directory

const FILE_READ_ATTRIBUTES      = ( 0x0080 )    // all

const FILE_WRITE_ATTRIBUTES     = ( 0x0100 )    // all

const FILE_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x1FF)

const FILE_GENERIC_READ         = (STANDARD_RIGHTS_READ     |
                                   FILE_READ_DATA           |
                                   FILE_READ_ATTRIBUTES     |
                                   FILE_READ_EA             |
                                   SYNCHRONIZE)


const FILE_GENERIC_WRITE        = (STANDARD_RIGHTS_WRITE    |
                                   FILE_WRITE_DATA          |
                                   FILE_WRITE_ATTRIBUTES    |
                                   FILE_WRITE_EA            |
                                   FILE_APPEND_DATA         |
                                   SYNCHRONIZE)


const FILE_GENERIC_EXECUTE      = (STANDARD_RIGHTS_EXECUTE  |
                                   FILE_READ_ATTRIBUTES     |
                                   FILE_EXECUTE             |
                                   SYNCHRONIZE)

// end_access
const FILE_SHARE_READ                 = 0x00000001  
const FILE_SHARE_WRITE                = 0x00000002  
const FILE_SHARE_DELETE               = 0x00000004  
const FILE_ATTRIBUTE_READONLY             = 0x00000001  
const FILE_ATTRIBUTE_HIDDEN               = 0x00000002  
const FILE_ATTRIBUTE_SYSTEM               = 0x00000004  
const FILE_ATTRIBUTE_DIRECTORY            = 0x00000010  
const FILE_ATTRIBUTE_ARCHIVE              = 0x00000020  
const FILE_ATTRIBUTE_DEVICE               = 0x00000040  
const FILE_ATTRIBUTE_NORMAL               = 0x00000080  
const FILE_ATTRIBUTE_TEMPORARY            = 0x00000100  
const FILE_ATTRIBUTE_SPARSE_FILE          = 0x00000200  
const FILE_ATTRIBUTE_REPARSE_POINT        = 0x00000400  
const FILE_ATTRIBUTE_COMPRESSED           = 0x00000800  
const FILE_ATTRIBUTE_OFFLINE              = 0x00001000  
const FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = 0x00002000  
const FILE_ATTRIBUTE_ENCRYPTED            = 0x00004000  
const FILE_ATTRIBUTE_INTEGRITY_STREAM     = 0x00008000  
const FILE_ATTRIBUTE_VIRTUAL              = 0x00010000  
const FILE_ATTRIBUTE_NO_SCRUB_DATA        = 0x00020000  
const FILE_ATTRIBUTE_EA                   = 0x00040000  
const FILE_ATTRIBUTE_PINNED               = 0x00080000  
const FILE_ATTRIBUTE_UNPINNED             = 0x00100000  
const FILE_ATTRIBUTE_RECALL_ON_OPEN       = 0x00040000  
const FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = 0x00400000 
const TREE_CONNECT_ATTRIBUTE_PRIVACY      = 0x00004000  
const TREE_CONNECT_ATTRIBUTE_INTEGRITY    = 0x00008000  
const TREE_CONNECT_ATTRIBUTE_GLOBAL       = 0x00000004  
const TREE_CONNECT_ATTRIBUTE_PINNED       = 0x00000002  
const FILE_ATTRIBUTE_STRICTLY_SEQUENTIAL  = 0x20000000  
const FILE_NOTIFY_CHANGE_FILE_NAME    = 0x00000001   
const FILE_NOTIFY_CHANGE_DIR_NAME     = 0x00000002   
const FILE_NOTIFY_CHANGE_ATTRIBUTES   = 0x00000004   
const FILE_NOTIFY_CHANGE_SIZE         = 0x00000008   
const FILE_NOTIFY_CHANGE_LAST_WRITE   = 0x00000010   
const FILE_NOTIFY_CHANGE_LAST_ACCESS  = 0x00000020   
const FILE_NOTIFY_CHANGE_CREATION     = 0x00000040   
const FILE_NOTIFY_CHANGE_SECURITY     = 0x00000100   
const FILE_ACTION_ADDED                   = 0x00000001   
const FILE_ACTION_REMOVED                 = 0x00000002   
const FILE_ACTION_MODIFIED                = 0x00000003   
const FILE_ACTION_RENAMED_OLD_NAME        = 0x00000004   
const FILE_ACTION_RENAMED_NEW_NAME        = 0x00000005   
const MAILSLOT_NO_MESSAGE             = -1
const MAILSLOT_WAIT_FOREVER           = -1
const FILE_CASE_SENSITIVE_SEARCH          = 0x00000001  
const FILE_CASE_PRESERVED_NAMES           = 0x00000002  
const FILE_UNICODE_ON_DISK                = 0x00000004  
const FILE_PERSISTENT_ACLS                = 0x00000008  
const FILE_FILE_COMPRESSION               = 0x00000010  
const FILE_VOLUME_QUOTAS                  = 0x00000020  
const FILE_SUPPORTS_SPARSE_FILES          = 0x00000040  
const FILE_SUPPORTS_REPARSE_POINTS        = 0x00000080  
const FILE_SUPPORTS_REMOTE_STORAGE        = 0x00000100  
const FILE_RETURNS_CLEANUP_RESULT_INFO    = 0x00000200  
const FILE_SUPPORTS_POSIX_UNLINK_RENAME   = 0x00000400  




const FILE_VOLUME_IS_COMPRESSED           = 0x00008000  
const FILE_SUPPORTS_OBJECT_IDS            = 0x00010000  
const FILE_SUPPORTS_ENCRYPTION            = 0x00020000  
const FILE_NAMED_STREAMS                  = 0x00040000  
const FILE_READ_ONLY_VOLUME               = 0x00080000  
const FILE_SEQUENTIAL_WRITE_ONCE          = 0x00100000  
const FILE_SUPPORTS_TRANSACTIONS          = 0x00200000  
const FILE_SUPPORTS_HARD_LINKS            = 0x00400000  
const FILE_SUPPORTS_EXTENDED_ATTRIBUTES   = 0x00800000  
const FILE_SUPPORTS_OPEN_BY_FILE_ID       = 0x01000000  
const FILE_SUPPORTS_USN_JOURNAL           = 0x02000000  
const FILE_SUPPORTS_INTEGRITY_STREAMS     = 0x04000000  
const FILE_SUPPORTS_BLOCK_REFCOUNTING     = 0x08000000  
const FILE_SUPPORTS_SPARSE_VDL            = 0x10000000  
const FILE_DAX_VOLUME                     = 0x20000000  
const FILE_SUPPORTS_GHOSTING              = 0x40000000  

const FILE_INVALID_FILE_ID               = -1

const FILE_BEGIN           = 0
const FILE_CURRENT         = 1
const FILE_END             = 2
//
// Constants
//
const CREATE_NEW          = 1
const CREATE_ALWAYS       = 2
const OPEN_EXISTING       = 3
const OPEN_ALWAYS         = 4
const TRUNCATE_EXISTING   = 5

const INVALID_FILE_SIZE = 0xFFFFFFFF
const INVALID_SET_FILE_POINTER = -1
const INVALID_FILE_ATTRIBUTES = -1

const INVALID_HANDLE_VALUE = (-1 as HANDLE)

#extern "user32"
fn RegisterClassExA(class: *WNDCLASSEXA): *None => #extern

fn CreateWindowExA(
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

fn DefWindowProcA(
    hWnd: HWND,
    Msg: UINT,
    wParam: WPARAM,
    lParam: LPARAM,
): LRESULT => #extern;
    
fn PostQuitMessage(exit_code: int): None => #extern

fn PeekMessageA(
    lpMsg: *var MSG,
    hWnd: HWND,
    wMsgFilterMin: UINT,
    wMsgFilterMax: UINT,
    wRemoveMsg: UINT,
): BOOL => #extern
    
// FIXME lpMsg is const in c
fn TranslateMessage(lpMsg: *var MSG): BOOL => #extern
// FIXME lpMsg is const in c
fn DispatchMessageA(lpMsg: *var MSG): LRESULT => #extern;

#extern "kernel32"
fn CloseHandle(hObject: HANDLE): BOOL => #extern
fn DuplicateHandle(hSourceProcessHandle: HANDLE, hSourceHandle: HANDLE, hTargetProcessHandle: HANDLE, lpTargetHandle: LPHANDLE, dwDesiredAccess: DWORD, bInheritHandle: BOOL, dwOptions: DWORD): BOOL => #extern
fn CompareObjectHandles(hFirstObjectHandle: HANDLE, hSecondObjectHandle: HANDLE): BOOL => #extern
fn GetHandleInformation(hObject: HANDLE, lpdwFlags: LPDWORD): BOOL => #extern
fn SetHandleInformation(hObject: HANDLE, dwMask: DWORD, dwFlags: DWORD): BOOL => #extern
fn GetModuleHandleA(module_name: LPCSTR): HMODULE => #extern

fn VirtualAlloc(
    lpAddress: LPVOID,
    dwSize: SIZE_T,
    flAllocationType: DWORD,
    flProtect: DWORD,
): LPVOID => #extern

fn VirtualFree(
    lpAddress: LPVOID,
    dwSize: SIZE_T,
    dwFreeType: DWORD,
): BOOL => #extern

fn GetLastError(): DWORD => #extern;

//
// fileapi.h
//
const DISK_SPACE_INFORMATION = struct {
    ActualAvailableAllocationUnits:       ULONGLONG
    ActualPoolUnavailableAllocationUnits: ULONGLONG
    CallerTotalAllocationUnits:           ULONGLONG
    CallerAvailableAllocationUnits:       ULONGLONG
    CallerPoolUnavailableAllocationUnits: ULONGLONG
    UsedAllocationUnits:                  ULONGLONG
    TotalReservedAllocationUnits:         ULONGLONG
    VolumeStorageReserveAllocationUnits:  ULONGLONG
    AvailableCommittedAllocationUnits:    ULONGLONG
    PoolAvailableAllocationUnits:         ULONGLONG
    SectorsPerAllocationUnit:             DWORD
    BytesPerSector:                       DWORD
}

const WIN32_FILE_ATTRIBUTE_DATA = struct {
    dwFileAttributes: DWORD
    ftCreationTime:   FILETIME
    ftLastAccessTime: FILETIME
    ftLastWriteTime:  FILETIME
    nFileSizeHigh:    DWORD
    nFileSizeLow:     DWORD
}
const LPWIN32_FILE_ATTRIBUTE_DATA = *WIN32_FILE_ATTRIBUTE_DATA

const BY_HANDLE_FILE_INFORMATION = struct {
    dwFileAttributes:     DWORD
    ftCreationTime:       FILETIME
    ftLastAccessTime:     FILETIME
    ftLastWriteTime:      FILETIME
    dwVolumeSerialNumber: DWORD
    nFileSizeHigh:        DWORD
    nFileSizeLow:         DWORD
    nNumberOfLinks:       DWORD
    nFileIndexHigh:       DWORD
    nFileIndexLow:        DWORD
}
const PBY_HANDLE_FILE_INFORMATION = *BY_HANDLE_FILE_INFORMATION
const LPBY_HANDLE_FILE_INFORMATION = *BY_HANDLE_FILE_INFORMATION

const CREATEFILE2_EXTENDED_PARAMETERS = struct {
    dwSize:               DWORD
    dwFileAttributes:     DWORD
    dwFileFlags:          DWORD
    dwSecurityQosFlags:   DWORD
    lpSecurityAttributes: LPSECURITY_ATTRIBUTES
    hTemplateFile:        HANDLE
}
const PCREATEFILE2_EXTENDED_PARAMETERS = *CREATEFILE2_EXTENDED_PARAMETERS
const LPCREATEFILE2_EXTENDED_PARAMETERS = *CREATEFILE2_EXTENDED_PARAMETERS

const STREAM_INFO_LEVELS = S32
const FindStreamInfoStandard = 0
const FindStreamInfoMaxInfoLevel = 1

const WIN32_FIND_STREAM_DATA = struct {
    StreamSize:  LARGE_INTEGER
    cStreamName: [MAX_PATH + 36]WCHAR
}
const PWIN32_FIND_STREAM_DATA = *WIN32_FIND_STREAM_DATA

fn CompareFileTime(lpFileTime1: *let FILETIME, lpFileTime2: *let FILETIME): LONG => #extern
fn CreateDirectoryA(lpPathName: LPCSTR, lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL => #extern
fn CreateDirectoryW(lpPathName: LPCWSTR, lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL => #extern
fn CreateFileA(lpFileName: LPCSTR, dwDesiredAccess: DWORD, dwShareMode: DWORD, lpSecurityAttributes: LPSECURITY_ATTRIBUTES, dwCreationDisposition: DWORD, dwFlagsAndAttributes: DWORD, hTemplateFile: HANDLE): HANDLE => #extern
fn CreateFileW(lpFileName: LPCWSTR, dwDesiredAccess: DWORD, dwShareMode: DWORD, lpSecurityAttributes: LPSECURITY_ATTRIBUTES, dwCreationDisposition: DWORD, dwFlagsAndAttributes: DWORD, hTemplateFile: HANDLE): HANDLE => #extern
fn DefineDosDeviceW(dwFlags: DWORD, lpDeviceName: LPCWSTR, lpTargetPath: LPCWSTR): BOOL => #extern
fn DeleteFileA(lpFileName: LPCSTR): BOOL => #extern
fn DeleteFileW(lpFileName: LPCWSTR): BOOL => #extern
fn DeleteVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR): BOOL => #extern
fn FileTimeToLocalFileTime(lpFileTime: *let FILETIME, lpLocalFileTime: LPFILETIME): BOOL => #extern
fn FindClose(hFindFile: HANDLE): BOOL => #extern
fn FindCloseChangeNotification(hChangeHandle: HANDLE): BOOL => #extern
fn FindFirstChangeNotificationA(lpPathName: LPCSTR, bWatchSubtree: BOOL, dwNotifyFilter: DWORD): HANDLE => #extern
fn FindFirstChangeNotificationW(lpPathName: LPCWSTR, bWatchSubtree: BOOL, dwNotifyFilter: DWORD): HANDLE => #extern
fn FindFirstFileA(lpFileName: LPCSTR, lpFindFileData: LPWIN32_FIND_DATAA): HANDLE => #extern
fn FindFirstFileW(lpFileName: LPCWSTR, lpFindFileData: LPWIN32_FIND_DATAW): HANDLE => #extern
//fn FindFirstFileExA(lpFileName: LPCSTR, fInfoLevelId: FINDEX_INFO_LEVELS, lpFindFileData: LPVOID, fSearchOp: FINDEX_SEARCH_OPS, lpSearchFilter: LPVOID, dwAdditionalFlags: DWORD): HANDLE => #extern
//fn FindFirstFileExW(lpFileName: LPCWSTR, fInfoLevelId: FINDEX_INFO_LEVELS, lpFindFileData: LPVOID, fSearchOp: FINDEX_SEARCH_OPS, lpSearchFilter: LPVOID, dwAdditionalFlags: DWORD): HANDLE => #extern
fn FindFirstVolumeW(lpszVolumeName: LPWSTR, cchBufferLength: DWORD): HANDLE => #extern
fn FindNextChangeNotification(hChangeHandle: HANDLE): BOOL => #extern
fn FindNextFileA(hFindFile: HANDLE, lpFindFileData: LPWIN32_FIND_DATAA): BOOL => #extern
fn FindNextFileW(hFindFile: HANDLE, lpFindFileData: LPWIN32_FIND_DATAW): BOOL => #extern
fn FindNextVolumeW(hFindVolume: HANDLE, lpszVolumeName: LPWSTR, cchBufferLength: DWORD): BOOL => #extern
fn FindVolumeClose(hFindVolume: HANDLE): BOOL => #extern
fn FlushFileBuffers(hFile: HANDLE): BOOL => #extern
fn GetDiskFreeSpaceA(lpRootPathName: LPCSTR, lpSectorsPerCluster: LPDWORD, lpBytesPerSector: LPDWORD, lpNumberOfFreeClusters: LPDWORD, lpTotalNumberOfClusters: LPDWORD): BOOL => #extern
fn GetDiskFreeSpaceW(lpRootPathName: LPCWSTR, lpSectorsPerCluster: LPDWORD, lpBytesPerSector: LPDWORD, lpNumberOfFreeClusters: LPDWORD, lpTotalNumberOfClusters: LPDWORD): BOOL => #extern
fn GetDiskFreeSpaceExA(lpDirectoryName: LPCSTR, lpFreeBytesAvailableToCaller: PULARGE_INTEGER, lpTotalNumberOfBytes: PULARGE_INTEGER, lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL => #extern
fn GetDiskFreeSpaceExW(lpDirectoryName: LPCWSTR, lpFreeBytesAvailableToCaller: PULARGE_INTEGER, lpTotalNumberOfBytes: PULARGE_INTEGER, lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL => #extern
fn GetDiskSpaceInformationA(rootPath: LPCSTR, diskSpaceInfo: *var DISK_SPACE_INFORMATION): HRESULT => #extern
fn GetDiskSpaceInformationW(rootPath: LPCWSTR, diskSpaceInfo: *var DISK_SPACE_INFORMATION): HRESULT => #extern
fn GetDriveTypeA(lpRootPathName: LPCSTR): UINT => #extern
fn GetDriveTypeW(lpRootPathName: LPCWSTR): UINT => #extern
fn GetFileAttributesA(lpFileName: LPCSTR): DWORD => #extern
fn GetFileAttributesW(lpFileName: LPCWSTR): DWORD => #extern
//fn GetFileAttributesExA(lpFileName: LPCSTR, fInfoLevelId: GET_FILEEX_INFO_LEVELS, lpFileInformation: LPVOID): BOOL => #extern
//fn GetFileAttributesExW(lpFileName: LPCWSTR, fInfoLevelId: GET_FILEEX_INFO_LEVELS, lpFileInformation: LPVOID): BOOL => #extern
fn GetFileInformationByHandle(hFile: HANDLE, lpFileInformation: LPBY_HANDLE_FILE_INFORMATION): BOOL => #extern
fn GetFileSize(hFile: HANDLE, lpFileSizeHigh: LPDWORD): DWORD => #extern
fn GetFileSizeEx(hFile: HANDLE, lpFileSize: PLARGE_INTEGER): BOOL => #extern
fn GetFileType(hFile: HANDLE): DWORD => #extern
fn GetFinalPathNameByHandleA(hFile: HANDLE, lpszFilePath: LPSTR, cchFilePath: DWORD, dwFlags: DWORD): DWORD => #extern
fn GetFinalPathNameByHandleW(hFile: HANDLE, lpszFilePath: LPWSTR, cchFilePath: DWORD, dwFlags: DWORD): DWORD => #extern
fn GetFileTime(hFile: HANDLE, lpCreationTime: LPFILETIME, lpLastAccessTime: LPFILETIME, lpLastWriteTime: LPFILETIME): BOOL => #extern
fn GetFullPathNameW(lpFileName: LPCWSTR, nBufferLength: DWORD, lpBuffer: LPWSTR, lpFilePart: *var LPWSTR): DWORD => #extern
fn GetFullPathNameA(lpFileName: LPCSTR, nBufferLength: DWORD, lpBuffer: LPSTR, lpFilePart: *var LPSTR): DWORD => #extern
fn GetLogicalDrives(): DWORD => #extern
fn GetLogicalDriveStringsW(nBufferLength: DWORD, lpBuffer: LPWSTR): DWORD => #extern
fn GetLongPathNameA(lpszShortPath: LPCSTR, lpszLongPath: LPSTR, cchBuffer: DWORD): DWORD => #extern
fn GetLongPathNameW(lpszShortPath: LPCWSTR, lpszLongPath: LPWSTR, cchBuffer: DWORD): DWORD => #extern
fn GetShortPathNameW(lpszLongPath: LPCWSTR, lpszShortPath: LPWSTR, cchBuffer: DWORD): DWORD => #extern
fn GetTempFileNameW(lpPathName: LPCWSTR, lpPrefixString: LPCWSTR, uUnique: UINT, lpTempFileName: LPWSTR): UINT => #extern
fn GetVolumeInformationByHandleW(hFile: HANDLE, lpVolumeNameBuffer: LPWSTR, nVolumeNameSize: DWORD, lpVolumeSerialNumber: LPDWORD, lpMaximumComponentLength: LPDWORD, lpFileSystemFlags: LPDWORD, lpFileSystemNameBuffer: LPWSTR, nFileSystemNameSize: DWORD): BOOL => #extern
fn GetVolumeInformationW(lpRootPathName: LPCWSTR, lpVolumeNameBuffer: LPWSTR, nVolumeNameSize: DWORD, lpVolumeSerialNumber: LPDWORD, lpMaximumComponentLength: LPDWORD, lpFileSystemFlags: LPDWORD, lpFileSystemNameBuffer: LPWSTR, nFileSystemNameSize: DWORD): BOOL => #extern
fn GetVolumePathNameW(lpszFileName: LPCWSTR, lpszVolumePathName: LPWSTR, cchBufferLength: DWORD): BOOL => #extern
fn LocalFileTimeToFileTime(lpLocalFileTime: *let FILETIME, lpFileTime: LPFILETIME): BOOL => #extern
fn LockFile(hFile: HANDLE, dwFileOffsetLow: DWORD, dwFileOffsetHigh: DWORD, nNumberOfBytesToLockLow: DWORD, nNumberOfBytesToLockHigh: DWORD): BOOL => #extern
fn LockFileEx(hFile: HANDLE, dwFlags: DWORD, dwReserved: DWORD, nNumberOfBytesToLockLow: DWORD, nNumberOfBytesToLockHigh: DWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
fn QueryDosDeviceW(lpDeviceName: LPCWSTR, lpTargetPath: LPWSTR, ucchMax: DWORD): DWORD => #extern
fn ReadFile(hFile: HANDLE, lpBuffer: LPVOID, nNumberOfBytesToRead: DWORD, lpNumberOfBytesRead: LPDWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
//fn ReadFileEx(hFile: HANDLE, lpBuffer: LPVOID, nNumberOfBytesToRead: DWORD, lpOverlapped: LPOVERLAPPED, lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL => #extern
//fn ReadFileScatter(hFile: HANDLE, aSegmentArray: *var FILE_SEGMENT_ELEMENT, nNumberOfBytesToRead: DWORD, lpReserved: LPDWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
fn RemoveDirectoryA(lpPathName: LPCSTR): BOOL => #extern
fn RemoveDirectoryW(lpPathName: LPCWSTR): BOOL => #extern
fn SetEndOfFile(hFile: HANDLE): BOOL => #extern
fn SetFileAttributesA(lpFileName: LPCSTR, dwFileAttributes: DWORD): BOOL => #extern
fn SetFileAttributesW(lpFileName: LPCWSTR, dwFileAttributes: DWORD): BOOL => #extern
//fn SetFileInformationByHandle(hFile: HANDLE, FileInformationClass: FILE_INFO_BY_HANDLE_CLASS, lpFileInformation: LPVOID, dwBufferSize: DWORD): BOOL => #extern
fn SetFilePointer(hFile: HANDLE, lDistanceToMove: LONG, lpDistanceToMoveHigh: PLONG, dwMoveMethod: DWORD): DWORD => #extern
fn SetFilePointerEx(hFile: HANDLE, liDistanceToMove: LARGE_INTEGER, lpNewFilePointer: PLARGE_INTEGER, dwMoveMethod: DWORD): BOOL => #extern
fn SetFileTime(hFile: HANDLE, lpCreationTime: *let FILETIME, lpLastAccessTime: *let FILETIME, lpLastWriteTime: *let FILETIME): BOOL => #extern
fn SetFileValidData(hFile: HANDLE, ValidDataLength: LONGLONG): BOOL => #extern
fn UnlockFile(hFile: HANDLE, dwFileOffsetLow: DWORD, dwFileOffsetHigh: DWORD, nNumberOfBytesToUnlockLow: DWORD, nNumberOfBytesToUnlockHigh: DWORD): BOOL => #extern
fn UnlockFileEx(hFile: HANDLE, dwReserved: DWORD, nNumberOfBytesToUnlockLow: DWORD, nNumberOfBytesToUnlockHigh: DWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
fn WriteFile(hFile: HANDLE, lpBuffer: LPCVOID, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: LPDWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
//fn WriteFileEx(hFile: HANDLE, lpBuffer: LPCVOID, nNumberOfBytesToWrite: DWORD, lpOverlapped: LPOVERLAPPED, lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL => #extern
//fn WriteFileGather(hFile: HANDLE, aSegmentArray: *var FILE_SEGMENT_ELEMENT, nNumberOfBytesToWrite: DWORD, lpReserved: LPDWORD, lpOverlapped: LPOVERLAPPED): BOOL => #extern
fn GetTempPathW(nBufferLength: DWORD, lpBuffer: LPWSTR): DWORD => #extern
fn GetVolumeNameForVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR, lpszVolumeName: LPWSTR, cchBufferLength: DWORD): BOOL => #extern
fn GetVolumePathNamesForVolumeNameW(lpszVolumeName: LPCWSTR, lpszVolumePathNames: LPWCH, cchBufferLength: DWORD, lpcchReturnLength: PDWORD): BOOL => #extern
fn CreateFile2(lpFileName: LPCWSTR, dwDesiredAccess: DWORD, dwShareMode: DWORD, dwCreationDisposition: DWORD, pCreateExParams: LPCREATEFILE2_EXTENDED_PARAMETERS): HANDLE => #extern
fn SetFileIoOverlappedRange(FileHandle: HANDLE, OverlappedRangeStart: PUCHAR, Length: ULONG): BOOL => #extern
fn GetCompressedFileSizeA(lpFileName: LPCSTR, lpFileSizeHigh: LPDWORD): DWORD => #extern
fn GetCompressedFileSizeW(lpFileName: LPCWSTR, lpFileSizeHigh: LPDWORD): DWORD => #extern
fn FindFirstStreamW(lpFileName: LPCWSTR, InfoLevel: STREAM_INFO_LEVELS, lpFindStreamData: LPVOID, dwFlags: DWORD): HANDLE => #extern
fn FindNextStreamW(hFindStream: HANDLE, lpFindStreamData: LPVOID): BOOL => #extern
fn AreFileApisANSI(): BOOL => #extern
fn GetTempPathA(nBufferLength: DWORD, lpBuffer: LPSTR): DWORD => #extern
fn FindFirstFileNameW(lpFileName: LPCWSTR, dwFlags: DWORD, StringLength: LPDWORD, LinkName: PWSTR): HANDLE => #extern
fn FindNextFileNameW(hFindStream: HANDLE, StringLength: LPDWORD, LinkName: PWSTR): BOOL => #extern
fn GetVolumeInformationA(lpRootPathName: LPCSTR, lpVolumeNameBuffer: LPSTR, nVolumeNameSize: DWORD, lpVolumeSerialNumber: LPDWORD, lpMaximumComponentLength: LPDWORD, lpFileSystemFlags: LPDWORD, lpFileSystemNameBuffer: LPSTR, nFileSystemNameSize: DWORD): BOOL => #extern
fn GetTempFileNameA(lpPathName: LPCSTR, lpPrefixString: LPCSTR, uUnique: UINT, lpTempFileName: LPSTR): UINT => #extern
fn SetFileApisToOEM(): VOID => #extern
fn SetFileApisToANSI(): VOID => #extern

//
// minwinbase.h
//

const SECURITY_ATTRIBUTES = struct {
    nLength: DWORD
    lpSecurityDescriptor: LPVOID
    bInheritHandle: BOOL
}
const PSECURITY_ATTRIBUTES = *var SECURITY_ATTRIBUTES
const LPSECURITY_ATTRIBUTES = *var SECURITY_ATTRIBUTES

const OVERLAPPED = struct {
    Internal:     ULONG_PTR
    InternalHigh: ULONG_PTR
    Pointer:      PVOID
    hEvent:       HANDLE
}
const LPOVERLAPPED = *var OVERLAPPED

const FILETIME = struct {
    dwLowDateTime:  DWORD
    dwHighDateTime: DWORD
}
const PFILETIME = *var FILETIME
const LPFILETIME = *var FILETIME

const WIN32_FIND_DATAA = struct {
    dwFileAttributes:   DWORD
    ftCreationTime:     FILETIME
    ftLastAccessTime:   FILETIME
    ftLastWriteTime:    FILETIME
    nFileSizeHigh:      DWORD
    nFileSizeLow:       DWORD
    dwReserved0:        DWORD
    dwReserved1:        DWORD
    cFileName:          [MAX_PATH]CHAR
    cAlternateFileName: [14]CHAR
}
const PWIN32_FIND_DATAA = *var WIN32_FIND_DATAA
const LPWIN32_FIND_DATAA = *var WIN32_FIND_DATAA

const WIN32_FIND_DATAW = struct {
    dwFileAttributes:   DWORD
    ftCreationTime:     FILETIME
    ftLastAccessTime:   FILETIME
    ftLastWriteTime:    FILETIME
    nFileSizeHigh:      DWORD
    nFileSizeLow:       DWORD
    dwReserved0:        DWORD
    dwReserved1:        DWORD
    cFileName:          [MAX_PATH]WCHAR
    cAlternateFileName: [14]WCHAR
}
const PWIN32_FIND_DATAW = *var WIN32_FIND_DATAW
const LPWIN32_FIND_DATAW = *var WIN32_FIND_DATAW

const FINDEX_INFO_LEVELS = S32
const FindExInfoStandard     = 0
const FindExInfoBasic        = 1
const FindExInfoMaxInfoLevel = 2