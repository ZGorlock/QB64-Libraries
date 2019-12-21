DECLARE DYNAMIC LIBRARY "user32"
    FUNCTION DefWindowProcA& (BYVAL hwnd AS LONG, BYVAL wMsg AS LONG, BYVAL wParam AS LONG, BYVAL lParam AS LONG)
    FUNCTION EnumDisplaySettingsA& (BYVAL lpszDeviceName AS _UNSIGNED _OFFSET, BYVAL iModeNum AS _UNSIGNED LONG, BYVAL lpDevMode AS _UNSIGNED _OFFSET)
    FUNCTION GetCursorPos (BYVAL lpPoint AS _OFFSET)
    FUNCTION GetDesktopWindow%& ()
    FUNCTION GetForegroundWindow%& ()
    FUNCTION GetKeyState% (BYVAL nVirtKey AS LONG)
    FUNCTION GetSystemMetrics (BYVAL nIndex AS _UNSIGNED LONG)
    FUNCTION OpenInputDesktop%& (BYVAL dwFlags AS _OFFSET, BYVAL fInherit AS _OFFSET, BYVAL dwDesiredAccess AS _OFFSET)
    FUNCTION SendMessageA& (BYVAL hwnd AS LONG, BYVAL wMsg AS LONG, BYVAL wParam AS LONG, BYVAL lParam AS LONG)
    FUNCTION SetThreadDesktop%& (BYVAL hDesktop AS _OFFSET)
    FUNCTION SystemParametersInfoW& (BYVAL uiAction AS _UNSIGNED LONG, BYVAL uiParam AS _UNSIGNED LONG, BYVAL pvParam AS _OFFSET, BYVAL fWinlni AS _UNSIGNED LONG)
END DECLARE
DECLARE LIBRARY
    FUNCTION GetAsyncKeyState (BYVAL vkey AS _UNSIGNED LONG)
END DECLARE

TYPE LPPOINT
    x AS _UNSIGNED LONG
    y AS _UNSIGNED LONG
END TYPE
TYPE RECT
    left AS LONG
    top AS LONG
    right AS LONG
    bottom AS LONG
END TYPE

'getinput variable dimensioning
DIM SHARED Alt AS _BIT '           alt key, -1 = down
DIM SHARED Ams AS _BIT '           alt + _/- key
DIM SHARED Apk AS _BIT '           application key
DIM SHARED Aps AS _BIT '           alt + =/+ key
DIM SHARED Bsp AS _BIT '           backspace char, -1=down
DIM SHARED Click AS _BIT '         if a mouse button is being pressed click = -1
DIM SHARED Clickchange AS _BIT '   if click status has changed = -1
DIM SHARED Clk AS _BIT '           clear key
DIM SHARED Ctrl AS _BIT '          control key, -1 = down
DIM SHARED Dlk AS _BIT '           delete key, -1=down
DIM SHARED Edk AS _BIT '           end key, -1=down
DIM SHARED Esc AS _BIT '           -1 if esc is pressed, should lead to terminating functions
DIM SHARED Exk AS _BIT '           execute key
DIM SHARED Fs AS _BIT '            change fs if you use _FULLSCREEN or _FULLSCREEN _OFF, 0=off, -1=fullscreen
DIM SHARED Getinputinit AS _BIT '  initiation flag
DIM SHARED Help AS _BIT '          use this to send a user to a help menu, -1= F1 pressed
DIM SHARED Hlk AS _BIT '           help key
DIM SHARED Hme AS _BIT '           home key, -1=down
DIM SHARED Ins AS _BIT '           insert key, -1=down
DIM SHARED Isinput AS _BIT '       if there is input isinput = -1
DIM SHARED Movemouse AS _BIT '     if the mouse has been moved = -1
DIM SHARED Nlk AS _BIT '           numlock key
DIM SHARED Ntk AS _BIT '           next track key
DIM SHARED oldClick AS _BIT '      the value of Click last loop
DIM SHARED Osc AS _BIT '           off screen click, a click or any mouse button, not necessarily on the program window
DIM SHARED Pak AS _BIT '           pause key
DIM SHARED Pdn AS _BIT '           page down key, -1=down
DIM SHARED Ppk AS _BIT '           play/pause media key
DIM SHARED Prk AS _BIT '           print key
DIM SHARED Psk AS _BIT '           print screen key
DIM SHARED Ptk AS _BIT '           previous track key
DIM SHARED Pup AS _BIT '           page up key, -1 = down
DIM SHARED Pw AS _BIT '            change pw if you show or hide the program window with _SCRENSHOW and _SCREENHIDE, 0=hidden, -1=shown
DIM SHARED Rtn AS _BIT '           enter char, -1= down
DIM SHARED Sbk AS _BIT '           start browser key
DIM SHARED Sft AS _BIT '           shift key, -1 = down
DIM SHARED Sle AS _BIT '           sleep key
DIM SHARED Slk AS _BIT '           scroll lock key
DIM SHARED Smk AS _BIT '           start mail key
DIM SHARED Spk AS _BIT '           stop media key
DIM SHARED Stk AS _BIT '           select key
DIM SHARED Tbc AS _BIT '           tab key, -1=down
DIM SHARED Vdk AS _BIT '           volume down key
DIM SHARED Vmk AS _BIT '           volume mute key
DIM SHARED Vuk AS _BIT '           volume up key
DIM SHARED Wnk AS _BIT '           windows key
DIM SHARED Zmk AS _BIT '           zoom key
DIM SHARED Act AS SINGLE '         TIMER of last activity
DIM SHARED Char AS SINGLE '        char, val=ASC(char)
DIM SHARED Day AS SINGLE '         day
DIM SHARED Hour AS SINGLE '        hour
DIM SHARED Keys AS SINGLE '        the number of keys() down at that moment >=0
DIM SHARED Minute AS SINGLE '      minute
DIM SHARED Month AS SINGLE '       month
DIM SHARED Num AS SINGLE '         number, val=ASC(number)
DIM SHARED Second AS SINGLE '      second
DIM SHARED T AS SINGLE '           TIMER
DIM SHARED Year AS SINGLE '        year
DIM SHARED Hwnd AS LONG '          program window handle, static
DIM SHARED Mouseswap AS LONG '     system metric
DIM SHARED Td AS DOUBLE '          TIMER(.01)
DIM SHARED Tf AS _FLOAT '          TIMER(.001)
DIM SHARED Dtwin AS _OFFSET '      desktop window handle, static
DIM SHARED K AS STRING '           CHR$() from INKEY$
DIM SHARED Programname AS STRING ' (same as your _TITLE()) MUST BE DEFINED AT THE START OF THE PROGRAM!
DIM SHARED Timestamp AS STRING '   %year%%month%%day%%hour%%minute%%second%

'getinput array dimensioning
REDIM SHARED Akey(4) AS _BIT '        directional keys, akey(1) = up, akey(2) = left, akey(3) = right, akey(4) = down
REDIM SHARED Alt(34) AS _BIT '        alt + letter charatcers
REDIM SHARED Ctrl(26) AS _BIT '       ctrl-X character, A-Z
REDIM SHARED Fkey(12) AS _BIT '       F1 through F12
REDIM SHARED Fkeya(12) AS _BIT '      F1 through F12 with alt held
REDIM SHARED Fkeyc(12) AS _BIT '      F1 through F12 with control held
REDIM SHARED Fkeys(12) AS _BIT '      F1 through F12 with shift held
REDIM SHARED Keys(256) AS _BIT '      -1=down, 0=not down, supports multiple keys, num of keys down can be obtained by the variable 'keys', http://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx collected even when program is not foreground window
REDIM SHARED Numa(10) AS _BIT '       number with alt down
REDIM SHARED Numc(4) AS _BIT '        numpad with ctrl key down

REDIM SHARED Locks(3) AS INTEGER '    capslock, numlock, scrolllock (0=off, 1=on, -127=was off now held, -128=was on now held)
REDIM SHARED Mousedata(10) AS SINGLE ' mousedata(1) = mouse x position on program window, mousedata(2) = mouse y position on program window, mousedata(3) = mouse x position on screen, mousedata(4) = mouse y position on screen, mousedata(5) = left click, -1=clicked, mousedata(6) = right click, -1 = clicked, mousedata(7) = middle click, -1 = clicked, mousedata(8) = scroll wheel, mousedata(9) = old mouse x on program, mousedata(10) = old mouse y on program
REDIM SHARED Screendim(4) AS SINGLE ' screendim(1) = screen resolution x, screendim(2) = screen resolution y, screendim(3) = screen work area x, screendim(4) = screen work area y