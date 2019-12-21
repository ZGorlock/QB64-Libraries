'collects input of various forms
'precondition: required libraries are set, required variables are dimensioned and shared, programname$ is set properly
'note: when getinputinit = 0, getinput initates and collects data that doesn't need to be collected at each call
'note: look at the getinput variable dimensioning to see the descriptions of the variables
SUB getinput
IF Getinputinit = 0 THEN
    DIM resolution AS LONG
    DIM titletag AS LONG
    DIM oidesk AS _OFFSET
    DIM stdesk AS _OFFSET
    DIM LPP AS LPPOINT
    DIM Rec AS RECT
    resolution = _SCREENIMAGE
    Screendim(1) = _WIDTH(resolution)
    Screendim(2) = _HEIGHT(resolution)
    IF SystemParametersInfoW&(SPI_GETWORKAREA, 0, _OFFSET(Rec), 0) <> 0 THEN
        Screendim(3) = Rec.right
        Screendim(4) = Rec.bottom
    END IF
    Dtwin = GetDesktopWindow
    oidesk = OpenInputDesktop(0, 0, GENERIC_ALL)
    IF oidesk <> Dtwin AND oidesk > 0 THEN stdesk = SetThreadDesktop(oidesk)
    titletag = RND * &H1000000
    _TITLE Programname + " - " + HEX$(titletag)
    _TITLE Programname
    IF SendMessageA(Hwnd, WM_SETHOTKEY, VK_F4, 0) = 1 THEN bringtotop& = DefWindowProcA(Hwnd, WM_SHOWWINDOW, 0, 0)
    Mouseswap = GetSystemMetrics(23)
    Getinputinit = -1
END IF
DIM storescroll AS _BYTE
DIM altfind AS _UNSIGNED _BYTE
DIM anumfind AS _UNSIGNED _BYTE
DIM cnumfind AS _UNSIGNED _BYTE
DIM ctrlchar AS _UNSIGNED _BYTE
DIM fkeyfind AS _UNSIGNED _BYTE
DIM fkeyafind AS _UNSIGNED _BYTE
DIM fkeycfind AS _UNSIGNED _BYTE
DIM fkeysfind AS _UNSIGNED _BYTE
DIM thekey AS _UNSIGNED _BYTE
DIM saveoldx AS SINGLE
DIM saveoldy AS SINGLE
DIM getmousepos AS LONG
DIM stampday AS STRING
DIM stamphour AS STRING
DIM stampminute AS STRING
DIM stampmonth AS STRING
DIM stampsecond AS STRING
DIM stampyear AS STRING
REDIM Akey(4) AS _BIT
REDIM Alt(34) AS _BIT
REDIM Ctrl(26) AS _BIT
REDIM Fkey(12) AS _BIT
REDIM Fkeya(12) AS _BIT
REDIM Fkeyc(12) AS _BIT
REDIM Fkeys(12) AS _BIT
REDIM Keys(256) AS _BIT
REDIM Numc(6) AS _BIT
REDIM Locks(3) AS INTEGER
Alt = 0
Ams = 0
Apk = 0
Aps = 0
Bsp = 0
Char = 0
Clk = 0
Ctrl = 0
Dlk = 0
Edk = 0
Esc = 0
Exk = 0
Help = 0
Hlk = 0
Hme = 0
Ins = 0
Keys = 0
Nlk = 0
Ntk = 0
Num = 0
Osc = 0
Pak = 0
Pdn = 0
Ppk = 0
Prk = 0
Psk = 0
Ptk = 0
Pup = 0
Rtn = 0
Sbk = 0
Sft = 0
Sle = 0
Slk = 0
Smk = 0
Spk = 0
Stk = 0
Tbc = 0
Vdk = 0
Vmk = 0
Vuk = 0
Wnk = 0
Zmk = 0
T = TIMER
Td = TIMER(.01)
Tf = TIMER(.001)
stampmonth = LEFT$(DATE$, 2)
Month = VAL(stampmonth)
stampday = MID$(DATE$, 4, 2)
Day = VAL(stampday)
stampyear = RIGHT$(DATE$, 4)
Year = VAL(stampyear)
stamphour = LEFT$(TIME$, 2)
Hour = VAL(stamphour)
stampminute = MID$(TIME$, 4, 2)
Minute = VAL(stampminute)
stampsecond = RIGHT$(TIME$, 2)
Second = VAL(stampsecond)
Timestamp = stampyear + stampmonth + stampday + stamphour + stampminute + stampsecond
K = INKEY$
IF K > "" THEN
    keyd = ASC(K)
    Act = TIMER
END IF
FOR ctrlchar = 1 TO 26
    IF keyd = ctrlchar THEN Ctrl(ctrlchar) = -1
NEXT ctrlchar
IF keyd = 8 THEN Bsp = -1
IF keyd = 9 THEN Tbc = -1
IF keyd = 13 THEN Rtn = -1
IF keyd = 27 THEN Esc = -1
IF keyd >= 48 AND keyd <= 57 THEN Num = keyd
IF keyd >= 32 AND keyd <= 126 THEN Char = keyd
FOR altfind = 1 TO 34
    IF K = (CHR$(0) + CHR$((15 + altfind))) THEN Alt(altfind) = -1
NEXT altfind
FOR fkeyfind = 1 TO 10
    IF K = (CHR$(0) + CHR$((58 + fkeyfind))) THEN Fkey(fkeyfind) = -1
NEXT fkeyfind
IF K = (CHR$(0) + CHR$(59)) THEN Help = -1
IF K = (CHR$(0) + CHR$(61)) THEN
    SELECT CASE Pw
        CASE -1
            _SCREENHIDE
            Pw = 0
        CASE 0
            _SCREENSHOW
            Pw = -1
    END SELECT
END IF
IF K = (CHR$(0) + CHR$(71)) THEN Hme = -1
IF K = (CHR$(0) + CHR$(73)) THEN Pup = -1
IF K = (CHR$(0) + CHR$(79)) THEN Edk = -1
IF K = (CHR$(0) + CHR$(81)) THEN Pdn = -1
IF K = (CHR$(0) + CHR$(82)) THEN Ins = -1
IF K = (CHR$(0) + CHR$(83)) THEN Dlk = -1
Akey(1) = _KEYDOWN(18432)
Akey(2) = _KEYDOWN(19200)
Akey(3) = _KEYDOWN(19712)
Akey(4) = _KEYDOWN(20480)
FOR fkeysfind = 1 TO 10
    IF K = (CHR$(0) + CHR$(83 + fkeysfind)) THEN Fkeys(fkeysfind) = -1
NEXT fkeysfind
FOR fkeycfind = 1 TO 10
    IF K = (CHR$(0) + CHR$(93 + fkeycfind)) THEN Fkeyc(fkeycfind) = -1
NEXT fkeycfind
FOR fkeyafind = 1 TO 10
    IF K = (CHR$(0) + CHR$(103 + fkeyafind)) THEN Fkeya(fkeyafind) = -1
NEXT fkeyafind
FOR cnumfind = 1 TO 6
    IF K = (CHR$(0) + CHR$((113 + cnumfind))) THEN Numc(cnumfind) = -1
NEXT cnumfind
FOR anumfind = 1 TO 10
    IF K = (CHR$(0) + CHR$((119 + anumfind))) THEN Numa(anumfind) = -1
NEXT anumfind
IF K = (CHR$(0) + CHR$(130)) THEN Ams = -1
IF K = (CHR$(0) + CHR$(130)) THEN Aps = -1
IF K = (CHR$(0) + CHR$(133)) THEN
    Fkey(11) = -1
    SELECT CASE Fs
        CASE -1
            _FULLSCREEN _OFF
            Fs = 0
        CASE 0
            _FULLSCREEN
            Fs = -1
    END SELECT
END IF
IF K = (CHR$(0) + CHR$(134)) THEN Fkey(12) = -1
IF K = (CHR$(0) + CHR$(135)) THEN Fkeys(11) = -1
IF K = (CHR$(0) + CHR$(136)) THEN Fkeys(12) = -1
IF K = (CHR$(0) + CHR$(137)) THEN Fkeyc(11) = -1
IF K = (CHR$(0) + CHR$(138)) THEN Fkeyc(12) = -1
IF K = (CHR$(0) + CHR$(139)) THEN Fkeya(11) = -1
IF K = (CHR$(0) + CHR$(140)) THEN Fkeya(12) = -1
FOR thekey = &H01 TO &HFE
    IF GetAsyncKeyState(thekey) THEN
        Keys = Keys + 1
        Keys(thekey) = -1
    END IF
NEXT
IF Keys(1) OR Keys(2) OR Keys(3) THEN Osc = -1
IF Keys(12) THEN Clk = -1
IF Keys(16) THEN Sft = -1
IF Keys(17) THEN Ctrl = -1
IF Keys(18) THEN Alt = -1
IF Keys(19) THEN Pak = -1
IF Keys(41) THEN Stk = -1
IF Keys(42) THEN Prk = -1
IF Keys(43) THEN Exk = -1
IF Keys(44) THEN Psk = -1
IF Keys(47) THEN Hlk = -1
IF Keys(91) OR Keys(92) THEN Wnk = -1
IF Keys(93) THEN Apk = -1
IF Keys(95) THEN Sle = -1
IF Keys(144) THEN Nlk = -1
IF Keys(145) THEN Slk = -1
IF Keys(172) THEN Sbk = -1
IF Keys(173) THEN Vmk = -1
IF Keys(174) THEN Vdk = -1
IF Keys(175) THEN Vuk = -1
IF Keys(176) THEN Ntk = -1
IF Keys(177) THEN Ptk = -1
IF Keys(178) THEN Spk = -1
IF Keys(179) THEN Ppk = -1
IF Keys(180) THEN Smk = -1
IF Keys(251) THEN Zmk = -1
Locks(1) = GetKeyState(VK_CAPITAL)
Locks(2) = GetKeyState(VK_NUMLOCK)
Locks(3) = GetKeyState(VK_SCROLL)
Mousedata(8) = 0
saveoldx = Mousedata(1)
saveoldy = Mousedata(2)
DO WHILE _MOUSEINPUT
    storescroll = Mousedata(8)
    REDIM Mousedata(10) AS SINGLE
    Mousedata(1) = _MOUSEX
    Mousedata(2) = _MOUSEY
    getmousepos = GetCursorPos(_OFFSET(LPP))
    Mousedata(3) = LPP.x
    Mousedata(4) = LPP.y
    SELECT CASE Mouseswap
        CASE 0
            IF _MOUSEBUTTON(1) THEN Mousedata(5) = -1
            IF _MOUSEBUTTON(2) THEN Mousedata(6) = -1
        CASE ELSE
            IF _MOUSEBUTTON(2) THEN Mousedata(5) = -1
            IF _MOUSEBUTTON(1) THEN Mousedata(6) = -1
    END SELECT
    IF _MOUSEBUTTON(3) THEN Mousedata(7) = -1
    Mousedata(8) = storescroll + _MOUSEWHEEL
LOOP
Mousedata(9) = saveoldx
Mousedata(10) = saveoldy
Movemouse = (Mousedata(1) <> Mousedata(9) OR Mousedata(2) <> Mousedata(10))
oldClick = Click
Click = (Mousedata(5) OR Mousedata(6) OR Mousedata(7))
Clickchange = (Click <> oldClick)
Isinput = (Keys OR Movemouse OR Click)
END SUB

'determines whether a point is on a box or not
'parameter: the x coordinate of the point
'parameter: the y coordinate of the point
'parameter: the x coordinate of the upper left corner of box
'parameter: the y coordinate of the upper left corner of box
'parameter: the x coordinate of the lower right corner of box
'parameter: the y coordinate of the lower right corner of box
'return: whether the point is on it or not
FUNCTION isonbox` (x AS LONG, y AS LONG, x1 AS LONG, y1 AS LONG, x2 AS LONG, y2 AS LONG)
IF x < x1 THEN EXIT FUNCTION
IF x > x2 THEN EXIT FUNCTION
IF y < y1 THEN EXIT FUNCTION
IF y > y2 THEN EXIT FUNCTION
isonbox = -1
END FUNCTION