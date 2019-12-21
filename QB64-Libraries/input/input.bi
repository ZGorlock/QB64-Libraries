'******************************************************************************
'
'QB64 Input Library by Gorlock
'v0.3
'
'******************************************************************************
'
'Version 0.1  - 2013-10-29
'Version 0.2  - 2013-11-05
'  - added mouse functions
'  - made key functions assynchronous
'Version 0.3  - 2014-01-02
'  - added library for on screen keyboards
'
'******************************************************************************
'
'SUB       DRAWKEYBOARD      (handle%)                                             Draws a keyboard
'SUB       GETINPUT          ()                                                    Updates input variables
'SUB       GETMOUSEINPUT     ()                                                    Updates mouse input variables
'SUB       HIDEKEYBOARD      (handle%)                                             Hides a keyboard
'SUB       KEYBOARDFREE      (handle%)                                             Frees a keyboard handle
'SUB       KEYBOARDSETCOLOR  (handle%, frame~&, l~&, f~&, lh~&, fh~&, la~&, fa~&)  Defines the colors of a keyboard
'SUB       PUTKEYBOARD       (x&, y&, handle%, dest&)                              Moves a keyboard to a certain location and destination image
'SUB       SHOWKEYBOARD      (handle%)                                             Shows a keyboard that was hidden
'SUB       UPDATEHOTKEYS     ()                                                    Checks for hotkey presses
'FUNCTION  AKEY`             (x%%)                                                 Returns TRUE if Alt Key + a letter x is down
'FUNCTION  ALT`              ()                                                    Returns TRUE if the Alt Key is down
'FUNCTION  APK`              ()                                                    Returns TRUE if the Application Key is down
'FUNCTION  BSP`              ()                                                    Returns TRUE if the Backspace Key is down
'FUNCTION  CHAR~%%           ()                                                    Returns the ASCII value of a key hit if it is between 32 and 126
'FUNCTION  CKEY`             (x%%)                                                 Returns TRUE if Ctrl Key + a letter x is down
'FUNCTION  CLICK`            ()                                                    Returns TRUE if a mouse button is down
'FUNCTION  CLICKOLD`         ()                                                    Returns the value of CLICK last call
'FUNCTION  CLICKCHANGE`      ()                                                    Returns TRUE if CLICK does not equal CLICKOLD
'FUNCTION  CLK`              ()                                                    Returns TRUE if the Caps Lock Key is down
'FUNCTION  CLR`              ()                                                    Returns TRUE if the Clear Key is down
'FUNCTION  CTR`              ()                                                    Returns TRUE if the Ctrl Key is down
'FUNCTION  DKEY`             (x%%)                                                 Returns TRUE if Direction Key x is down
'FUNCTION  DLK`              ()                                                    Returns TRUE if the Delete Key is down
'FUNCTION  EDK`              ()                                                    Returns TRUE if the End Key is down
'FUNCTION  ESC`              ()                                                    Returns TRUE if the Escape Key is down
'FUNCTION  EXK`              ()                                                    Returns TRUE if the Execute Key is down
'FUNCTION  FKEY`             (x%%)                                                 Returns TRUE if Function Key x is down
'FUNCTION  FKEY_A`           (x%%)                                                 Returns TRUE if Alt Key + a Function Key x is down
'FUNCTION  FKEY_C`           (x%%)                                                 Returns TRUE if Ctrl Key + a Function Key x is down
'FUNCTION  FKEY_S`           (x%%)                                                 Returns TRUE if Shift Key + a Function Key x is down
'FUNCTION  HLK`              ()                                                    Returns TRUE if the Help Key is down
'FUNCTION  HME`              ()                                                    Returns TRUE if the Home Key is down
'FUNCTION  INS`              ()                                                    Returns TRUE if the Insert Key is down
'FUNCTION  ISINPUT`          ()                                                    Returns TRUE if there is input on the screen
'FUNCTION  KEYBOARDHIT%      (handle%)                                             Returns the value of a keyboard hit if any
'FUNCTION  KEYBOARDINPUT`    ()                                                    Returns TRUE if there is keyboard input
'FUNCTION  KEYBOARDNEW%      (layoutfile$, keywidth%, status%%)                    Creates a new keyboard
'FUNCTION  KEYBOARDSTATUS`   (handle%)                                             Returns the visibility status of a keyboard
'FUNCTION  KEYX`             (x~%%)                                                Returns the assynchronous status of virtual key x
'FUNCTION  LASTGETINPUT!     ()                                                    Returns the TIMER of the last call to GETINPUT
'FUNCTION  LASTINPUT~&       ()                                                    Returns the tick time of the last input according to the system
'FUNCTION  LCLICK`           ()                                                    Returns TRUE if the left mouse button is down
'FUNCTION  LCLICKOLD`        ()                                                    Returns the value of LCLICK last call
'FUNCTION  LKEY%%            (x%%)                                                 Returns a detailed query of Lock Key x
'FUNCTION  MCLICK`           ()                                                    Returns TRUE if the middle mouse button is down
'FUNCTION  MCLICKOLD`        ()                                                    Returns the value of MCLICK last call
'FUNCTION  MOUSESCREENX&     ()                                                    Returns the x coordinate of the mouse cursor on the screen
'FUNCTION  MOUSESCREENY&     ()                                                    Returns the y coordinate of the mouse cursor on the screen
'FUNCTION  MOUSEWHEEL%%      ()                                                    Returns the value of the mouse wheel multiplied by the scroll multiplier
'FUNCTION  MOUSEX&           ()                                                    Returns the x coordinate of the mouse cursor
'FUNCTION  MOUSEY&           ()                                                    Returns the y coordinate of the mouse cursor
'FUNCTION  MOVEMOUSE`        ()                                                    Tests if the mouse has moved since the last call
'FUNCTION  NLK`              ()                                                    Returns TRUE if the Num Lock Key is down
'FUNCTION  NTK`              ()                                                    Returns TRUE if the Next Track Key is down
'FUNCTION  NUM%%             ()                                                    Returns a number if it is being pressed, otherwise -1
'FUNCTION  NUM_A`            (x%%)                                                 Returns TRUE if Alt Key + a number x is down
'FUNCTION  OSC`              ()                                                    Returns TRUE if there is an on screen click
'FUNCTION  PAK`              ()                                                    Returns TRUE if the Pause Key is down
'FUNCTION  PAD_C`            (x%%)                                                 Returns TRUE if Ctrl Key + Keypad Key x is down
'FUNCTION  PDN`              ()                                                    Returns TRUE if the Page Down Key is down
'FUNCTION  PPK`              ()                                                    Returns TRUE if the Play/Pause Key is down
'FUNCTION  PRK`              ()                                                    Returns TRUE if the Print Key is down
'FUNCTION  PSK`              ()                                                    Returns TRUE if the Print Screen Key is down
'FUNCTION  PTK`              ()                                                    Returns TRUE if the Previous Track Key is down
'FUNCTION  PUP`              ()                                                    Returns TRUE if the Page Up Key is down
'FUNCTION  RCLICK`           ()                                                    Returns TRUE if the right mouse button is down
'FUNCTION  RCLICKOLD`        ()                                                    Returns the value of RCLICK last call
'FUNCTION  RTN`              ()                                                    Returns TRUE if the Return Key is down
'FUNCTION  SBK`              ()                                                    Returns TRUE if the Start Browser Key is down
'FUNCTION  SFT`              ()                                                    Returns TRUE if the Shift Key is down
'FUNCTION  SLE`              ()                                                    Returns TRUE if the Sleep Key is down
'FUNCTION  SLK`              ()                                                    Returns TRUE if the Scroll Lock Key is down
'FUNCTION  SMK`              ()                                                    Returns TRUE if the Start Mail Key is down
'FUNCTION  SPK`              ()                                                    Returns TRUE if the Stop Key is down
'FUNCTION  STK`              ()                                                    Returns TRUE if the Select Key is down
'FUNCTION  TBK`              ()                                                    Returns TRUE if the Tab Key is down
'FUNCTION  VALIDKEYBOARD`    (handle%)                                             Returns if a keyboard handle is valid or not
'FUNCTION  VDK`              ()                                                    Returns TRUE if the Volume Down Key is down
'FUNCTION  VMK`              ()                                                    Returns TRUE if the Volume Mute Key is down
'FUNCTION  VUK`              ()                                                    Returns TRUE if the Volume Up Key is down
'FUNCTION  WNK`              ()                                                    Returns TRUE if the Windows Key is down
'FUNCTION  ZMK`              ()                                                    Returns TRUE if the Zoom Key is down
'
'******************************************************************************
'
'Paste this code into your program as described in the documentation:
'https://db.tt/BXxHAtlA
'
'Include this code at the beginning of your program:
'https://db.tt/LAUsIW0c
'
'******************************************************************************
'
'Requires Gorlock's Common Library: https://db.tt/k9Tb6QJ4
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/jYaN3vRR
'
'******************************************************************************

SUB DRAWKEYBOARD (handle AS INTEGER)
DIM colorscheme AS _BYTE
DIM keywidth AS _BYTE
DIM board AS _UNSIGNED _BYTE
DIM drawkeyboardcol AS _UNSIGNED _BYTE
DIM drawkeyboardrow AS _UNSIGNED _BYTE
DIM getwidth AS _UNSIGNED _BYTE
DIM keyprint AS INTEGER
DIM keyspace AS INTEGER
SHARED keyboards() AS KEYBOARD
SHARED keyboardlayouts() AS INTEGER
SELECT CASE handle
    CASE KEYBOARD_ALLKEYBOARDS
        FOR handle = UBOUND(keyboards) TO 1 STEP -1
            DRAWKEYBOARD handle
        NEXT handle
    CASE ELSE
        IF keyboards(handle).shift = 1 THEN keyboards(handle).shift = 0
        IF SFT THEN keyboards(handle).shift = 1
        IF LKEY(LKEY_CAPS) = 1 THEN
            keyboards(handle).caps = 1
        ELSE IF LKEY(LKEY_CAPS) = -128 THEN
                keyboards(handle).caps = 0
            END IF
        END IF
        board = 1
        IF keyboards(handle).shift > 0 OR keyboards(handle).caps > 0 THEN board = 2
        IF keyboards(handle).dest THEN _DEST keyboards(handle).dest
        FOR drawkeyboardrow = 0 TO keyboards(handle).rows - 1
            FOR drawkeyboardcol = 0 TO keyboards(handle).cols - 1
                keyprint = keyboardlayouts(board, drawkeyboardrow + 1, drawkeyboardcol + 1, handle)
                IF keyprint THEN
                    keywidth = 1
                    FOR getwidth = drawkeyboardcol + 1 TO keyboards(handle).cols - 1
                        IF keyboardlayouts(board, drawkeyboardrow + 1, getwidth + 1, handle) = keyprint THEN
                            keywidth = keywidth + 1
                        ELSE
                            EXIT FOR
                        END IF
                    NEXT getwidth
                    LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth)-STEP(keywidth * keyboards(handle).keywidth, keyboards(handle).keywidth), keyboards(handle).framecolor, B
                    colorscheme = 0
                    IF isonbox(MInput.x, MInput.y, keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth, keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + (keywidth * keyboards(handle).keywidth - 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + (keyboards(handle).keywidth - 2)) THEN
                        IF LCLICK THEN
                            keyboards(handle).click = keyprint
                            colorscheme = 1
                        ELSE
                            IF keyboards(handle).click = keyprint THEN
                                SELECT CASE keyboards(handle).click
                                    CASE 256
                                        IF keyboards(handle).shift = 2 THEN
                                            keyboards(handle).shift = FALSE
                                        ELSE
                                            IF NOT SFT THEN keyboards(handle).shift = 2
                                        END IF
                                    CASE 257
                                        SELECT CASE keyboards(handle).caps
                                            CASE 0
                                                keyboards(handle).caps = 1
                                            CASE 1
                                                keyboards(handle).caps = 0
                                        END SELECT
                                    CASE ELSE
                                        keyboards(handle).hit = keyprint
                                        IF NOT SFT THEN keyboards(handle).shift = FALSE
                                END SELECT
                            END IF
                            keyboards(handle).click = FALSE
                            colorscheme = -1
                        END IF
                    END IF
                    SELECT CASE keyprint
                        CASE 39
                            IF _KEYDOWN(39) THEN colorscheme = 1
                        CASE 45
                            IF KEYX(189) THEN colorscheme = 1
                        CASE 46
                            IF KEYX(190) THEN colorscheme = 1
                        CASE 96
                            IF _KEYDOWN(96) THEN colorscheme = 1
                        CASE 97 TO 122
                            IF KEYX(keyprint - 32) THEN colorscheme = 1
                        CASE 256
                            IF keyboards(handle).shift THEN colorscheme = 1
                        CASE 257
                            IF keyboards(handle).caps THEN colorscheme = 1
                        CASE ELSE
                            IF KEYX(keyprint) THEN colorscheme = 1
                    END SELECT
                    keyspace = keywidth * keyboards(handle).keywidth
                    _PRINTMODE _KEEPBACKGROUND
                    SELECT CASE colorscheme
                        CASE -1
                            COLOR keyboards(handle).lettercolor_hover
                            LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + 1, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + 1)-STEP(keywidth * keyboards(handle).keywidth - 2, (keyboards(handle).keywidth - 2)), keyboards(handle).fillcolor_hover, BF
                        CASE 0
                            COLOR keyboards(handle).lettercolor
                            LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + 1, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + 1)-STEP(keywidth * keyboards(handle).keywidth - 2, (keyboards(handle).keywidth - 2)), keyboards(handle).fillcolor, BF
                        CASE 1
                            COLOR keyboards(handle).lettercolor_active
                            LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + 1, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + 1)-STEP(keywidth * keyboards(handle).keywidth - 2, (keyboards(handle).keywidth - 2)), keyboards(handle).fillcolor_active, BF
                    END SELECT
                    SELECT CASE keyprint
                        CASE 8
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - (2 * _FONTWIDTH)) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), "<-"
                        CASE 13
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - (6 * _FONTWIDTH)) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), "Return"
                        CASE 32
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - (5 * _FONTWIDTH)) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), "Space"
                        CASE 256
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - (5 * _FONTWIDTH)) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), "Shift"
                        CASE 257
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - (4 * _FONTWIDTH)) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), "Caps"
                        CASE ELSE
                            _PRINTSTRING (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + ((keyspace - _FONTWIDTH) \ 2), keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + ((keyboards(handle).keywidth - _FONTHEIGHT) \ 2)), CHR$(keyprint)
                    END SELECT
                    drawkeyboardcol = drawkeyboardcol + keywidth - 1
                ELSE
                    LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth)-STEP(keyboards(handle).keywidth, keyboards(handle).keywidth), keyboards(handle).framecolor, B
                    LINE (keyboards(handle).x + drawkeyboardcol * keyboards(handle).keywidth + 1, keyboards(handle).y + drawkeyboardrow * keyboards(handle).keywidth + 1)-STEP((keyboards(handle).keywidth - 2), (keyboards(handle).keywidth - 2)), keyboards(handle).fillcolor_hover, BF
                END IF
        NEXT drawkeyboardcol, drawkeyboardrow
        IF keyboards(handle).dest THEN _DEST 0
END SELECT
END SUB

SUB GETINPUT ()
InputTime = TIMER
K = INKEY$
IF K > "" THEN
    Keyd = ASC(K)
ELSE
    Keyd = 0
END IF
GETMOUSEINPUT
END SUB

SUB GETMOUSEINPUT ()
DIM LPP AS LPPOINT
MInput.oldx = MInput.x
MInput.oldy = MInput.y
MInput.oldscreenx = MInput.screenx
MInput.oldscreeny = MInput.screeny
MInput.oldlclick = MInput.lclick
MInput.oldrclick = MInput.rclick
MInput.oldmclick = MInput.mclick
MInput.oldclick = MInput.click
MInput.lclick = FALSE
MInput.rclick = FALSE
MInput.mclick = FALSE
MInput.click = FALSE
MInput.wheel = FALSE
DO
    MInput.x = _MOUSEX
    MInput.y = _MOUSEY
    SELECT CASE Mouseswap
        CASE 0
            IF _MOUSEBUTTON(1) THEN MInput.lclick = TRUE
            IF _MOUSEBUTTON(2) THEN MInput.rclick = TRUE
        CASE 1
            IF _MOUSEBUTTON(2) THEN MInput.lclick = TRUE
            IF _MOUSEBUTTON(1) THEN MInput.rclick = TRUE
    END SELECT
    IF _MOUSEBUTTON(3) THEN MInput.mclick = TRUE
    MInput.wheel = MInput.wheel + _MOUSEWHEEL
LOOP WHILE _MOUSEINPUT
GetCursorPos _OFFSET(LPP)
MInput.screenx = LPP.x
MInput.screeny = LPP.y
IF MInput.lclick = TRUE OR MInput.rclick = TRUE OR MInput.mclick = TRUE THEN MInput.click = TRUE
END SUB

SUB HIDEKEYBOARD (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
SELECT CASE handle
    CASE KEYBOARD_ALLKEYBOARDS
        FOR handle = UBOUND(keyboards) TO 1 STEP -1
            HIDEKEYBOARD handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
        keyboards(handle).status = KEYBOARD_HIDE
END SELECT
END SUB

SUB KEYBOARDFREE (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
SHARED keyboardlayouts() AS INTEGER
SELECT CASE handle
    CASE KEYBOARD_ALLKEYBOARDS
        FOR handle = UBOUND(keyboards) TO 1 STEP -1
            KEYBOARDFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
        IF handle = UBOUND(keyboards) AND handle > 1 THEN
            REDIM _PRESERVE keyboards(1 TO UBOUND(keyboards) - 1) AS KEYBOARD
            REDIM _PRESERVE keyboardlayouts(1 TO 2, 1 TO 128, 1 TO 128, 1 TO UBOUND(keyboardlayouts, 4) - 1) AS INTEGER
        ELSE
            keyboards(handle).inuse = FALSE
        END IF
END SELECT
END SUB

SUB KEYBOARDSETCOLOR (handle AS INTEGER, framecolor AS _UNSIGNED LONG, l AS _UNSIGNED LONG, f AS _UNSIGNED LONG, lh AS _UNSIGNED LONG, fh AS _UNSIGNED LONG, la AS _UNSIGNED LONG, fa AS _UNSIGNED LONG)
SHARED keyboards() AS KEYBOARD
SELECT CASE handle
    CASE KEYBOARD_ALLKEYBOARDS
        FOR handle = UBOUND(keyboards) TO 1 STEP -1
            KEYBOARDSETCOLOR handle, framecolor, l, f, lh, fh, la, fa
        NEXT handle
    CASE ELSE
        IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
        IF framecolor THEN keyboards(handle).framecolor = framecolor
        IF l THEN keyboards(handle).lettercolor = l
        IF f THEN keyboards(handle).fillcolor = f
        IF lh THEN keyboards(handle).lettercolor_hover = lh
        IF fh THEN keyboards(handle).fillcolor_hover = fh
        IF la THEN keyboards(handle).lettercolor_active = la
        IF fa THEN keyboards(handle).fillcolor_active = fa
END SELECT
END SUB

SUB PUTKEYBOARD (x AS LONG, y AS LONG, handle AS INTEGER, dest AS LONG)
SHARED keyboards() AS KEYBOARD
IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
keyboards(handle).x = x
keyboards(handle).y = y
IF dest THEN keyboards(handle).dest = dest
END SUB

SUB SHOWKEYBOARD (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
SELECT CASE handle
    CASE KEYBOARD_ALLKEYBOARDS
        FOR handle = UBOUND(keyboards) TO 1 STEP -1
            SHOWKEYBOARD handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
        keyboards(handle).status = KEYBOARD_SHOW
END SELECT
END SUB

SUB UPDATEHOTKEYS ()
IF FKEY(F1) THEN Hotkeys.help = TRUE
IF FKEY(F4) THEN Hotkeys.close = TRUE
IF FKEY(F7) THEN Hotkeys.screenshot = TRUE
IF FKEY(F8) THEN Hotkeys.windowshot = TRUE
IF FKEY(F11) THEN Hotkeys.fullscreen = TRUE
END SUB

FUNCTION AKEY` (x AS _BYTE)
IF x < AKEY_A OR x > AKEY_Z THEN EXIT FUNCTION
IF K = (Null + CHR$(15 + x)) THEN AKEY = TRUE
END FUNCTION

FUNCTION ALT` ()
IF KEYX(&H12) THEN ALT = TRUE
END FUNCTION

FUNCTION APK` ()
IF KEYX(&H5D) THEN APK = TRUE
END FUNCTION

FUNCTION BSP` ()
IF KEYX(&H8) THEN BSP = TRUE
END FUNCTION

FUNCTION CHAR~%% ()
IF Keyd > 31 AND Keyd < 127 THEN CHAR = Keyd
END FUNCTION

FUNCTION CKEY` (x AS _BYTE)
IF x < CKEY_A OR x > CKEY_Z THEN EXIT FUNCTION
IF Keyd = x THEN CKEY = TRUE
END FUNCTION

FUNCTION CLICK` ()
CLICK = (MInput.click = TRUE)
END FUNCTION

FUNCTION CLICKOLD` ()
CLICKOLD = (MInput.oldclick = TRUE)
END FUNCTION

FUNCTION CLICKCHANGE` ()
CLICKCHANGE = (MInput.click <> MInput.oldclick)
END FUNCTION

FUNCTION CLK` ()
IF KEYX(&H14) THEN CLK = TRUE
END FUNCTION

FUNCTION CLR` ()
IF KEYX(&HC) THEN CLR = TRUE
END FUNCTION

FUNCTION CTR` ()
IF KEYX(&H11) THEN CTR = TRUE
END FUNCTION

FUNCTION DKEY` (x AS _BYTE)
SELECT CASE x
    CASE DKEY_UP
        IF _KEYDOWN(18432) THEN DKEY = TRUE
    CASE DKEY_LEFT
        IF _KEYDOWN(19200) THEN DKEY = TRUE
    CASE DKEY_RIGHT
        IF _KEYDOWN(19712) THEN DKEY = TRUE
    CASE DKEY_DOWN
        IF _KEYDOWN(20480) THEN DKEY = TRUE
END SELECT
END FUNCTION

FUNCTION DLK` ()
IF KEYX(&H2E) THEN DLK = TRUE
END FUNCTION

FUNCTION EDK` ()
IF KEYX(&H23) THEN EDK = TRUE
END FUNCTION

FUNCTION ESC` ()
IF KEYX(&H1B) THEN ESC = TRUE
END FUNCTION

FUNCTION EXK` ()
IF KEYX(&H2B) THEN EXK = TRUE
END FUNCTION

FUNCTION FKEY` (x AS _BYTE)
IF x < F1 OR x > F12 THEN EXIT FUNCTION
SELECT CASE x
    CASE IS < F11
        IF K = (Null + CHR$(58 + x)) THEN FKEY = TRUE
    CASE F11
        IF K = (Null + CHR$(133)) THEN FKEY = TRUE
    CASE F12
        IF K = (Null + CHR$(134)) THEN FKEY = TRUE
END SELECT
END FUNCTION

FUNCTION FKEY_A` (x AS _BYTE)
IF x < F1 OR x > F12 THEN EXIT FUNCTION
SELECT CASE x
    CASE IS < F11
        IF K = (Null + CHR$(103 + x)) THEN FKEY_A = TRUE
    CASE F11
        IF K = (Null + CHR$(139)) THEN FKEY_A = TRUE
    CASE F12
        IF K = (Null + CHR$(140)) THEN FKEY_A = TRUE
END SELECT
END FUNCTION

FUNCTION FKEY_C` (x AS _BYTE)
IF x < F1 OR x > F12 THEN EXIT FUNCTION
SELECT CASE x
    CASE IS < F11
        IF K = (Null + CHR$(93 + x)) THEN FKEY_C = TRUE
    CASE F11
        IF K = (Null + CHR$(137)) THEN FKEY_C = TRUE
    CASE F12
        IF K = (Null + CHR$(138)) THEN FKEY_C = TRUE
END SELECT
END FUNCTION

FUNCTION FKEY_S` (x AS _BYTE)
IF x < F1 OR x > F12 THEN EXIT FUNCTION
SELECT CASE x
    CASE IS < F11
        IF K = (Null + CHR$(83 + x)) THEN FKEY_S = TRUE
    CASE F11
        IF K = (Null + CHR$(135)) THEN FKEY_S = TRUE
    CASE F12
        IF K = (Null + CHR$(136)) THEN FKEY_S = TRUE
END SELECT
END FUNCTION

FUNCTION HLK` ()
IF KEYX(&H27) THEN HLK = TRUE
END FUNCTION

FUNCTION HME` ()
IF KEYX(&H24) THEN HME = TRUE
END FUNCTION

FUNCTION INS` ()
IF KEYX(&H2D) THEN INS = TRUE
END FUNCTION

FUNCTION ISINPUT` ()
ISINPUT = (Keyd OR MOVEMOUSE OR CLICK)
END FUNCTION

FUNCTION KEYBOARDHIT% (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
KEYBOARDHIT = keyboards(handle).hit
keyboards(handle).hit = FALSE
END FUNCTION

FUNCTION KEYBOARDINPUT` ()
KEYBOARDINPUT = (Keyd > 0)
END FUNCTION

FUNCTION KEYBOARDNEW% (layoutfile AS STRING, keywidth AS INTEGER, status AS _BYTE)
DIM getkeyboardrow AS _BYTE
DIM ripkeyboard AS _BYTE
DIM findfreekeyboard AS INTEGER
DIM n AS INTEGER
DIM a(256) AS STRING
SHARED keyboards() AS KEYBOARD
SHARED keyboardlayouts() AS INTEGER
IF NOT _FILEEXISTS(layoutfile) THEN EXIT FUNCTION
IF keywidth = 0 THEN keywidth = KEYBOARD_DEFAULTKEYWIDTH
IF status > KEYBOARD_HIDE THEN status = KEYBOARD_HIDE
IF status < KEYBOARD_SHOW THEN status = KEYBOARD_SHOW
FOR findfreekeyboard = 1 TO UBOUND(keyboards)
    IF keyboards(findfreekeyboard).inuse = FALSE THEN
        KEYBOARDNEW = findfreekeyboard
        EXIT FOR
    END IF
NEXT findfreekeyboard
IF KEYBOARDNEW = 0 THEN
    REDIM _PRESERVE keyboards(1 TO UBOUND(keyboards) + 1) AS KEYBOARD
    REDIM _PRESERVE keyboardlayouts(1 TO 2, 1 TO 128, 1 TO 128, 1 TO UBOUND(keyboardlayouts, 4) + 1) AS INTEGER
    KEYBOARDNEW = UBOUND(keyboards)
END IF
keyboards(KEYBOARDNEW).inuse = TRUE
keyboards(KEYBOARDNEW).status = status
keyboards(KEYBOARDNEW).x = 0
keyboards(KEYBOARDNEW).y = 0
keyboards(KEYBOARDNEW).hit = FALSE
keyboards(KEYBOARDNEW).framecolor = KEYBOARD_DEFAULTCOLOR_FRAME
keyboards(KEYBOARDNEW).lettercolor = KEYBOARD_DEFAULTCOLOR_LETTER
keyboards(KEYBOARDNEW).fillcolor = KEYBOARD_DEFAULTCOLOR_FILL
keyboards(KEYBOARDNEW).lettercolor_hover = KEYBOARD_DEFAULTCOLOR_LETTER_HOVER
keyboards(KEYBOARDNEW).fillcolor_hover = KEYBOARD_DEFAULTCOLOR_FILL_HOVER
keyboards(KEYBOARDNEW).lettercolor_active = KEYBOARD_DEFAULTCOLOR_LETTER_ACTIVE
keyboards(KEYBOARDNEW).fillcolor_active = KEYBOARD_DEFAULTCOLOR_FILL_ACTIVE
keyboards(KEYBOARDNEW).keywidth = keywidth
keyboards(KEYBOARDNEW).dest = 0
OPEN layoutfile FOR INPUT AS #1
DO
    n = n + 1
    LINE INPUT #1, a(n)
LOOP UNTIL EOF(1)
CLOSE #1
keyboards(KEYBOARDNEW).rows = n \ 2
keyboards(KEYBOARDNEW).cols = LEN(a(1)) \ 3
FOR getkeyboardrow = 1 TO keyboards(KEYBOARDNEW).rows
    FOR ripkeyboardkey = 1 TO keyboards(KEYBOARDNEW).cols
        keyboardlayouts(1, getkeyboardrow, ripkeyboardkey, KEYBOARDNEW) = VAL(MID$(a(getkeyboardrow), (ripkeyboardkey - 1) * 3 + 1, 3))
    NEXT ripkeyboardkey
NEXT getkeyboardrow
FOR getkeyboardrow = 1 TO keyboards(KEYBOARDNEW).rows
    FOR ripkeyboardkey = 1 TO keyboards(KEYBOARDNEW).cols
        keyboardlayouts(2, getkeyboardrow, ripkeyboardkey, KEYBOARDNEW) = VAL(MID$(a(getkeyboardrow + keyboards(KEYBOARDNEW).rows), (ripkeyboardkey - 1) * 3 + 1, 3))
    NEXT ripkeyboardkey
NEXT getkeyboardrow
keyboards(KEYBOARDNEW).width = keyboards(KEYBOARDNEW).cols * keyboards(KEYBOARDNEW).keywidth
keyboards(KEYBOARDNEW).height = keyboards(KEYBOARDNEW).rows * keyboards(KEYBOARDNEW).keywidth
END FUNCTION

FUNCTION KEYBOARDSTATUS` (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
IF NOT VALIDKEYBOARD(handle) THEN EXIT FUNCTION
KEYBOARDSTATUS = keyboards(handle).status
END FUNCTION

FUNCTION KEYX` (x AS _UNSIGNED _BYTE)
IF GetAsyncKeyState(x) THEN KEYX = TRUE
END FUNCTION

FUNCTION LASTGETINPUT! ()
LASTGETINPUT = InputTime
END FUNCTION

FUNCTION LASTINPUT~& ()
GetLastInputInfo _OFFSET(LastInputData)
LASTINPUT = LastInputData.dwTime
END FUNCTION

FUNCTION LCLICK` ()
LCLICK = (MInput.lclick = TRUE)
END FUNCTION

FUNCTION LCLICKOLD` ()
LCLICKOLD = (MInput.oldlclick = TRUE)
END FUNCTION

FUNCTION LKEY%% (x AS _BYTE)
SELECT CASE x
    CASE LKEY_CAPS
        LKEY = GetKeyState(&H14)
    CASE LKEY_NUM
        LKEY = GetKeyState(&H90)
    CASE LKEY_SCROLL
        LKEY = GetKeyState(&H91)
END SELECT
END FUNCTION

FUNCTION MCLICK` ()
MCLICK = (MInput.mclick = TRUE)
END FUNCTION

FUNCTION MCLICKOLD` ()
MCLICKOLD = (MInput.oldmclick = TRUE)
END FUNCTION

FUNCTION MOUSESCREENX& ()
MOUSESCREENX = MInput.screenx
END FUNCTION

FUNCTION MOUSESCREENY& ()
MOUSESCREENY = MInput.screeny
END FUNCTION

FUNCTION MOUSEWHEEL%% ()
MOUSEWHEEL = MInput.wheel * ScrollMultiplier
END FUNCTION

FUNCTION MOUSEX& ()
MOUSEX = MInput.x
END FUNCTION

FUNCTION MOUSEY& ()
MOUSEY = MInput.y
END FUNCTION

FUNCTION MOVEMOUSE` ()
MOVEMOUSE = (MInput.screenx <> MInput.oldscreenx OR MInput.screeny <> MInput.oldscreeny)
END FUNCTION

FUNCTION NLK` ()
IF KEYX(&H90) THEN NLK = TRUE
END FUNCTION

FUNCTION NTK` ()
IF KEYX(&HB0) THEN NTK = TRUE
END FUNCTION

FUNCTION NUM%% ()
NUM = -1
IF Keyd > 47 AND Keyd < 58 THEN NUM = Keyd - 48
END FUNCTION

FUNCTION NUM_A` (x AS _BYTE)
IF x < 0 OR x > 9 THEN EXIT FUNCTION
IF K = (Null + CHR$(120 + x)) THEN NUM_A = TRUE
END FUNCTION

FUNCTION OSC` ()
IF KEYX(&H1) OR KEYX(&H2) OR KEYX(&H3) THEN OSC = TRUE
END FUNCTION

FUNCTION PAK` ()
IF KEYX(&H13) THEN PAK = TRUE
END FUNCTION

FUNCTION PAD_C` (x AS _BYTE)
IF x < 1 OR x > 6 THEN EXIT FUNCTION
IF K = (Null + CHR$(113 + x)) THEN PAD_C = TRUE
END FUNCTION

FUNCTION PDN` ()
IF KEYX(&H22) THEN PDN = TRUE
END FUNCTION

FUNCTION PPK` ()
IF KEYX(&HB3) THEN PPK = TRUE
END FUNCTION

FUNCTION PRK` ()
IF KEYX(&H2A) THEN PRK = TRUE
END FUNCTION

FUNCTION PSK` ()
IF KEYX(&H2C) THEN PSK = TRUE
END FUNCTION

FUNCTION PTK` ()
IF KEYX(&HB1) THEN PTK = TRUE
END FUNCTION

FUNCTION PUP` ()
IF KEYX(&H21) THEN PUP = TRUE
END FUNCTION

FUNCTION RCLICK` ()
RCLICK = (MInput.rclick = TRUE)
END FUNCTION

FUNCTION RCLICKOLD` ()
RCLICKOLD = (MInput.oldrclick = TRUE)
END FUNCTION

FUNCTION RTN` ()
IF KEYX(&HD) THEN RTN = TRUE
END FUNCTION

FUNCTION SBK` ()
IF KEYX(&HAC) THEN SBK = TRUE
END FUNCTION

FUNCTION SFT` ()
IF KEYX(&H10) THEN SFT = TRUE
END FUNCTION

FUNCTION SLE` ()
IF KEYX(&H5F) THEN SLE = TRUE
END FUNCTION

FUNCTION SLK` ()
IF KEYX(&H91) THEN SLK = TRUE
END FUNCTION

FUNCTION SMK` ()
IF KEYX(&HB4) THEN SMK = TRUE
END FUNCTION

FUNCTION SPK` ()
IF KEYX(&HB2) THEN SPK = TRUE
END FUNCTION

FUNCTION STK` ()
IF KEYX(&H2A) THEN STK = TRUE
END FUNCTION

FUNCTION TBK` ()
IF KEYX(&H9) THEN TBK = TRUE
END FUNCTION

FUNCTION VALIDKEYBOARD` (handle AS INTEGER)
SHARED keyboards() AS KEYBOARD
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(keyboards) THEN EXIT FUNCTION
IF keyboards(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDKEYBOARD = TRUE
END FUNCTION

FUNCTION VDK` ()
IF KEYX(&HAE) THEN VDK = TRUE
END FUNCTION

FUNCTION VMK` ()
IF KEYX(&HAD) THEN VMK = TRUE
END FUNCTION

FUNCTION VUK` ()
IF KEYX(&HAF) THEN VUK = TRUE
END FUNCTION

FUNCTION WNK` ()
IF KEYX(&H5B) OR KEYX(&H5C) THEN WNK = TRUE
END FUNCTION

FUNCTION ZMK` ()
IF KEYX(&HFB) THEN ZMK = TRUE
END FUNCTION