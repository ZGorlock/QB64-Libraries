'******************************************************************************
'
'QB64 Common Library by Gorlock
'v0.5
'
'******************************************************************************
'
'Version 0.1  - 2013-10-04
'Version 0.11 - 2013-10-04
'  - added tutorial and init include file
'Version 0.12 - 2013-10-07
'  - added UnicodeToANSI function
'Version 0.13 - 2013-10-10
'  - added zeroset function
'Version 0.2  - 2013-10-27
'  - added SCREENSHOT, resetfile, Isfg and rangepick
'  - added windows version get
'  - added screen dimension variables
'  - added windows handles
'Version 0.21 - 2013-11-01
'  - added FULLSCREEN and HIDESCREEN
'  - fixed bug in SCREENSHOT
'Version 0.3  - 2013-11-02
'  - added TSTAMP function
'  - added single instance checking, errorhandler, and error log
'Version 0.31 - 2013-11-18
'  - added SaveBMP subroutine
'  - updated SCREENSHOT function
'  - fixed minor errors
'Version 0.4  - 2013-12-20
'  - added BaseConv and URLCASE functions
'Version 0.5  - 2013-02-15
'  - added RECTcollide function
'
'******************************************************************************
'
'SUB       FULLSCREEN       ()                                                Toggles console fullscreen
'SUB       HIDESCREEN       ()                                                Toggles console hidescreen
'SUB       MAPRECT          (s&, sl&, st&, sr&, sb&, d&, dl&, dt&, dr&, db&)  Maps rectangle (sl&, st&, sr&, sb&) from image source s& to rectangle (dl&, dt&, dr&, db&) from image destination d&
'SUB       resetfile        (filename$)                                       Empties a file
'SUB       SaveBMP          (x1%, y1%, x2%, y2%, image&, filename$)           Saves an image area to a BMP file
'SUB       SCREENSHOT       (region%%, fileloc$, filename$)                   Creates a screenshot to the file specified
'FUNCTION  BaseConv$        (basei%, basef%, n$)                              Converts a number from base basei% to base basef%
'FUNCTION  chance`          (p!)                                              Determines a chance event with probability p!
'FUNCTION  ctorgb&          (c%%)                                             Returns the 32bit color value of a 4bit color c%%
'FUNCTION  degtorad!        (d!)                                              Returns d! degrees as a value in radians
'FUNCTION  dice&            (s&)                                              Determines the result of a dice roll on a dice with s& sides
'FUNCTION  distance#        (x1!, y1!, x2!, y2!)                              Returns the distance between point (x1&, y1&) and point (x2&, y2&)
'FUNCTION  filetitle$       (f$)                                              Returns the file title of a file name string f$
'FUNCTION  filetype$        (f$)                                              Returns the file type of a file name string f$
'FUNCTION  Isfg`            ()                                                Returns TRUE if the program is the foreground window
'FUNCTION  isonbox`         (x!, y!, x1!, y1!, x2!, y2!)                      Returns whether a point (x&, y&) is within the area of rectangle (x1&, y1&, x2&, y&)
'FUNCTION  midpoint#        (p1!, p2!)                                        Returns the midpoint value between p1& and p2&
'FUNCTION  pointoncirclex!  (x!, y!, r!, a!)                                  Returns the x coordinate of the point on the circumference of a circle with center (x!, y!) and radius r& at a! radians from the horizontal
'FUNCTION  pointoncircley!  (x!, y!, r!, a!)                                  Returns the y coordinate of the point on the circumference of a circle with center (x!, y!) and radius r& at a! radians from the horizontal
'FUNCTION  radtodeg!        (r!)                                              Returns r! radians as a value in degrees
'FUNCTION  rangepick&       (s AS RANGE)                                      Picks a random number from a range
'FUNCTION  RECTcollide`     (a AS RECT, b AS RECT)                            Returns if two RECTs are overlapping or not
'FUNCTION  remfilesuffix$   (f$)                                              Removes the file suffix from a file name string f$
'FUNCTION  TSTAMP$          ()                                                Returns the timestamp expressed as a string in the form "YYYYMMDDHHMMSS"
'FUNCTION  TRIM$            (s$)                                              Trims the left and right of a string for spaces
'FUNCTION  trimdec#         (n#, d~%%)                                        Trims a value n# to to d~&& decimal places
'FUNCTION  TRIMnum$         (n!)                                              Converts a number n! to a string and trims the left and right for spaces
'FUNCTION  UnicodeToANSI$   (u$)                                              Converts a wide character string into an ascii character string
'FUNCTION  URLCASE$         (s$)                                              Converts a string to a URL acceptable string
'FUNCTION  zeroset$         (n!, z%%)                                         Prefixes a number with zeros to a specified length if required
'
'******************************************************************************
'
'Manually include this code into your program:
'https://db.tt/8iRMaXc8
'
'Include this code at the beginning of your program:
'https://db.tt/sut0WUyg
'
'******************************************************************************
'
'Requires Windows XP or higher
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/4uREMi0e
'
'******************************************************************************

SUB FULLSCREEN ()
SELECT CASE console.fs
    CASE FULLSCREEN_OFF
        _FULLSCREEN
        console.fs = FULLSCREEN_ON
    CASE FULLSCREEN_ON
        _FULLSCREEN _OFF
        console.fs = FULLSCREEN_OFF
END SELECT
END SUB

SUB HIDESCREEN ()
SELECT CASE console.hs
    CASE HIDESCREEN_OFF
        _SCREENHIDE
        console.hs = HIDESCREEN_ON
    CASE HIDESCREEN_ON
        _SCREENSHOW
        console.hs = HIDESCREEN_OFF
END SELECT
END SUB

SUB MAPRECT (s AS LONG, sl AS LONG, st AS LONG, sr AS LONG, sb AS LONG, d AS LONG, dl AS LONG, dt AS LONG, dr AS LONG, db AS LONG)
_MAPTRIANGLE _SEAMLESS(sl, st)-(sl, sb)-(sr, sb), s TO(dl, dt)-(dl, db)-(dr, db), d
_MAPTRIANGLE _SEAMLESS(sl, st)-(sr, st)-(sr, sb), s TO(dl, dt)-(dr, dt)-(dr, db), d
END SUB

SUB resetfile (filename AS STRING)
DIM ff AS LONG
ff = FREEFILE
OPEN filename FOR OUTPUT AS #ff
CLOSE #ff
END SUB

SUB SaveBMP (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, image AS LONG, filename AS STRING) 'SMcNeill
DIM b AS _UNSIGNED _BYTE
DIM c AS LONG
DIM cv AS LONG
DIM d AS LONG
DIM f AS LONG
DIM ff AS LONG
DIM fh AS LONG
DIM fw AS LONG
DIM h AS LONG
DIM i AS LONG
DIM imagesize AS LONG
DIM l AS LONG
DIM offsetbits AS LONG
DIM s AS LONG
DIM screen0to256 AS LONG
DIM tempscreen AS LONG
DIM w AS LONG
DIM x AS LONG
DIM y AS LONG
DIM zp AS LONG
DIM o AS _OFFSET
DIM m AS _MEM
DIM n AS _MEM
DIM t AS STRING * 1
DIM g AS STRING
DIM temp AS STRING
DIM t1 AS STRING
DIM zeropad AS STRING
DIM BMP AS BMPFormat
IF _PIXELSIZE(image) = 0 THEN
    d = _DEST
    s = _SOURCE
    DIM plt(15) AS LONG
    _SOURCE (image)
    FOR i = 0 TO 15
        plt(i) = _PALETTECOLOR(i, image)
    NEXT i
    f = _FONT(image)
    fw = _FONTWIDTH(image)
    fh = _FONTHEIGHT(image)
    w = _WIDTH(image) * _FONTWIDTH(f)
    h = _HEIGHT(image) * _FONTHEIGHT(f)
    l = (_WIDTH(image) * _HEIGHT(image)) * 2
    tempscreen = _NEWIMAGE(w, h + _HEIGHT(image), 256)
    screen0to256 = _NEWIMAGE(w, h, 256)
    m = _MEMIMAGE(image)
    o = m.OFFSET
    _DEST (tempscreen)
    FOR i = 0 TO 15
        _PALETTECOLOR i, plt(i)
    NEXT i
    _FONT f
    FOR i = 0 TO l - 2 STEP 2
        _MEMGET m, m.OFFSET + i, t
        _MEMGET m, m.OFFSET + i + 1, b
        IF b > 127 THEN b = b - 128
        COLOR b MOD 16, b \ 16
        PRINT t;
    NEXT i
    _PUTIMAGE , tempscreen, screen0to256, (0, 0)-(w, h)
    _FREEIMAGE tempscreen
    _DEST d
    _SOURCE s
    _MEMFREE m
    SaveBMP (x1) * fw, (y1) * fh, (x2) * fw, (y2) * fh, screen0to256, filename
    _FREEIMAGE screen0to256
    EXIT SUB
END IF
m = _MEMIMAGE(image)
IF x1 > x2 THEN SWAP x1, x2
IF y1 > y2 THEN SWAP y1, y2
s = _SOURCE
_SOURCE image
BMP.PWidth = (x2 - x1) + 1
BMP.PDepth = (y2 - y1) + 1
BMP.ID = "BM"
BMP.Blank = 0
BMP.Hsize = 40
BMP.Planes = 1
BMP.Compression = 0
BMP.Xres = 0
BMP.Yres = 0
BMP.SigColors = 0
SELECT CASE _PIXELSIZE(image)
    CASE 1
        temp = SPACE$(x2 - x1 + 1)
        offsetbits = 54 + 1024
        BMP.BPP = 8
        IF BMP.PWidth MOD 4 THEN zeropad = SPACE$(4 - (BMP.PWidth MOD 4))
        imagesize = (BMP.PWidth + LEN(zeropad)) * BMP.PDepth
        BMP.ImageBytes = imagesize
        BMP.NumColors = 256
        BMP.Size = imagesize + offsetbits
        BMP.Offset = offsetbits
    CASE 4
        temp = SPACE$(3)
        offsetbits = 54
        BMP.BPP = 24
        IF ((BMP.PWidth * 3) MOD 4) THEN zeropad = SPACE$(4 - ((BMP.PWidth * 3) MOD 4))
        imagesize = (BMP.PWidth + LEN(zeropad)) * BMP.PDepth
        BMP.ImageBytes = imagesize
        BMP.NumColors = 0
        BMP.Size = imagesize * 3 + offsetbits
        BMP.Offset = offsetbits
END SELECT
ff = FREEFILE
n = _MEMNEW(BMP.Size)
_MEMPUT n, n.OFFSET, BMP
o = n.OFFSET + 54
zp = LEN(zeropad)
$CHECKING:OFF
IF BMP.BPP = 8 THEN
    FOR c = 0 TO 255
        cv = _PALETTECOLOR(c, image)
        g = CHR$(_BLUE32(cv)) + CHR$(_GREEN32(cv)) + CHR$(_RED32(cv)) + CHR$(0)
        _MEMPUT n, o, g
        o = o + 4
    NEXT c
    y = y2 + 1
    w = _WIDTH(image)
    x = x2 - x1 + 1
    DO
        y = y - 1
        _MEMGET m, m.OFFSET + (w * y + x1), temp
        _MEMPUT n, o, temp
        o = o + x
        _MEMPUT n, o, zeropad
        o = o + zp
    LOOP UNTIL y = y1
ELSE
    y = y2 + 1
    w = _WIDTH(image)
    DO
        y = y - 1
        x = x1 - 1
        DO
            x = x + 1
            _MEMGET m, m.OFFSET + (w * y + x) * 4, temp
            _MEMPUT n, o, temp
            o = o + 3
        LOOP UNTIL x = x2
        _MEMPUT n, o, zeropad
        o = o + zp
    LOOP UNTIL y = y1
END IF
$CHECKING:ON
_MEMFREE m
OPEN filename FOR BINARY AS #ff
t1 = SPACE$(BMP.Size)
_MEMGET n, n.OFFSET, t1
PUT #ff, , t1
_MEMFREE n
CLOSE #ff
_SOURCE s
END SUB

SUB SCREENSHOT (region AS _BYTE, fileloc AS STRING, filename AS STRING)
DIM ssfile AS STRING
IF region < SCREENSHOT_MONITOR OR region > SCREENSHOT_WINDOW THEN EXIT SUB
IF filename = "" THEN filename = TSTAMP
IF fileloc = "" THEN fileloc = Ssloc
ssfile = fileloc
IF ssfile > "" THEN ssfile = ssfile + "\"
ssfile = ssfile + filename + ".bmp"
SELECT CASE region
    CASE SCREENSHOT_MONITOR
        SaveBMP 0, 0, MONITORX - 1, MONITORY - 1, _SCREENIMAGE, ssfile
    CASE SCREENSHOT_WINDOW
        SaveBMP 0, 0, SCRX - 1, SCRY - 1, 0, ssfile
END SELECT
END SUB

FUNCTION BaseConv$ (basei AS INTEGER, basef AS INTEGER, n AS STRING)
DIM i AS LONG
DIM x AS LONG
DIM dec10 AS _FLOAT
DIM num10 AS _UNSIGNED _INTEGER64
DIM r AS _UNSIGNED _INTEGER64
DIM v AS _UNSIGNED _INTEGER64
DIM c AS STRING
DIM charseti AS STRING
DIM charsetf AS STRING
DIM dec AS STRING
DIM numdec AS STRING
SELECT CASE basei
    CASE BASE_BASE64
        charseti = CHARSET_BASE64
    CASE IS > LEN(CHARSET_STANDARD) - 1
        FOR x = 0 TO 255
            charseti = charseti + CHR$(x)
        NEXT x
    CASE ELSE
        charseti = CHARSET_STANDARD
END SELECT
SELECT CASE basef
    CASE BASE_BASE64
        charsetf = CHARSET_BASE64
    CASE IS > LEN(CHARSET_STANDARD) - 1
        FOR x = 0 TO 255
            charsetf = charsetf + CHR$(x)
        NEXT x
    CASE ELSE
        charsetf = CHARSET_STANDARD
END SELECT
IF basei <= BASE_BASE64 THEN
    IF INSTR(n, ".") THEN
        dec = MID$(n, INSTR(n, ".") + 1)
        n = LEFT$(n, INSTR(n, ".") - 1)
    END IF
    FOR i = 1 TO LEN(dec)
        c = MID$(dec, LEN(dec) - i + 1, 1)
        v = INSTR(charseti, c) - 1
        dec10 = dec10 + v * (basei ^ (i - 1))
    NEXT i
    dec10 = dec10 / (basei ^ (i - 1))
END IF
dec = MID$(LTRIM$(RTRIM$(STR$(dec10))), 2)
FOR i = 1 TO LEN(n)
    c = MID$(n, LEN(n) - i + 1, 1)
    v = INSTR(charseti, c) - 1
    num10 = num10 + v * (basei ^ (i - 1))
NEXT i
IF basef = BASE_DECIMAL THEN
    BaseConv = TRIMnum(num10) + "." + dec
    EXIT FUNCTION
END IF
r = num10
DO
    v = r MOD basef
    BaseConv = MID$(charsetf, v + 1, 1) + BaseConv
    IF r < basef THEN EXIT DO
    r = r \ basef
LOOP
IF LEN(dec) THEN
    BaseConv = BaseConv + "."
    FOR x = 1 TO LEN(dec)
        dec10 = dec10 * basef
        v = INT(dec10)
        BaseConv = BaseConv + MID$(charsetf, v + 1, 1)
        dec10 = dec10 - v
    NEXT x
    FOR x = LEN(BaseConv) TO 1 STEP -1
        c = MID$(BaseConv, x, 1)
        IF c <> "0" THEN EXIT FOR
    NEXT x
    BaseConv = LEFT$(BaseConv, x)
END IF
END FUNCTION

FUNCTION chance` (p AS SINGLE)
chance = p >= RND
END FUNCTION

FUNCTION ctorgb& (c AS _BYTE)
SELECT CASE c
    CASE 0
        ctorgb = 4278190080
    CASE 1
        ctorgb = 4278190248
    CASE 2
        ctorgb = 4278233088
    CASE 3
        ctorgb = 4278233256
    CASE 4
        ctorgb = 4289200128
    CASE 5
        ctorgb = 4289200296
    CASE 6
        ctorgb = 4289221632
    CASE 7
        ctorgb = 4289243304
    CASE 8
        ctorgb = 4283716692
    CASE 9
        ctorgb = 4283716860
    CASE 10
        ctorgb = 4283759700
    CASE 11
        ctorgb = 4283759868
    CASE 12
        ctorgb = 4294726740
    CASE 13
        ctorgb = 4294726908
    CASE 14
        ctorgb = 4294769748
    CASE 15
        ctorgb = 4294769916
    CASE ELSE
        ctorgb = 4278190080
END SELECT
END FUNCTION

FUNCTION degtorad! (d AS SINGLE)
degtorad = d * PI / 180
END FUNCTION

FUNCTION dice& (s AS LONG)
dice = INT(RND * s + 1)
END FUNCTION

FUNCTION distance# (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
distance = SQR(((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))
END FUNCTION

FUNCTION filetitle$ (f AS STRING)
DIM x AS LONG
FOR x = LEN(f) TO 1 STEP -1
    IF MID$(f, x, 1) = "/" OR MID$(f, x, 1) = "\" OR x = 1 THEN
        filetitle = RIGHT$(f, LEN(f) - x)
        EXIT FUNCTION
    END IF
NEXT x
END FUNCTION

FUNCTION filetype$ (f AS STRING)
DIM x AS LONG
FOR x = LEN(f) TO 1 STEP -1
    IF MID$(f, x, 1) = CHR$(46) THEN
        filetype = RIGHT$(f, LEN(f) - x + 1)
        EXIT FUNCTION
    END IF
NEXT x
END FUNCTION

FUNCTION Isfg` ()
IF Hwnd = GetForegroundWindow THEN Isfg = TRUE
END FUNCTION

FUNCTION isonbox` (x AS SINGLE, y AS SINGLE, x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
IF x < x1 THEN EXIT FUNCTION
IF x > x2 THEN EXIT FUNCTION
IF y < y1 THEN EXIT FUNCTION
IF y > y2 THEN EXIT FUNCTION
isonbox = -1
END FUNCTION

FUNCTION midpoint# (p1 AS SINGLE, p2 AS SINGLE)
midpoint = (p1 + p2) / 2
END FUNCTION

FUNCTION pointoncirclex! (x AS SINGLE, y AS SINGLE, r AS SINGLE, a AS SINGLE)
pointoncirclex = x + (r * COS(a))
END FUNCTION

FUNCTION pointoncircley! (x AS SINGLE, y AS SINGLE, r AS SINGLE, a AS SINGLE)
pointoncircley = y + (r * SIN(a))
END FUNCTION

FUNCTION radtodeg! (r AS SINGLE)
radtodeg = r * 180 / PI
END FUNCTION

FUNCTION rangepick& (s AS RANGE)
rangepick = INT(RND * (s.max - s.min) + s.min)
END FUNCTION

FUNCTION RECTcollide` (a AS RECT, b AS RECT)
IF a.left > b.right THEN EXIT FUNCTION
IF a.right < b.left THEN EXIT FUNCTION
IF a.top > b.bottom THEN EXIT FUNCTION
IF a.bottom < b.top THEN EXIT FUNCTION
RECTcollide = TRUE
END FUNCTION

FUNCTION remfilesuffix$ (f AS STRING)
DIM x AS LONG
remfilesuffix = f
FOR x = LEN(f) TO 1 STEP -1
    IF MID$(f, x, 1) = CHR$(46) THEN
        remfilesuffix = LEFT$(f, x - 1)
        EXIT FUNCTION
    END IF
NEXT x
END FUNCTION

FUNCTION TSTAMP$ ()
TSTAMP = RIGHT$(DATE$, 4) + LEFT$(DATE$, 2) + MID$(DATE$, 4, 2) + LEFT$(TIME$, 2) + MID$(TIME$, 4, 2) + RIGHT$(TIME$, 2)
END FUNCTION

FUNCTION TRIM$ (s AS STRING)
TRIM = LTRIM$(RTRIM$(s))
END FUNCTION

FUNCTION trimdec# (n AS DOUBLE, d AS _UNSIGNED _BYTE)
trimdec = _ROUND(n * (10 ^ d)) / (10 ^ d)
END FUNCTION

FUNCTION TRIMnum$ (n AS SINGLE)
TRIMnum = TRIM(STR$(n))
END FUNCTION

FUNCTION UnicodeToANSI$ (u AS STRING)
DIM getansi AS _UNSIGNED LONG
FOR getansi = 1 TO LEN(u) STEP 2
    UnicodeToANSI = UnicodeToANSI + MID$(u, getansi, 1)
NEXT getansi
END FUNCTION

FUNCTION URLCASE$ (s AS STRING)
DIM x AS LONG
DIM c AS STRING
FOR x = 1 TO LEN(s)
    c = MID$(s, x, 1)
    SELECT CASE ASC(c)
        CASE 32
            c = "%20"
        CASE 38
            c = "%3f"
        CASE 47
            c = "%2f"
        CASE 58
            c = "%3a"
        CASE 59
            c = "%3b"
        CASE 61
            c = "%3d"
        CASE 63
            c = "%3f"
        CASE 64
            c = "%40"
    END SELECT
    URLCASE = URLCASE + c
NEXT x
END FUNCTION

FUNCTION zeroset$ (n AS SINGLE, z AS _BYTE)
zeroset = LEFT$(TRIMnum(n), z)
zeroset = STRING$((z - LEN(zeroset)), 48) + zeroset
END FUNCTION