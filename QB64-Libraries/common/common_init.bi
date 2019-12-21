'single instance checking
ON ERROR GOTO sierr
OPEN "temp\si.tmp" FOR OUTPUT AS #32767
LOCK #32767
sierrpass:
ON ERROR GOTO 0

'declare common sub/func
DECLARE SUB FULLSCREEN ()
DECLARE SUB HIDESCREEN ()
DECLARE SUB MAPRECT (source AS LONG, sourceleft AS LONG, sourcetop AS LONG, sourceright AS LONG, sourcebottom AS LONG, dest AS LONG, destleft AS LONG, desttop AS LONG, destright AS LONG, destbottom AS LONG)
DECLARE SUB resetfile (filename AS STRING)
DECLARE SUB SaveBMP (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, image AS LONG, filename AS STRING)
DECLARE SUB SCREENSHOT (region AS _BYTE, fileloc AS STRING, filename AS STRING)
DECLARE FUNCTION BaseConv$ (basei AS INTEGER, basef AS INTEGER, n AS STRING)
DECLARE FUNCTION chance` (probability AS SINGLE)
DECLARE FUNCTION ctorgb& (c AS _BYTE)
DECLARE FUNCTION degtorad! (d AS SINGLE)
DECLARE FUNCTION dice& (sides AS LONG)
DECLARE FUNCTION distance## (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
DECLARE FUNCTION filetitle$ (filename AS STRING)
DECLARE FUNCTION filetype$ (filename AS STRING)
DECLARE FUNCTION Isfg` ()
DECLARE FUNCTION isonbox` (x AS SINGLE, y AS SINGLE, x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
DECLARE FUNCTION midpoint## (p1 AS SINGLE, p2 AS SINGLE)
DECLARE FUNCTION pointoncirclex! (x AS SINGLE, y AS SINGLE, r AS SINGLE, a AS SINGLE)
DECLARE FUNCTION pointoncircley! (x AS SINGLE, y AS SINGLE, r AS SINGLE, a AS SINGLE)
DECLARE FUNCTION radtodeg! (r AS SINGLE)
DECLARE FUNCTION rangepick& (s AS RANGE)
DECLARE FUNCTION RECTcollide` (a AS RECT, b AS RECT)
DECLARE FUNCTION remfilesuffix$ (filename AS STRING)
DECLARE FUNCTION TRIM$ (s AS STRING)
DECLARE FUNCTION trimdec## (num AS _FLOAT, dec AS _UNSIGNED _BYTE)
DECLARE FUNCTION TRIMnum$ (n AS SINGLE)
DECLARE FUNCTION UnicodeToANSI$ (u AS STRING)
DECLARE FUNCTION URLCASE$ (s AS STRING)
DECLARE FUNCTION zeroset$ (n AS SINGLE, z AS _BYTE)

'primary windows type declarations
TYPE RECT
    left AS LONG
    top AS LONG
    right AS LONG
    bottom AS LONG
END TYPE

'windows type declarations
TYPE OSVERSIONINFOEX
    dwOSVersionInfoSize AS _UNSIGNED LONG
    dwMajorVersion AS _UNSIGNED LONG
    dwMinorVersion AS _UNSIGNED LONG
    dwBuildNumber AS _UNSIGNED LONG
    dwPlatformId AS _UNSIGNED LONG
    szCSDVersion AS STRING * 128
    wServicePackMajor AS _UNSIGNED INTEGER
    wServicePackMinor AS _UNSIGNED INTEGER
    wSuiteMask AS _UNSIGNED INTEGER
    wProductType AS _UNSIGNED _BYTE
    wReserved AS _UNSIGNED _BYTE
END TYPE

'primary custom type declarations
TYPE COORDINATE
    x AS DOUBLE
    y AS DOUBLE
END TYPE
TYPE RANGE
    min AS DOUBLE
    max AS DOUBLE
END TYPE

'file format type declarations
TYPE BMPFormat
    ID AS STRING * 2
    Size AS LONG
    Blank AS LONG
    Offset AS LONG
    Hsize AS LONG
    PWidth AS LONG
    PDepth AS LONG
    Planes AS INTEGER
    BPP AS INTEGER
    Compression AS LONG
    ImageBytes AS LONG
    Xres AS LONG
    Yres AS LONG
    NumColors AS LONG
    SigColors AS LONG
END TYPE

'custom type declarations
TYPE errorhandle
    err AS INTEGER
    line AS _UNSIGNED LONG
    count AS _UNSIGNED LONG
END TYPE
TYPE CONSOLEMETRICS
    fs AS _BYTE
    hs AS _BYTE
END TYPE

'boolean constant definitions
CONST FALSE = 0
CONST TRUE = NOT FALSE

'mathematical constant definitions
CONST E = 2.7182818285
CONST GR = 1.6180339888
CONST PI = 3.1415926536
CONST PI2 = 6.2831853072
CONST SQR2 = 1.4142135623
CONST SQR3 = 1.7320508075
CONST SQR5 = 2.2360679774

'windows constant definitions
CONST GENERIC_ALL = 268435456
CONST SPI_GETWORKAREA = &H30

'base character set constants
CONST CHARSET_BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
CONST CHARSET_STANDARD = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/-*=!@#$%^&*():;?<>{}[]\|`~"

'custom constant definitions
CONST BASE_BASE64 = 64
CONST BASE_BINARY = 2
CONST BASE_BYTE = 256
CONST BASE_DECIMAL = 10
CONST BASE_DUODECIMAL = 12
CONST BASE_HEXIDECIMAL = 16
CONST BASE_OCTAL = 8
CONST BASE_SEXAGESIMAL = 60
CONST FULLSCREEN_ON = TRUE
CONST FULLSCREEN_OFF = FALSE
CONST HIDESCREEN_ON = TRUE
CONST HIDESCREEN_OFF = FALSE
CONST SCREENSHOT_WINDOW = FALSE
CONST SCREENSHOT_MONITOR = TRUE
CONST TEXTCOLOR_DEFAULT = 15

'dimension shared variables
DIM SHARED Winos AS _BYTE
DIM SHARED Hwnd AS LONG
DIM SHARED MONITORX AS LONG
DIM SHARED MONITORY AS LONG
DIM SHARED SCRX AS LONG
DIM SHARED SCRY AS LONG
DIM SHARED SCRZ AS LONG
DIM SHARED WORKAREAX AS LONG
DIM SHARED WORKAREAY AS LONG
DIM SHARED Version AS _FLOAT
DIM SHARED Dtwin AS _OFFSET
DIM SHARED Crlf AS STRING
DIM SHARED Null AS STRING
DIM SHARED Programname AS STRING
DIM SHARED Q AS STRING
DIM SHARED Slash AS STRING
DIM SHARED Ssloc AS STRING
DIM SHARED console AS CONSOLEMETRICS

'dimension shared arrays
REDIM SHARED fonts(1 TO 0) AS LONG
REDIM SHARED errors(1 TO 0) AS errorhandle

'set constant shared strings
Crlf = CHR$(13) + CHR$(10)
Null = CHR$(0)
Q = CHR$(34)

'determine Slash character
IF INSTR(_OS$, "[WINDOWS]") THEN
    Slash = CHR$(92)
ELSE
    Slash = CHR$(47)
END IF

'screen dimensions defintions
SCRX = 640
SCRY = 480
SCRZ = 32

'retrieve windows version
DIM osinfo AS OSVERSIONINFOEX
osinfo.dwOSVersionInfoSize = LEN(osinfo) '156 bytes
Winos = -1
IF GetVersionExA(_OFFSET(osinfo)) THEN
    SELECT CASE osinfo.dwMajorVersion
        CASE 5
            SELECT CASE osinfo.dwMinorVersion
                CASE 0
                    Winos = 0 'Windows 2000
                CASE 1, 2
                    Winos = 1 'Windows XP
            END SELECT
        CASE 6
            SELECT CASE osinfo.dwMinorVersion
                CASE 0
                    Winos = 2 'Windows Vista / 2008
                CASE 1
                    Winos = 3 'Windows 7 / 2008 R2
                CASE 2
                    Winos = 4 'Windows 8
            END SELECT
    END SELECT
END IF

'get resolution information
DIM resolution AS LONG
DIM rec AS RECT
resolution = _SCREENIMAGE
MONITORX = _WIDTH(resolution)
MONITORY = _HEIGHT(resolution)
_FREEIMAGE resolution
IF SystemParametersInfoW&(SPI_GETWORKAREA, 0, _OFFSET(rec), 0) <> 0 THEN
    WORKAREAX = rec.right
    WORKAREAY = rec.bottom
END IF

'get window handles
DIM titletag AS LONG
DIM oidesk AS _OFFSET
DIM stdesk AS _OFFSET
Dtwin = GetDesktopWindow
oidesk = OpenInputDesktop(0, 0, GENERIC_ALL)
IF oidesk <> Dtwin AND oidesk > 0 THEN stdesk = SetThreadDesktop(oidesk)
IF Programname > "" THEN
    titletag = RND * &H1000000
    _TITLE Programname + " - " + HEX$(titletag)
    Hwnd = FindWindow(0, Programname + " - " + HEX$(titletag) + CHR$(0))
    _TITLE Programname
END IF

'directory check
IF NOT _DIREXISTS("temp") THEN SHELL _HIDE "mkdir temp"
IF NOT _DIREXISTS("data") THEN SHELL _HIDE "mkdir data"

'open error log
OPEN "data\error.log" FOR APPEND AS #32766
ON ERROR GOTO errorhandler

'set screen image
SCREEN _NEWIMAGE(SCRX, SCRY, SCRZ)