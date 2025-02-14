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
TYPE CONSOLEMETRICS
    fs AS _BYTE
    hs AS _BYTE
END TYPE

'boolean constant definitions
CONST FALSE = 0
CONST TRUE = NOT FALSE

'mathematical constant definitions
CONST PI = 3.1415926536
CONST PI2 = 6.2831853072
CONST E = 2.7182818285
CONST GR = 1.6180339888
CONST SQR2 = 1.4142135623
CONST SQR3 = 1.7320508075
CONST SQR5 = 2.2360679774

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
DIM SHARED MONITORX AS LONG
DIM SHARED MONITORY AS LONG
DIM SHARED SCRX AS LONG
DIM SHARED SCRY AS LONG
DIM SHARED SCRZ AS LONG
DIM SHARED Crlf AS STRING
DIM SHARED Null AS STRING
DIM SHARED Q AS STRING
DIM SHARED Slash AS STRING
DIM SHARED Ssloc AS STRING
DIM SHARED console AS CONSOLEMETRICS

'set constant shared strings
Crlf = CHR$(13) + CHR$(10)
Null = CHR$(0)
Q = CHR$(34)

'determine slash character
IF INSTR(_OS$, "[WINDOWS]") THEN
    Slash = CHR$(92)
ELSE
    Slash = CHR$(47)
END IF

'screen dimensions defintions
SCRX = 640
SCRY = 480
SCRZ = 32

'get resolution information
DIM resolution AS LONG
DIM rec AS RECT
resolution = _SCREENIMAGE
MONITORX = _WIDTH(resolution)
MONITORY = _HEIGHT(resolution)
_FREEIMAGE resolution

'set screen image
SCREEN _NEWIMAGE(SCRX, SCRY, SCRZ)