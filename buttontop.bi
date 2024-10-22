DECLARE SUB BUTTONFREE (handle AS INTEGER)
DECLARE SUB BUTTONTEMPLATEFREE (handle AS INTEGER)
DECLARE SUB HIDEBUTTON (handle AS INTEGER)
DECLARE SUB LOCKBUTTON (handle AS INTEGER)
DECLARE SUB LOCKBUTTONTOGGLE (handle AS INTEGER)
DECLARE SUB PRINTBUTTON (handle AS INTEGER)
DECLARE SUB PUTBUTTON (left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, handle AS INTEGER)
DECLARE SUB SHOWBUTTON (handle AS INTEGER)
DECLARE SUB UNLOCKBUTTON (handle AS INTEGER)
DECLARE SUB UPDATEBUTTON (handle AS INTEGER)

DECLARE FUNCTION BUTTONCLICK%% (handle AS INTEGER)
DECLARE FUNCTION BUTTONHIT%% (handle AS INTEGER)
DECLARE FUNCTION BUTTONSTATUS%% (handle AS INTEGER)
DECLARE FUNCTION BUTTONTIME! (handle AS INTEGER)
DECLARE FUNCTION BUTTONVALUE` (handle AS INTEGER)
DECLARE FUNCTION NEWBUTTON% (template AS INTEGER, value AS _BYTE, status AS _BYTE, show AS _BYTE, auto AS _BYTE)
DECLARE FUNCTION NEWBUTTONTEMPLATE% (spritesheet AS STRING)
DECLARE FUNCTION VALIDBUTTON` (handle AS INTEGER)
DECLARE FUNCTION VALIDBUTTONTEMPLATE` (handle AS INTEGER)

TYPE RECT
    left AS LONG
    top AS LONG
    right AS LONG
    bottom AS LONG
END TYPE
TYPE buttonimage
    true AS INTEGER
    false AS INTEGER
END TYPE
TYPE buttontemplate
    inuse AS _BYTE
    spritesheet AS INTEGER
    width AS LONG
    height AS LONG
    stand AS buttonimage
    hover AS buttonimage
    active AS buttonimage
    locked AS buttonimage
END TYPE
TYPE button
    inuse AS _BYTE
    show AS _BYTE
    value AS _BYTE
    status AS _BYTE
    click AS _BYTE
    hit AS _BYTE
    template AS INTEGER
    area AS RECT
    time AS SINGLE
    timer AS LONG
    oldTIMER AS SINGLE
END TYPE

CONST FALSE = 0
CONST TRUE = NOT FALSE

CONST BUTTON_ALLBUTTONS = TRUE
CONST BUTTON_AUTOUPDATE_OFF = FALSE
CONST BUTTON_AUTOUPDATE_ON = TRUE
CONST BUTTON_ACTION = 4
CONST BUTTON_CREATED = 1
CONST BUTTON_HIDE = FALSE
CONST BUTTON_HOVER = 3
CONST BUTTON_LCLICK = 1
CONST BUTTON_LMCLICK = 5
CONST BUTTON_LOCKED = 5
CONST BUTTON_LRCLICK = 4
CONST BUTTON_LRMCLICK = 7
CONST BUTTON_MCLICK = 3
CONST BUTTON_NOCLICK = FALSE
CONST BUTTON_OFF = FALSE
CONST BUTTON_ON = TRUE
CONST BUTTON_RCLICK = 2
CONST BUTTON_RMCLICK = 6
CONST BUTTON_SHOW = TRUE
CONST BUTTON_STAND = 2
CONST BUTTON_UNCREATED = FALSE

REDIM buttons(0) AS button
REDIM buttontemplates(0) AS buttontemplate