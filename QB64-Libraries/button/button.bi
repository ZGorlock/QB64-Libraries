'******************************************************************************
'
'Gorlock's QB64 Button Library
'v0.32
'
'******************************************************************************
'
'Version 0.1  - 2013-08-13
'Version 0.11 - 2013-10-04
'  - added tutorial and init include file
'Version 0.2  - 2013-10-16
'  - added queue, swap, and clone commands
'Version 0.3  - 2013-12-14
'  - updated commands to use new version of Gorlock's QB64 Input Library
'  - made CLONEBUTTON and SWAPBUTTON more efficient
'  - fixed minor errors
'Version 0.31 - 2013-12-16
'  - added SETBUTTON
'  - fixed minor errors
'Version 0.32 - 2014-01-03
'  - added button sample resource
'  - added ability to define dest image
'
'******************************************************************************
'
'SUB       BUTTONBRINGTOFRONT    (handle%)                                       Sets a button's queue to be in front of all other buttons
'SUB       BUTTONFREE            (handle%)                                       Frees a button handle
'SUB       BUTTONSENDTOBACK      (handle%)                                       Sets a button's queue to be behind all other buttons
'SUB       BUTTONSETQUEUE        (handle%)                                       Sets a button's queue value
'SUB       BUTTONTEMPLATEFREE    (handle%)                                       Frees a button template handle
'SUB       HIDEBUTTON            (handle%)                                       Hides a button that was shown
'SUB       LOCKBUTTON            (handle%)                                       Locks a button
'SUB       LOCKBUTTONTOGGLE      (handle%)                                       Toggles a button between locked and unlocked
'SUB       PRINTBUTTON           (handle%)                                       Prints a button to the screen
'SUB       PUTBUTTON             (x&, y&, handle%, dest&)                        Puts a button centered on a coordinate
'SUB       SETBUTTON             (left&, top&, right&, bottom&, handle%, dest&)  Sets a button to a certain area
'SUB       SHOWBUTTON            (handle%)                                       Shows a button that was hidden
'SUB       SWAPBUTTON            (handle1%, handle2%)                            Swaps the position of two buttons in the array
'SUB       UNLOCKBUTTON          (handle%)                                       Unlocks a button
'SUB       UPDATEBUTTON          (handle%)                                       Updates a button
'FUNCTION  BUTTONCLICK%%         (handle%)                                       Returns the click value of a button
'FUNCTION  BUTTONHIT%%           (handle%)                                       Returns the hit value of a button
'FUNCTION  BUTTONQUEUE%          (handle%)                                       Returns the queue value of a button
'FUNCTION  BUTTONSTATUS%%        (handle%)                                       Returns the status of a button
'FUNCTION  BUTTONTIME!           (handle%)                                       Returns the time value of a button
'FUNCTION  BUTTONVALUE`          (handle%)                                       Returns whether a button is on or off
'FUNCTION  CLONEBUTTON%          (handle%)                                       Creates a copy of a button to a new handle
'FUNCTION  NEWBUTTON%            (template%, value%%, status%%, show%%, auto%%)  Creates a new button
'FUNCTION  NEWBUTTONTEMPLATE%    (spritesheet$)                                  Creates a new button template
'FUNCTION  VALIDBUTTON`          (handle%)                                       Returns whether a button handle is valid or not
'FUNCTION  VALIDBUTTONTEMPLATE`  (handle%)                                       Returns whether a button template handle is valid or not
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/gt5pIvJ6
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'This library requires Gorlock's QB64 Input Library: https://db.tt/DffOJN6z
'This library requires Gorlock's edited version of Ritchie's QB64 Sprite
'  Library: https://db.tt/O8HXSCBC
'
'For button samples download: https://db.tt/A6M8MXvW
'Credits for these images goes to Under the Moon - Open Game Art
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/p9wlgztu
'
'******************************************************************************

SUB BUTTONBRINGTOFRONT (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
BUTTONSETQUEUE handle, UBOUND(buttons)
END SUB

SUB BUTTONFREE (handle AS INTEGER)
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            BUTTONFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        IF buttons(handle).timer THEN TIMER(buttons(handle).timer) FREE
        IF handle = UBOUND(buttons) AND handle > 1 THEN
            REDIM _PRESERVE buttons(1 TO UBOUND(buttons) - 1) AS BUTTON
        ELSE
            buttons(handle).inuse = FALSE
        END IF
END SELECT
END SUB

SUB BUTTONSENDTOBACK (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
BUTTONSETQUEUE handle, 1
END SUB

SUB BUTTONSETQUEUE (handle AS INTEGER, queue AS INTEGER)
DIM findqueue AS INTEGER
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
IF queue < 1 OR queue > UBOUND(buttons) THEN EXIT SUB
FOR findqueue = 1 TO UBOUND(buttons)
    IF buttons(findqueue).queue = queue THEN SWAP buttons(handle).queue, buttons(findqueue).queue
NEXT findqueue
END SUB

SUB BUTTONTEMPLATEFREE (handle AS INTEGER)
SHARED buttontemplates() AS BUTTONTEMPLATE
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttontemplates) TO 1 STEP -1
            BUTTONTEMPLATEFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTONTEMPLATE(handle) THEN EXIT SUB
        SHEETFREE buttontemplates(handle).spritesheet
        IF handle = UBOUND(buttontemplates) THEN
            REDIM _PRESERVE buttontemplates(1 TO UBOUND(buttontemplates) - 1) AS BUTTONTEMPLATE
        ELSE
            buttontemplates(handle).inuse = FALSE
        END IF
END SELECT
END SUB

SUB HIDEBUTTON (handle AS INTEGER)
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            HIDEBUTTON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        buttons(handle).show = BUTTON_HIDE
        buttons(handle).status = BUTTON_CREATED
END SELECT
END SUB

SUB LOCKBUTTON (handle AS INTEGER)
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            LOCKBUTTON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        buttons(handle).status = BUTTON_LOCKED
END SELECT
END SUB

SUB LOCKBUTTONTOGGLE (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            LOCKBUTTONTOGGLE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        SELECT CASE buttons(handle).status
            CASE BUTTON_LOCKED
                buttons(handle).status = BUTTON_STAND
            CASE ELSE
                buttons(handle).status = BUTTON_LOCKED
        END SELECT
END SELECT
END SUB

SUB PRINTBUTTON (handle AS INTEGER)
DIM queue AS INTEGER
SHARED buttons() AS BUTTON
SHARED buttontemplates() AS BUTTONTEMPLATE
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR queue = 1 TO UBOUND(buttons)
                FOR handle = 1 TO UBOUND(buttons)
                    IF buttons(handle).queue = queue THEN PRINTBUTTON handle
        NEXT handle, queue
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        IF buttons(handle).show = BUTTON_HIDE THEN EXIT SUB
        IF buttons(handle).status < BUTTON_STAND THEN EXIT SUB
        IF buttons(handle).dest THEN _DEST buttons(handle).dest
        SELECT CASE buttons(handle).status
            CASE BUTTON_STAND
                SELECT CASE buttons(handle).value
                    CASE BUTTON_OFF
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).stand.false
                    CASE BUTTON_ON
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).stand.true
                END SELECT
            CASE BUTTON_HOVER
                SELECT CASE buttons(handle).value
                    CASE BUTTON_OFF
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).hover.false
                    CASE BUTTON_ON
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).hover.true
                END SELECT
            CASE BUTTON_ACTION
                SELECT CASE buttons(handle).value
                    CASE BUTTON_OFF
                         SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).active.false
                    CASE BUTTON_ON
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).active.true
                END SELECT
            CASE BUTTON_LOCKED
                SELECT CASE buttons(handle).value
                    CASE BUTTON_OFF
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).locked.false
                    CASE BUTTON_ON
                        SPRITESTRETCH buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom, buttontemplates(buttons(handle).template).locked.true
                END SELECT
        END SELECT
        IF buttons(handle).dest THEN _DEST 0
END SELECT
END SUB

SUB PUTBUTTON (x AS LONG, y AS LONG, handle AS INTEGER, dest AS LONG)
SHARED buttons() AS BUTTON
SHARED buttontemplates() AS BUTTONTEMPLATE
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
IF buttons(handle).show = BUTTON_HIDE THEN
    buttons(handle).show = BUTTON_SHOW
    buttons(handle).status = BUTTON_STAND
END IF
buttons(handle).area.left = x - _ROUND((buttontemplates(buttons(handle).template).width / 2))
buttons(handle).area.top = y - _ROUND((buttontemplates(buttons(handle).template).height / 2))
buttons(handle).area.right = buttons(handle).area.left + buttontemplates(buttons(handle).template).width
buttons(handle).area.bottom = buttons(handle).area.top + buttontemplates(buttons(handle).template).height
IF dest THEN buttons(handle).dest = dest
END SUB

SUB SETBUTTON (left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, handle AS INTEGER, dest AS LONG)
SHARED buttons() AS BUTTON
SHARED buttontemplates() AS BUTTONTEMPLATE
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
IF buttons(handle).show = BUTTON_HIDE THEN
    buttons(handle).show = BUTTON_SHOW
    buttons(handle).status = BUTTON_STAND
END IF
buttons(handle).area.left = left
buttons(handle).area.top = top
IF right THEN
    buttons(handle).area.right = right
ELSE
    buttons(handle).area.right = buttons(handle).area.left + buttontemplates(buttons(handle).template).width
END IF
IF bottom THEN
    buttons(handle).area.bottom = bottom
ELSE
    buttons(handle).area.bottom = buttons(handle).area.top + buttontemplates(buttons(handle).template).height
END IF
IF dest THEN buttons(handle).dest = dest
END SUB

SUB SHOWBUTTON (handle AS INTEGER)
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            SHOWBUTTON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        buttons(handle).show = BUTTON_SHOW
        buttons(handle).status = BUTTON_STAND
END SELECT
END SUB

SUB SWAPBUTTON (handle1 AS INTEGER, handle2 AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle1) THEN EXIT SUB
IF NOT VALIDBUTTON(handle2) THEN EXIT SUB
SWAP buttons(handle1), buttons(handle2)
END SUB

SUB UNLOCKBUTTON (handle AS INTEGER)
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            UNLOCKBUTTON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        buttons(handle).status = BUTTON_STAND
END SELECT
END SUB

SUB UPDATEBUTTON (handle AS INTEGER)
DIM saveclick AS _BYTE
DIM savestatus AS _BYTE
SHARED buttons() AS BUTTON
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            UPDATEBUTTON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        IF buttons(handle).show = BUTTON_HIDE THEN EXIT SUB
        saveclick = buttons(handle).click
        buttons(handle).click = 0
        IF LCLICK THEN buttons(handle).click = BUTTON_LCLICK
        IF RCLICK THEN buttons(handle).click = BUTTON_RCLICK
        IF MCLICK THEN buttons(handle).click = BUTTON_MCLICK
        IF LCLICK AND RCLICK THEN buttons(handle).click = BUTTON_LRCLICK
        IF LCLICK AND MCLICK THEN buttons(handle).click = BUTTON_LMCLICK
        IF RCLICK AND MCLICK THEN buttons(handle).click = BUTTON_RMCLICK
        IF LCLICK AND RCLICK AND MCLICK THEN buttons(handle).click = BUTTON_LRMCLICK
        IF buttons(handle).status <> BUTTON_LOCKED THEN
            savestatus = buttons(handle).status
            buttons(handle).hit = FALSE
            IF isonbox(MOUSEX, MOUSEY, buttons(handle).area.left, buttons(handle).area.top, buttons(handle).area.right, buttons(handle).area.bottom) THEN
                IF CLICK THEN
                    buttons(handle).status = BUTTON_ACTION
                ELSE
                    buttons(handle).status = BUTTON_HOVER
                END IF
                IF saveclick = BUTTON_LCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 1
                IF saveclick = BUTTON_RCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 2
                IF saveclick = BUTTON_MCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 3
                IF saveclick = BUTTON_LRCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 4
                IF saveclick = BUTTON_LMCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 5
                IF saveclick = BUTTON_RMCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 6
                IF saveclick = BUTTON_LRMCLICK AND buttons(handle).click = BUTTON_NOCLICK THEN buttons(handle).hit = 7
            ELSE
                buttons(handle).status = BUTTON_STAND
            END IF
            IF buttons(handle).status <> savestatus THEN buttons(handle).time = TIMER
            IF buttons(handle).hit THEN buttons(handle).value = buttons(handle).value XOR -2 ^ 0
        END IF
END SELECT
END SUB

FUNCTION BUTTONCLICK%% (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONCLICK = buttons(handle).click
END FUNCTION

FUNCTION BUTTONHIT%% (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONHIT = buttons(handle).hit
END FUNCTION

FUNCTION BUTTONQUEUE% (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONQUEUE = buttons(handle).queue
END FUNCTION

FUNCTION BUTTONSTATUS%% (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONSTATUS = buttons(handle).status
END FUNCTION

FUNCTION BUTTONTIME! (handle AS INTEGER)
DIM nowtime AS SINGLE
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
nowtime = TIMER(.001)
IF nowtime < buttons(handle).time THEN
    BUTTONTIME = nowtime + buttons(handle).time - 86400
ELSE
    BUTTONTIME = nowtime - buttons(handle).time
END IF
END FUNCTION

FUNCTION BUTTONVALUE` (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONVALUE = buttons(handle).value
END FUNCTION

FUNCTION CLONEBUTTON% (handle AS INTEGER)
DIM findfreebutton AS INTEGER
SHARED buttons() AS BUTTON
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
FOR findfreebutton = 1 TO UBOUND(buttons)
    IF buttons(findfreebutton).inuse = FALSE THEN
        CLONEBUTTON = findfreebutton
        EXIT FOR
    END IF
NEXT findfreebutton
IF CLONEBUTTON = 0 THEN
    REDIM _PRESERVE buttons(1 TO UBOUND(buttons) + 1) AS BUTTON
    CLONEBUTTON = UBOUND(buttons)
END IF
buttons(CLONEBUTTON) = buttons(handle)
IF buttons(CLONEBUTTON).timer THEN
    buttons(CLONEBUTTON).timer = _FREETIMER
    ON TIMER(buttons(CLONEBUTTON).timer, .025) UPDATEBUTTON CLONEBUTTON
    TIMER(buttons(CLONEBUTTON).timer) ON
    UPDATEBUTTON CLONEBUTTON
END IF
END FUNCTION

FUNCTION NEWBUTTON% (template AS INTEGER, value AS _BYTE, status AS _BYTE, show AS _BYTE, auto AS _BYTE)
DIM findfreebutton AS INTEGER
SHARED buttons() AS BUTTON
SHARED buttontemplates() AS BUTTONTEMPLATE
IF value < BUTTON_ON THEN value = BUTTON_ON
IF value > BUTTON_OFF THEN value = BUTTON_OFF
IF status > BUTTON_LOCKED THEN status = BUTTON_LOCKED
IF status < BUTTON_UNCREATED THEN status = BUTTON_UNCREATED
IF show < BUTTON_SHOW THEN show = BUTTON_SHOW
IF show > BUTTON_HIDE THEN show = BUTTON_HIDE
IF auto < BUTTON_AUTOUPDATE_ON THEN auto = BUTTON_AUTOUPDATE_ON
IF auto > BUTTON_AUTOUPDATE_OFF THEN auto = BUTTON_AUTOUPDATE_OFF
FOR findfreebutton = 1 TO UBOUND(buttons)
    IF buttons(findfreebutton).inuse = FALSE THEN
        NEWBUTTON = findfreebutton
        EXIT FOR
    END IF
NEXT findfreebutton
IF NEWBUTTON = 0 THEN
    REDIM _PRESERVE buttons(1 TO UBOUND(buttons) + 1) AS BUTTON
    NEWBUTTON = UBOUND(buttons)
END IF
buttons(NEWBUTTON).inuse = TRUE
buttons(NEWBUTTON).queue = NEWBUTTON
buttons(NEWBUTTON).show = show
buttons(NEWBUTTON).value = value
IF status THEN
    buttons(NEWBUTTON).status = status
ELSE
    buttons(NEWBUTTON).status = 1
END IF
buttons(NEWBUTTON).click = FALSE
buttons(NEWBUTTON).hit = FALSE
buttons(NEWBUTTON).template = template
buttons(NEWBUTTON).area.left = 0
buttons(NEWBUTTON).area.top = 0
buttons(NEWBUTTON).area.right = 0
buttons(NEWBUTTON).area.bottom = 0
buttons(NEWBUTTON).dest = 0
buttons(NEWBUTTON).time = 0
IF auto = BUTTON_AUTOUPDATE_ON THEN
    buttons(NEWBUTTON).timer = _FREETIMER
    ON TIMER(buttons(NEWBUTTON).timer, .025) UPDATEBUTTON NEWBUTTON
    TIMER(buttons(NEWBUTTON).timer) ON
    UPDATEBUTTON NEWBUTTON
ELSE
    buttons(NEWBUTTON).timer = 0
END IF
END FUNCTION

FUNCTION NEWBUTTONTEMPLATE% (spritesheet AS STRING)
DIM findfreebuttontemplate AS INTEGER
DIM spritesheetpeek AS LONG
SHARED buttontemplates() AS BUTTONTEMPLATE
IF NOT _FILEEXISTS(spritesheet) THEN EXIT FUNCTION
FOR findfreebuttontemplate = 1 TO UBOUND(buttontemplates)
    IF buttontemplates(findfreebuttontemplate).inuse = FALSE THEN
        NEWBUTTONTEMPLATE = findfreebuttontemplate
        EXIT FOR
    END IF
NEXT findfreebuttontemplate
IF NEWBUTTONTEMPLATE = 0 THEN
    REDIM _PRESERVE buttontemplates(1 TO UBOUND(buttontemplates) + 1) AS BUTTONTEMPLATE
    NEWBUTTONTEMPLATE = UBOUND(buttontemplates)
END IF
buttontemplates(NEWBUTTONTEMPLATE).inuse = TRUE
spritesheetpeek = _LOADIMAGE(spritesheet)
IF spritesheetpeek = 0 THEN EXIT FUNCTION
buttontemplates(NEWBUTTONTEMPLATE).width = INT(_WIDTH(spritesheetpeek) / BUTTON_COLUMNS)
buttontemplates(NEWBUTTONTEMPLATE).height = INT(_HEIGHT(spritesheetpeek) / BUTTON_ROWS)
_FREEIMAGE spritesheetpeek
buttontemplates(NEWBUTTONTEMPLATE).spritesheet = SPRITESHEETLOAD(spritesheet, buttontemplates(NEWBUTTONTEMPLATE).width, buttontemplates(NEWBUTTONTEMPLATE).height, SPRITE_NOTRANSPARENCY)
buttontemplates(NEWBUTTONTEMPLATE).stand.false = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 1, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).stand.true = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 5, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).hover.false = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 2, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).hover.true = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 6, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).active.false = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 3, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).active.true = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 7, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).locked.false = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 4, SPRITE_DONTSAVE)
buttontemplates(NEWBUTTONTEMPLATE).locked.true = SPRITENEW(buttontemplates(NEWBUTTONTEMPLATE).spritesheet, 8, SPRITE_DONTSAVE)
END FUNCTION

FUNCTION VALIDBUTTON` (handle AS INTEGER)
SHARED buttons() AS BUTTON
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(buttons) THEN EXIT FUNCTION
IF buttons(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDBUTTON = TRUE
END FUNCTION

FUNCTION VALIDBUTTONTEMPLATE` (handle AS INTEGER)
SHARED buttontemplates() AS BUTTONTEMPLATE
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(buttontemplates) THEN EXIT FUNCTION
IF buttontemplates(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDBUTTONTEMPLATE = TRUE
END FUNCTION