'Gorlock's QB64 Button Library
'2013-08-13
'
'SUB       BUTTONFREE            (handle%)                                       Frees a button handle
'SUB       BUTTONTEMPLATEFREE    (handle%)                                       Frees a button template handle
'SUB       HIDEBUTTON            (handle%)                                       Hides a button that was shown
'SUB       LOCKBUTTON            (handle%)                                       Locks a button
'SUB       LOCKBUTTONTOGGLE      (handle%)                                       Toggles a button between locked and unlocked
'SUB       PRINTBUTTON           (handle%)                                       Prints a button to the screen
'SUB       PUTBUTTON             (left&, top&, right&, bottom&, handle%)         Moves a button to certain coordinates
'SUB       SHOWBUTTON            (handle%)                                       Shows a button that was hidden
'SUB       UNLOCKBUTTON          (handle%)                                       Unlocks a button
'SUB       UPDATEBUTTON          (handle%)                                       Updates a button
'FUNCTION  BUTTONCLICK%%         (handle%)                                       Returns the click value of a button
'FUNCTION  BUTTONHIT%%           (handle%)                                       Returns the hit value of a button
'FUNCTION  BUTTONSTATUS%%        (handle%)                                       Returns the status of a button
'FUNCTION  BUTTONTIME!           (handle%)                                       Returns the time value of a button
'FUNCTION  BUTTONVALUE`          (handle%)                                       Returns whether a button is on or off
'FUNCTION  NEWBUTTON%            (template%, value%%, status%%, show%%, auto%%)  Creates a new button
'FUNCTION  NEWBUTTONTEMPLATE%    (spritesheet$)                                  Creates a new button template
'FUNCTION  VALIDBUTTON`          (handle%)                                       Returns whether a button handle is valid or not
'FUNCTION  VALIDBUTTONTEMPLATE`  (handle%)                                       Returns whether a button template handle is valid or not
'
'Requires Ritchie's QB64 Sprite Library to run
'Requires your program be in OPTION BASE 1

SUB BUTTONFREE (handle AS INTEGER)
SHARED buttons() AS button
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            IF VALIDBUTTON(handle) THEN
                IF buttons(handle).timer THEN TIMER(buttons(handle).timer) FREE
                IF handle = UBOUND(buttons) THEN
                    REDIM _PRESERVE buttons(UBOUND(buttons) - 1) AS button
                ELSE
                    buttons(handle).inuse = 0
                END IF
            END IF
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        IF buttons(handle).timer THEN TIMER(buttons(handle).timer) FREE
        IF handle = UBOUND(buttons) THEN
            REDIM _PRESERVE buttons(UBOUND(buttons) - 1) AS button
        ELSE
            buttons(handle).inuse = 0
        END IF
END SELECT
END SUB

SUB BUTTONTEMPLATEFREE (handle AS INTEGER)
SHARED buttontemplates() AS buttontemplate
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttontemplates) TO 1 STEP -1
            IF VALIDBUTTONTEMPLATE(handle) THEN
                SHEETFREE buttontemplates(handle).spritesheet
                IF handle = UBOUND(buttontemplates) THEN
                    REDIM _PRESERVE buttontemplates(UBOUND(buttontemplates) - 1) AS buttontemplate
                ELSE
                    buttontemplates(handle).inuse = 0
                END IF
            END IF
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTONTEMPLATE(handle) THEN EXIT SUB
        SHEETFREE buttontemplates(handle).spritesheet
        IF handle = UBOUND(buttontemplates) THEN
            REDIM _PRESERVE buttontemplates(UBOUND(buttontemplates) - 1) AS buttontemplate
        ELSE
            buttontemplates(handle).inuse = 0
        END IF
END SELECT
END SUB

SUB HIDEBUTTON (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
buttons(handle).show = BUTTON_HIDE
buttons(handle).status = BUTTON_CREATED
END SUB

SUB LOCKBUTTON (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
buttons(handle).status = BUTTON_LOCKED
END SUB

SUB LOCKBUTTONTOGGLE (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
buttons(handle).status = buttons(handle).status XOR 2 ^ 0
END SUB

SUB PRINTBUTTON (handle AS INTEGER)
SHARED buttons() AS button
SHARED buttontemplates() AS buttontemplate
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
IF buttons(handle).show = BUTTON_HIDE THEN EXIT SUB
IF buttons(handle).status < BUTTON_STAND THEN EXIT SUB
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
END SUB

SUB PUTBUTTON (left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, handle AS INTEGER)
SHARED buttons() AS button
SHARED buttontemplates() AS buttontemplate
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
IF left * top = 0 THEN EXIT SUB
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
END SUB

SUB SHOWBUTTON (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
buttons(handle).show = BUTTON_SHOW
buttons(handle).status = BUTTON_STAND
END SUB

SUB UNLOCKBUTTON (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT SUB
buttons(handle).status = BUTTON_STAND
END SUB

SUB UPDATEBUTTON (handle AS INTEGER)
DIM lclick AS _BIT
DIM mclick AS _BIT
DIM onbutton AS _BIT
DIM rclick AS _BIT
DIM saveclick AS _BYTE
DIM savestatus AS _BYTE
DIM x AS LONG
DIM y AS LONG
SHARED buttons() AS button
DO
    x = _MOUSEX
    y = _MOUSEY
    lclick = _MOUSEBUTTON(1)
    rclick = _MOUSEBUTTON(2)
    mclick = _MOUSEBUTTON(3)
LOOP WHILE _MOUSEINPUT
SELECT CASE handle
    CASE BUTTON_ALLBUTTONS
        FOR handle = UBOUND(buttons) TO 1 STEP -1
            IF VALIDBUTTON(handle) THEN
                IF buttons(handle).show = BUTTON_SHOW THEN
                    saveclick = buttons(handle).click
                    buttons(handle).click = 0
                    IF lclick THEN buttons(handle).click = BUTTON_LCLICK
                    IF rclick THEN buttons(handle).click = BUTTON_RCLICK
                    IF mclick THEN buttons(handle).click = BUTTON_MCLICK
                    IF lclick AND rclick THEN buttons(handle).click = BUTTON_LRCLICK
                    IF lclick AND mclick THEN buttons(handle).click = BUTTON_LMCLICK
                    IF rclick AND mclick THEN buttons(handle).click = BUTTON_RMCLICK
                    IF lclick AND rclick AND mclick THEN buttons(handle).click = BUTTON_LRMCLICK
                    IF buttons(handle).status <> BUTTON_LOCKED THEN
                        savestatus = buttons(handle).status
                        onbutton = -1
                        IF x < buttons(handle).area.left OR x > buttons(handle).area.right THEN onbutton = 0
                        IF y < buttons(handle).area.top OR y > buttons(handle).area.bottom THEN onbutton = 0
                        IF onbutton THEN
                            IF lclick OR rclick OR mclick THEN
                                buttons(handle).status = BUTTON_ACTION
                            ELSE
                                buttons(handle).status = BUTTON_HOVER
                            END IF
                        ELSE
                            buttons(handle).status = BUTTON_STAND
                        END IF
                        IF buttons(handle).status <> savestatus THEN buttons(handle).time = TIMER(.001)
                        buttons(handle).hit = 0
                        IF saveclick = BUTTON_LCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 1
                        IF saveclick = BUTTON_RCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 2
                        IF saveclick = BUTTON_MCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 3
                        IF saveclick = BUTTON_LRCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 4
                        IF saveclick = BUTTON_LMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 5
                        IF saveclick = BUTTON_RMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 6
                        IF saveclick = BUTTON_LRMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 7
                        IF buttons(handle).hit THEN buttons(handle).value = NOT buttons(handle).value
                    END IF
                END IF
            END IF
        NEXT handle
    CASE ELSE
        IF NOT VALIDBUTTON(handle) THEN EXIT SUB
        IF buttons(handle).show = BUTTON_HIDE THEN EXIT SUB
        saveclick = buttons(handle).click
        buttons(handle).click = 0
        IF lclick THEN buttons(handle).click = BUTTON_LCLICK
        IF rclick THEN buttons(handle).click = BUTTON_RCLICK
        IF mclick THEN buttons(handle).click = BUTTON_MCLICK
        IF lclick AND rclick THEN buttons(handle).click = BUTTON_LRCLICK
        IF lclick AND mclick THEN buttons(handle).click = BUTTON_LMCLICK
        IF rclick AND mclick THEN buttons(handle).click = BUTTON_RMCLICK
        IF lclick AND rclick AND mclick THEN buttons(handle).click = BUTTON_LRMCLICK
        IF buttons(handle).status <> BUTTON_LOCKED THEN
            savestatus = buttons(handle).status
            onbutton = -1
            IF x < buttons(handle).area.left OR x > buttons(handle).area.right THEN onbutton = 0
            IF y < buttons(handle).area.top OR y > buttons(handle).area.bottom THEN onbutton = 0
            IF onbutton THEN
                IF lclick OR rclick OR mclick THEN
                    buttons(handle).status = BUTTON_ACTION
                ELSE
                    buttons(handle).status = BUTTON_HOVER
                END IF
            ELSE
                buttons(handle).status = BUTTON_STAND
            END IF
            IF buttons(handle).status <> savestatus THEN buttons(handle).time = TIMER(.001)
            buttons(handle).hit = 0
            IF saveclick = BUTTON_LCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 1
            IF saveclick = BUTTON_RCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 2
            IF saveclick = BUTTON_MCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 3
            IF saveclick = BUTTON_LRCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 4
            IF saveclick = BUTTON_LMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 5
            IF saveclick = BUTTON_RMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 6
            IF saveclick = BUTTON_LRMCLICK AND buttons(handle).click = BUTTON_NOCLICK AND onbutton THEN buttons(handle).hit = 7
            IF buttons(handle).hit THEN buttons(handle).value = buttons(handle).value XOR -2 ^ 0
        END IF
END SELECT
END SUB

FUNCTION BUTTONCLICK%% (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONCLICK = buttons(handle).click
END FUNCTION

FUNCTION BUTTONHIT%% (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONHIT = buttons(handle).hit
END FUNCTION

FUNCTION BUTTONSTATUS%% (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONSTATUS = buttons(handle).status
END FUNCTION

FUNCTION BUTTONTIME! (handle AS INTEGER)
DIM currenttime AS SINGLE
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
currenttime = TIMER(.001)
IF currenttime < buttons(handle).time THEN
    BUTTONTIME = currenttime + buttons(handle).time - 86400
ELSE
    BUTTONTIME = currenttime - buttons(handle).time
END IF
END FUNCTION

FUNCTION BUTTONVALUE` (handle AS INTEGER)
SHARED buttons() AS button
IF NOT VALIDBUTTON(handle) THEN EXIT FUNCTION
BUTTONVALUE = buttons(handle).value
END FUNCTION

FUNCTION NEWBUTTONTEMPLATE% (spritesheet AS STRING)
DIM findfreebuttontemplate AS INTEGER
DIM spritesheetpeek AS LONG
SHARED buttontemplates() AS buttontemplate
IF NOT _FILEEXISTS(spritesheet) THEN EXIT FUNCTION
FOR findfreebuttontemplate = 1 TO UBOUND(buttontemplates)
    IF buttontemplates(findfreebuttontemplate).inuse = 0 THEN
        NEWBUTTONTEMPLATE = findfreebuttontemplate
        EXIT FOR
    END IF
NEXT findfreebuttontemplate
IF NEWBUTTONTEMPLATE = 0 THEN
    REDIM _PRESERVE buttontemplates(UBOUND(buttontemplates) + 1) AS buttontemplate
    NEWBUTTONTEMPLATE = UBOUND(buttontemplates)
END IF
buttontemplates(NEWBUTTONTEMPLATE).inuse = -1
spritesheetpeek = _LOADIMAGE(spritesheet)
IF spritesheetpeek = 0 THEN EXIT FUNCTION
buttontemplates(NEWBUTTONTEMPLATE).width = INT(_WIDTH(spritesheetpeek) / 4)
buttontemplates(NEWBUTTONTEMPLATE).height = INT(_HEIGHT(spritesheetpeek) / 2)
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

FUNCTION NEWBUTTON% (template AS INTEGER, value AS _BYTE, status AS _BYTE, show AS _BYTE, auto AS _BYTE)
DIM findfreebutton AS INTEGER
SHARED buttons() AS button
SHARED buttontemplates() AS buttontemplate
IF value < BUTTON_ON THEN value = BUTTON_ON
IF value > BUTTON_OFF THEN value = BUTTON_OFF
IF status > BUTTON_LOCKED THEN status = BUTTON_LOCKED
IF status < BUTTON_UNCREATED THEN status = BUTTON_UNCREATED
IF show < BUTTON_SHOW THEN show = BUTTON_SHOW
IF show > BUTTON_HIDE THEN show = BUTTON_HIDE
IF auto < BUTTON_AUTOUPDATE_ON THEN auto = BUTTON_AUTOUPDATE_ON
IF auto > BUTTON_AUTOUPDATE_OFF THEN auto = BUTTON_AUTOUPDATE_OFF
FOR findfreebutton = 1 TO UBOUND(buttons)
    IF buttons(findfreebutton).inuse = 0 THEN
        NEWBUTTON = findfreebutton
        EXIT FOR
    END IF
NEXT findfreebutton
IF NEWBUTTON = 0 THEN
    REDIM _PRESERVE buttons(UBOUND(buttons) + 1) AS button
    NEWBUTTON = UBOUND(buttons)
END IF
buttons(NEWBUTTON).inuse = -1
buttons(NEWBUTTON).show = show
buttons(NEWBUTTON).value = value
IF status THEN
    buttons(NEWBUTTON).status = status
ELSE
    buttons(NEWBUTTON).status = 1
END IF
buttons(NEWBUTTON).click = 0
buttons(NEWBUTTON).hit = 0
buttons(NEWBUTTON).template = template
buttons(NEWBUTTON).area.left = 0
buttons(NEWBUTTON).area.top = 0
buttons(NEWBUTTON).area.right = 0
buttons(NEWBUTTON).area.bottom = 0
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

FUNCTION VALIDBUTTON` (handle AS INTEGER)
SHARED buttons() AS button
IF handle > UBOUND(buttons) OR handle = 0 THEN EXIT FUNCTION
IF buttons(handle).inuse = 0 THEN EXIT FUNCTION
VALIDBUTTON = -1
END FUNCTION

FUNCTION VALIDBUTTONTEMPLATE` (handle AS INTEGER)
SHARED buttontemplates() AS buttontemplate
IF handle > UBOUND(buttontemplates) OR handle = 0 THEN EXIT FUNCTION
IF buttontemplates(handle).inuse = 0 THEN EXIT FUNCTION
VALIDBUTTONTEMPLATE = -1
END FUNCTION