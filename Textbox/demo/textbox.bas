SCREEN _NEWIMAGE(640, 480, 32)

'$INCLUDE:'textboxresourcestop.bi'
'$INCLUDE:'textboxtop.bi'

DIM a AS INTEGER, b AS INTEGER, c AS INTEGER
DIM bg AS LONG, ts AS LONG
a = NEWTEXTBOX(50, 50, 550, 321, TEXTBOX_SHOW, TEXTBOX_TEXT_STATIC, TEXTBOX_INPUT_DIRECT, 0, TEXTBOX_DEFAULTFONT, _RGBA32(0, 0, 0, 255), _RGBA32(128, 64, 64, 150), _RGBA32(255, 255, 255, 255), TEXTBOX_SCROLLBAR_OFF, TEXTBOX_LINK_ON)
b = NEWTEXTBOX(300, 300, 630, 450, TEXTBOX_SHOW, TEXTBOX_TEXT_REVERSE_STRICT, TEXTBOX_INPUT_DIRECT, 0, TEXTBOX_DEFAULTFONT, TEXTBOX_DEFAULTCOLOR, _RGBA32(64, 128, 64, 150), _RGBA32(255, 255, 255, 255), TEXTBOX_SCROLLBAR_ON, TEXTBOX_LINK_ON)
c = NEWTEXTBOX(20, 300, 180, 460, TEXTBOX_SHOW, TEXTBOX_TEXT_STATIC, TEXTBOX_INPUT_DIRECT, 0, TEXTBOX_DEFAULTFONT, TEXTBOX_DEFAULTCOLOR, _RGBA32(64, 64, 128, 255), TEXTBOX_NOFRAME, TEXTBOX_SCROLLBAR_OFF, TEXTBOX_LINK_ON)
bg = _LOADIMAGE("a.jpg")
TEXTBOXSETBACKGROUND a, bg, TEXTBOX_BACKGROUND_STRETCH
TEXTBOXADDTEXT a, "Type variables use <#c1>DOT</#c> <#c13>variable names to <#l50>read</#l> or write</#c> specific <#c2>values</#c>. They do not use type suffixes as they can hold <#l51>ANY</#l> variable type values! The name before the dot is the one you defined after the type definition and the name after is the variable name used inside of the TYPE. The name of the dimensioned type variable alone can be used to <#c13>PUT # or GET #</#c> all of the data at once!", TEXTBOX_NOTICK
TEXTBOXADDTEXT a, "Type <#c5>variables</#c> use DOT variable <#l60>names to read or write specific values. They do not use type suffixes as they can hold ANY variable type values!</#l>", TEXTBOX_NOTICK
TEXTBOXADDTEXT a, "<#l14>*click this to change the size of the box*</#l>", TEXTBOX_NOTICK
SCROLLBARSET b, 1, TEXTBOX_SCROLLBAR_BOX_HOLLOW, TEXTBOX_SCROLLBAR_LINE, TEXTBOX_SCROLLBAR_NOBOX
ts = _SNDOPEN("a.ogg", "SYNC")
TEXTBOXSETTICK b, ts
TEXTBOXADDTEXT c, "Type variables use DOT variable names to read or write specific values. They do not use type suffixes as they can hold ANY variable type values!", 25
TEXTBOXSENDTOBACK c

DO
    _LIMIT 32
    CLS
    CALL getinput
    UPDATETEXTBOX TEXTBOX_ALLTEXTBOXES
    PRINTTEXTBOX TEXTBOX_ALLTEXTBOXES
    IF LINKSTATUS(a, 14) = TEXTBOX_LINKSTATUS_ACTIVE THEN
        SELECT CASE pat
            CASE 0
                TEXTBOXRESIZE a, 50, 50, 420, 420
                pat = 1
            CASE 1
                TEXTBOXRESIZE a, 50, 50, 550, 321
                pat = 0
        END SELECT
    END IF
    IF K$ = CHR$(32) THEN
        g = g + 1
        TEXTBOXADDTEXT b, "Type <#c" + STR$(g) + ">variables</#c> use DOT variable names to read or write specific values. They do not use type suffixes as they can hold ANY variable type values!", 25
    END IF
    IF K$ = CHR$(97) THEN
        SELECT CASE textboxes(b).scrollbar
            CASE TEXTBOX_SCROLLBAR_OFF
                SCROLLBARSHOW b
            CASE TEXTBOX_SCROLLBAR_ON
                SCROLLBARHIDE b
        END SELECT
    END IF
    _DISPLAY
LOOP UNTIL K$ = CHR$(27)
FREETEXTBOX TEXTBOX_ALLTEXTBOXES
SYSTEM

'$INCLUDE:'textbox.bi'
'$INCLUDE:'textboxresources.bi'