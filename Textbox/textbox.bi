'Gorlock's QB64 Textbox Library
'2013-09-25
'
'SUB       FREETEXTBOX           (handle%)                                                                                                                                     Frees a textbox handle
'SUB       HIDETEXTBOX           (handle%)                                                                                                                                     Hides a textbox
'SUB       LINKHIDE              (handle%)                                                                                                                                     Hides links in a textbox
'SUB       LINKSET               (handle%, standcolor~&, hovercolor~&, activecolor~&, linkul%%)                                                                                Sets metrics for a textbox's links
'SUB       LINKSHOW              (handle%)                                                                                                                                     Shows links in a textbox
'SUB       PRINTTEXTBOX          (handle%)                                                                                                                                     Prints a textbox to the screen
'SUB       SHOWTEXTBOX           (handle%)                                                                                                                                     Shows a textbox that had been hidden
'SUB       TEXTBOXADDTEXT        (handle%, text$, tickspeed%%)                                                                                                                 Adds text to a textbox
'SUB       TEXTBOXBRINGTOFRONT   (handle%)                                                                                                                                     Moves a textbox to the font of the queue
'SUB       TEXTBOXCLEARTEXT      (handle%)                                                                                                                                     Clears the text from a textbox
'SUB       TEXTBOXFORMATTEXT     (handle%, textstring%, tickspeed%%)                                                                                                           Formats a textstring
'SUB       TEXTBOXREFORMATTEXT   (handle%)                                                                                                                                     Reformats all the text in a textbox
'SUB       TEXTBOXRESIZE         (handle%, left&, top&, right&, bottom&)                                                                                                       Fits a textbox to a different area
'SUB       TEXTBOXSENDTOBACK     (handle%)                                                                                                                                     Moves a textbox to the back of the queue
'SUB       TEXTBOXSETBACKGROUND  (handle%, background&, behaviour%%)                                                                                                           Sets a custom background for a textbox
'SUB       TEXTBOXSETQUEUE       (handle%, queue%)                                                                                                                             Sets a textbox's queue to a certain value
'SUB       TEXTBOXSETTICK        (handle%, tick&)                                                                                                                              Sets the sound for ticking of a textbox
'SUB       TEXTBOXTICK           (handle%)                                                                                                                                     Progresses the autotyping of a textbox
'SUB       SCROLLBARHIDE         (handle%)                                                                                                                                     Hides the scrollbar
'SUB       SCROLLBARSET          (handle%, scollbarcolor~&, scrollbartype%%, scrollbarline%%, scrollbarbox%%)                                                                  Sets metrics for a textbox's scrollbar
'SUB       SCROLLBARSHOW         (handle%)                                                                                                                                     Shows the scrollbar
'SUB       SWAPTEXTBOX           (handle1%, handle2%)                                                                                                                          Swaps the position of two textboxes in the array
'SUB       UPDATETEXTBOX         (handle%)                                                                                                                                     Updates a textbox
'FUNCTION  CLONETEXTBOX%         (handle%)                                                                                                                                     Creates a copy of a textbox
'FUNCTION  LINKSTATUS%%          (handle%, linkcode%)                                                                                                                          Returns a link's status
'FUNCTION  NEWTEXTBOX%           (left&, top&, right&, bottom&, status%%, boxtype%%, inputtype%%, dest&, font&, fontcolor~&, backgroundcolor~&, frame~&, scrollbar%%, link%%)  Creates a new textbox
'FUNCTION  TEXTBOXBACKGROUND&    (handle%)                                                                                                                                     Creates a background image for a textbox
'FUNCTION  TEXTBOXQUEUE%         (handle%)                                                                                                                                     Returns a textbox's value in the queue
'FUNCTION  TEXTBOXSTATUS`        (handle%)                                                                                                                                     Returns the hidden status of a textbox
'FUNCTION  VALIDTEXTBOX`         (handle%)                                                                                                                                     Returns if a textbox handle is valid of not
'
'Requires your program be in OPTION BASE 1
'Requires getinput sub by Gorlock
'Requires isonbox function by Gorlock

SUB FREETEXTBOX (handle AS INTEGER)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            FREETEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF textboxes(handle).inuse = -1 THEN
            TEXTBOXCLEARTEXT handle
            IF textboxes(handle).background THEN _FREEIMAGE (textboxes(handle).background)
            IF textboxes(handle).ticktimer THEN TIMER(textboxes(handle).ticktimer) FREE
            IF handle = UBOUND(textboxes) THEN
                REDIM _PRESERVE textboxes(UBOUND(textboxes) - 1) AS textbox
            ELSE
                textboxes(handle).inuse = 0
            END IF
        END IF
END SELECT
END SUB

SUB HIDETEXTBOX (handle AS INTEGER)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            HIDETEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).status = TEXTBOX_HIDE
END SELECT
END SUB

SUB LINKHIDE (handle AS INTEGER)
DIM hidelinks AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).link = TEXTBOX_LINK_OFF
        FOR hidelinks = 1 TO textboxes(handle).links
            textbox_color(handle, textbox_link(handle, hidelinks, 4), 3) = TEXTBOX_LINK_OFF
        NEXT hidelinks
END SELECT
END SUB

SUB LINKSET (handle AS INTEGER, standcolor AS _UNSIGNED LONG, hovercolor AS _UNSIGNED LONG, activecolor AS _UNSIGNED LONG, linkul AS _BYTE)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKSET handle, standcolor, hovercolor, activecolor, linkul
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF standcolor > 1 THEN textboxes(handle).linkstand = standcolor
        IF hovercolor > 1 THEN textboxes(handle).linkhover = hovercolor
        IF activecolor > 1 THEN textboxes(handle).linkactive = activecolor
        IF linkul < 1 THEN textboxes(handle).linkul = linkul
END SELECT
END SUB

SUB LINKSHOW (handle AS INTEGER)
DIM showlinks AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKSHOW handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).link = TEXTBOX_LINK_ON
        FOR showlinks = 1 TO textboxes(handle).links
            textbox_color(handle, textbox_link(handle, showlinks, 4), 3) = TEXTBOX_LINK_STAND
        NEXT showlinks
END SELECT
END SUB

SUB PRINTTEXTBOX (handle AS INTEGER)
DIM thickenbox AS _BYTE
DIM checkcolors AS INTEGER
DIM checklinks AS INTEGER
DIM printlines AS INTEGER
DIM endline AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR queue = 1 TO UBOUND(textboxes)
                FOR handle = 1 TO UBOUND(textboxes)
                    IF textboxes(handle).queue = queue THEN PRINTTEXTBOX handle
        NEXT handle, queue
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF textboxes(handle).status = TEXTBOX_SHOW THEN
            IF textboxes(handle).dest THEN _DEST textboxes(handle).dest
            IF textboxes(handle).background THEN _PUTIMAGE (textboxes(handle).area.left, textboxes(handle).area.top), textboxes(handle).background
            IF textboxes(handle).frame THEN LINE (textboxes(handle).area.left, textboxes(handle).area.top)-(textboxes(handle).area.right, textboxes(handle).area.bottom), textboxes(handle).frame, B
            IF textboxes(handle).font THEN _FONT textboxes(handle).font
            _PRINTMODE _KEEPBACKGROUND
            SELECT CASE textboxes(handle).type
                CASE TEXTBOX_TITLE
                    _PRINTSTRING (textboxes(handle).area.right - textboxes(handle).area.left - _PRINTWIDTH(MID$(textbox_text(handle), textbox_index(handle, 1), textbox_index(handle, 2) - textbox_index(handle, 1))) / 2 + textboxes(handle).area.left, (textboxes(handle).area.bottom - textboxes(handle).area.left - _FONTHEIGHT(textboxes(handle).font)) / 2 + textboxes(handle).area.top), MID$(textbox_text(handle), textbox_index(handle, 1), textbox_index(handle, 2) - textbox_index(handle, 1))
                CASE ELSE
                    endline = textboxes(handle).screenline + textboxes(handle).rows - 1
                    IF textboxes(handle).textlines < textboxes(handle).screenline + textboxes(handle).rows THEN endline = textboxes(handle).screenline + textboxes(handle).rows
                    FOR printlines = textboxes(handle).screenline TO endline
                        COLOR textboxes(handle).color
                        _PRINTSTRING (textboxes(handle).area.left + 8, textboxes(handle).area.top + 8 + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(handle, printlines), textbox_index(handle, printlines + 1) - textbox_index(handle, printlines))
                        FOR checkcolors = 1 TO textboxes(handle).colors
                            IF NOT ((textbox_color(handle, checkcolors, 1) < textbox_index(handle, printlines) AND textbox_color(handle, checkcolors, 2) < textbox_index(handle, printlines)) OR (textbox_color(handle, checkcolors, 1) > textbox_index(handle, printlines + 1) AND textbox_color(handle, checkcolors, 2) > textbox_index(handle, printlines + 1))) AND textbox_color(handle, checkcolors, 3) > TEXTBOX_LINK_OFF THEN
                                COLOR textbox_color(handle, checkcolors, 3)
                                IF textbox_color(handle, checkcolors, 1) < textbox_index(handle, printlines) THEN
                                    IF textbox_color(handle, checkcolors, 2) > textbox_index(handle, printlines + 1) THEN
                                        _PRINTSTRING (textboxes(handle).area.left + 8, textboxes(handle).area.top + 8 + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(handle, printlines), textbox_index(handle, printlines + 1) - textbox_index(handle, printlines))
                                    ELSE
                                        _PRINTSTRING (textboxes(handle).area.left + 8, textboxes(handle).area.top + 8 + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(handle, printlines), textbox_color(handle, checkcolors, 2) - textbox_index(handle, printlines))
                                    END IF
                                ELSE
                                    IF textbox_color(handle, checkcolors, 2) > textbox_index(handle, printlines + 1) THEN
                                        _PRINTSTRING (textboxes(handle).area.left + 8 + (textbox_color(handle, checkcolors, 1) - textbox_index(handle, printlines) + 1 * (textbox_index(handle, printlines) = 0)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_color(handle, checkcolors, 1), textbox_index(handle, printlines + 1) - textbox_color(handle, checkcolors, 1))
                                    ELSE
                                        _PRINTSTRING (textboxes(handle).area.left + 8 + (textbox_color(handle, checkcolors, 1) - textbox_index(handle, printlines) + 1 * (textbox_index(handle, printlines) = 0)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_color(handle, checkcolors, 1), textbox_color(handle, checkcolors, 2) - textbox_color(handle, checkcolors, 1))
                                    END IF
                                END IF
                            END IF
                        NEXT checkcolors
                        IF textboxes(handle).link = TEXTBOX_LINK_ON AND textboxes(handle).linkul = TEXTBOX_LINK_UNDERLINE_ON THEN
                            FOR checklinks = 1 TO textboxes(handle).links
                                IF NOT ((textbox_link(handle, checklinks, 1) < textbox_index(handle, printlines) AND textbox_link(handle, checklinks, 2) < textbox_index(handle, printlines)) OR (textbox_link(handle, checklinks, 1) > textbox_index(handle, printlines + 1) AND textbox_link(handle, checklinks, 2) > textbox_index(handle, printlines + 1))) AND textbox_link(handle, checklinks, 3) > TEXTBOX_LINK_OFF THEN
                                    IF textbox_link(handle, checklinks, 1) < textbox_index(handle, printlines) THEN
                                        IF textbox_link(handle, checklinks, 2) > textbox_index(handle, printlines + 1) THEN
                                            LINE (textboxes(handle).area.left + 8, textboxes(handle).area.top + 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_index(handle, printlines + 1) - textbox_index(handle, printlines) - 1) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(handle, textbox_link(handle, checklinks, 4), 3)
                                        ELSE
                                            LINE (textboxes(handle).area.left + 8, textboxes(handle).area.top + 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_link(handle, checklinks, 2) - textbox_index(handle, printlines)) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(handle, textbox_link(handle, checklinks, 4), 3)
                                        END IF
                                    ELSE
                                        IF textbox_link(handle, checklinks, 2) > textbox_index(handle, printlines + 1) THEN
                                            LINE (textboxes(handle).area.left + 8 + (textbox_link(handle, checklinks, 1) - textbox_index(handle, printlines) + 1 * (printlines = textboxes(handle).screenline) + 1 * (textboxes(handle).screenline < 1 AND printlines = 1)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_index(handle, printlines + 1) - textbox_index(handle, printlines) - 1) * _FONTWIDTH(textboxes(handle).font) - (textbox_link(handle, checklinks, 1) - textbox_index(handle, printlines)) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(handle, textbox_link(handle, checklinks, 4), 3)
                                        ELSE
                                            LINE (textboxes(handle).area.left + 8 + (textbox_link(handle, checklinks, 1) - textbox_index(handle, printlines) + 1 * (printlines = textboxes(handle).screenline) + 1 * (textboxes(handle).screenline < 1 AND printlines = 1)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_link(handle, checklinks, 2) - textbox_index(handle, printlines) - 1) * _FONTWIDTH(textboxes(handle).font) - (textbox_link(handle, checklinks, 1) - textbox_index(handle, printlines) - 1) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(handle, textbox_link(handle, checklinks, 4), 3)
                                        END IF
                                    END IF
                                END IF
                            NEXT checklinks
                        END IF
                    NEXT printlines
                    IF textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON THEN
                        IF textboxes(handle).scrollbarbox = TEXTBOX_SCROLLBAR_BOX THEN LINE (textboxes(handle).area.right - (2 * (_FONTWIDTH(textboxes(handle).font))), textboxes(handle).area.top + 1)-STEP(0, (textboxes(handle).area.bottom - textboxes(handle).area.top) - 2), textboxes(handle).scrollbarcolor
                        SELECT CASE textboxes(handle).scrollbartype
                            CASE TEXTBOX_SCROLLBAR_BOX_DEFAULT
                                scrollbarsize = textboxes(handle).rows / textboxes(handle).textlines * (textboxes(handle).area.bottom - textboxes(handle).area.top)
                                IF scrollbarsize < 15 THEN scrollbarsize = 15
                                IF scrollbarsize > textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN scrollbarsize = textboxes(handle).area.bottom - textboxes(handle).area.top - 4
                                IF scrollbarsize < textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize)) + textboxes(handle).area.top + 2
                                ELSE
                                    scrollbarloc = textboxes(handle).area.top + 2
                                END IF
                                LINE (textboxes(handle).area.right - 2, scrollbarloc)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2, scrollbarsize), textboxes(handle).scrollbar, BF
                                IF scrollbarsize > 15 THEN LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2))-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                IF scrollbarsize > 30 THEN
                                    LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - 3)-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                    LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) + 3)-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                END IF
                            CASE TEXTBOX_SCROLLBAR_BOX_HOLLOW
                                scrollbarsize = textboxes(handle).rows / textboxes(handle).textlines * (textboxes(handle).area.bottom - textboxes(handle).area.top)
                                IF scrollbarsize < 15 THEN scrollbarsize = 15
                                IF scrollbarsize > textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN scrollbarsize = textboxes(handle).area.bottom - textboxes(handle).area.top - 4
                                IF scrollbarsize < textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize)) + textboxes(handle).area.top + 2
                                ELSE
                                    scrollbarloc = textboxes(handle).area.top + 2
                                END IF
                                FOR thickenbox = 0 TO 3
                                    LINE (textboxes(handle).area.right - 2 - thickenbox, scrollbarloc + thickenbox)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2 + 2 * thickenbox, scrollbarsize - 2 * thickenbox), textboxes(handle).scrollbar, B
                                NEXT thickenbox
                                IF scrollbarsize > 15 THEN LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbar, BF
                                IF scrollbarsize > 30 THEN
                                    LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - (scrollbarsize / 4) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbar, BF
                                    LINE (textboxes(handle).area.right - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) + (scrollbarsize / 4) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbar, BF
                                END IF
                            CASE TEXTBOX_SCROLLBAR_BOX_HOLLOW_NOLINE
                                scrollbarsize = textboxes(handle).rows / textboxes(handle).textlines * (textboxes(handle).area.bottom - textboxes(handle).area.top)
                                IF scrollbarsize < 15 THEN scrollbarsize = 15
                                IF scrollbarsize > textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN scrollbarsize = textboxes(handle).area.bottom - textboxes(handle).area.top - 4
                                IF scrollbarsize < textboxes(handle).area.bottom - textboxes(handle).area.top - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize)) + textboxes(handle).area.top + 2
                                ELSE
                                    scrollbarloc = textboxes(handle).area.top + 2
                                END IF
                                FOR thickenbox = 0 TO 3
                                    LINE (textboxes(handle).area.right - 2 - thickenbox, scrollbarloc + thickenbox)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2 + 2 * thickenbox, scrollbarsize - 2 * thickenbox), textboxes(handle).scrollbar, B
                                NEXT thickenbox
                            CASE TEXTBOX_SCROLLBAR_BALL_DEFAULT
                                scrollbarsize = _FONTWIDTH(textboxes(handle).font) * 1.5
                                scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize * 2)) + textboxes(handle).area.top + 2 + scrollbarsize / 2
                                CIRCLE (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), scrollbarsize / 2, textboxes(handle).scrollbar
                                PAINT (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), textboxes(handle).scrollbar
                            CASE TEXTBOX_SCROLLBAR_BALL_HOLLOW
                                scrollbarsize = _FONTWIDTH(textboxes(handle).font) * 1.5
                                scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize * 2)) + textboxes(handle).area.top + 2 + scrollbarsize / 2
                                CIRCLE (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), scrollbarsize / 2, textboxes(handle).scrollbar
                            CASE TEXTBOX_SCROLLBAR_DOT
                                scrollbarsize = 1
                                scrollbarloc = ((textboxes(handle).screenline - 1) / (textboxes(handle).textlines - textboxes(handle).rows) * (textboxes(handle).area.bottom - textboxes(handle).area.top - 4 - scrollbarsize * 2)) + textboxes(handle).area.top + 2 + scrollbarsize / 2
                                PSET (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), textboxes(handle).scrollbar
                        END SELECT
                        IF textboxes(handle).scrollbarline = TEXTBOX_SCROLLBAR_LINE THEN
                            LINE (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 2)-(textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc), textboxes(handle).scrollbarcolor
                            LINE (textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize)-(textboxes(handle).area.right - _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.bottom - 2), textboxes(handle).scrollbarcolor
                        END IF
                    END IF
            END SELECT
            COLOR textboxes(handle).color
            IF textboxes(handle).font THEN _FONT fonts(FONT_DEFAULT)
            IF textboxes(handle).dest THEN _DEST 0
        END IF
END SELECT
END SUB

SUB SHOWTEXTBOX (handle AS INTEGER)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SHOWTEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).status = TEXTBOX_SHOW
END SELECT
END SUB

SUB TEXTBOXADDTEXT (handle AS INTEGER, text AS STRING, tickspeed AS _BYTE)
DIM linkcolor AS _BIT
DIM terminateindex AS INTEGER
DIM movetext AS LONG
DIM valuesave AS STRING
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
textboxes(handle).textstrings = textboxes(handle).textstrings + 1
textbox_index_original(handle, textboxes(handle).textstrings + 1) = textbox_index_original(handle, textboxes(handle).textstrings)
FOR movetext = 1 TO LEN(text)
    IF MID$(text, movetext, 2) = "<#" THEN
        SELECT CASE MID$(text, movetext + 2, 1)
            CASE "c"
                textboxes(handle).colors = textboxes(handle).colors + 1
                textbox_color(handle, textboxes(handle).colors, 1) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
                textbox_color(handle, textboxes(handle).colors, 2) = textbox_index_original(handle, textboxes(handle).textstrings) + LEN(text)
                movetext = movetext + 2
                valuesave = ""
                DO
                    movetext = movetext + 1
                    IF MID$(text, movetext, 1) = ">" THEN EXIT DO
                    valuesave = valuesave + MID$(text, movetext, 1)
                LOOP UNTIL movetext >= LEN(text)
                valuesave = LTRIM$(RTRIM$(valuesave))
                IF movetext < LEN(text) THEN
                    IF VAL(valuesave) OR LEFT$(valuesave, 1) = "0" THEN
                        IF VAL(valuesave) <= 15 THEN
                            SELECT CASE VAL(valuesave)
                                CASE 0
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190080
                                CASE 1
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190248
                                CASE 2
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278233088
                                CASE 3
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278233256
                                CASE 4
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289200128
                                CASE 5
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289200296
                                CASE 6
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289221632
                                CASE 7
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289243304
                                CASE 8
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283716692
                                CASE 9
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283716860
                                CASE 10
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283759700
                                CASE 11
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283759868
                                CASE 12
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294726740
                                CASE 13
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294726908
                                CASE 14
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294769748
                                CASE 15
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294769916
                                CASE ELSE
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190080
                            END SELECT
                        ELSE
                            textbox_color(handle, textboxes(handle).colors, 3) = VAL(valuesave)
                        END IF
                    ELSE IF LEFT$(valuesave, 1) = "(" THEN
                            textbox_color(handle, textboxes(handle).colors, 3) = _RGB32(VAL(MID$(valuesave, 2, 3)), VAL(MID$(valuesave, 6, 3)), VAL(MID$(valuesave, 10, 3)))
                        ELSE
                            SELECT CASE UCASE$(valuesave)
                                CASE "BLACK"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190080
                                CASE "DARK BLUE", "DARKBLUE", "DARK_BLUE"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190248
                                CASE "DARK LIME", "DARKLIME", "DARK_LIME"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278233088
                                CASE "DARK CYAN", "DARKCYAN", "DARK_CYAN"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278233256
                                CASE "RED"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289200128
                                CASE "DARK MAGENTA", "DARKMAGENTA", "DARK_MAGENTA"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289200296
                                CASE "GOLDEN ROD", "GOLDENROD", "GOLDEN_ROD"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289221632
                                CASE "LIGHT GRAY", "LIGHTGRAY", "LIGHT_GRAY"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4289243304
                                CASE "GRAY"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283716692
                                CASE "BLUE"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283716860
                                CASE "GREEN"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283759700
                                CASE "CYAN"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4283759868
                                CASE "ORANGE"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294726740
                                CASE "MAGENTA"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294726908
                                CASE "YELLOW"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294769748
                                CASE "WHITE"
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4294769916
                                CASE ELSE
                                    textbox_color(handle, textboxes(handle).colors, 3) = 4278190080
                            END SELECT
                        END IF
                    END IF
                END IF
            CASE "l"
                textboxes(handle).links = textboxes(handle).links + 1
                textbox_link(handle, textboxes(handle).links, 1) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
                textbox_link(handle, textboxes(handle).links, 2) = textbox_index_original(handle, textboxes(handle).textstrings) + LEN(text)
                movetext = movetext + 2
                valuesave = ""
                DO
                    movetext = movetext + 1
                    IF MID$(text, movetext, 1) = ">" THEN EXIT DO
                    valuesave = valuesave + MID$(text, movetext, 1)
                LOOP UNTIL movetext >= LEN(text)
                IF movetext < LEN(text) THEN
                    textbox_link(handle, textboxes(handle).links, 3) = VAL(valuesave)
                ELSE
                    textbox_link(handle, textboxes(handle).links, 3) = textboxes(handle).links
                END IF
                textboxes(handle).colors = textboxes(handle).colors + 1
                textbox_color(handle, textboxes(handle).colors, 1) = textbox_link(handle, textboxes(handle).links, 1)
                textbox_color(handle, textboxes(handle).colors, 2) = textbox_link(handle, textboxes(handle).links, 2)
                textbox_color(handle, textboxes(handle).colors, 3) = textboxes(handle).linkstand
                textbox_link(handle, textboxes(handle).links, 4) = textboxes(handle).colors
                textbox_link(handle, textboxes(handle).links, 5) = TEXTBOX_LINKSTATUS_STAND
        END SELECT
    ELSE IF MID$(text, movetext, 3) = "</#" THEN
            SELECT CASE MID$(text, movetext + 3, 1)
                CASE "c"
                    FOR terminateindex = textboxes(handle).colors TO 1 STEP -1
                        linkcolor = 0
                        FOR checklinks = 1 TO textboxes(handle).links
                            IF textbox_link(handle, checklinks, 4) = terminateindex THEN
                                linkcolor = -1
                                EXIT FOR
                            END IF
                        NEXT checklinks
                        IF NOT linkcolor THEN
                            IF textbox_color(handle, terminateindex, 2) = textbox_index_original(handle, textboxes(handle).textstrings) + LEN(text) THEN
                                textbox_color(handle, terminateindex, 2) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
                                EXIT FOR
                            END IF
                        END IF
                    NEXT terminateindex
                    movetext = movetext + 4
                CASE "l"
                    FOR terminateindex = textboxes(handle).links TO 1 STEP -1
                        IF textbox_link(handle, terminateindex, 2) = textbox_index_original(handle, textboxes(handle).textstrings) + LEN(text) THEN
                            textbox_link(handle, terminateindex, 2) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
                            textbox_color(handle, textbox_link(handle, terminateindex, 4), 2) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
                            EXIT FOR
                        END IF
                    NEXT terminateindex
                    movetext = movetext + 4
            END SELECT
        ELSE
            textbox_text(handle) = textbox_text(handle) + MID$(text, movetext, 1)
            textbox_index_original(handle, textboxes(handle).textstrings + 1) = textbox_index_original(handle, textboxes(handle).textstrings + 1) + 1
        END IF
    END IF
NEXT movetext
TEXTBOXFORMATTEXT handle, textboxes(handle).textstrings, tickspeed
END SUB

SUB TEXTBOXBRINGTOFRONT (handle AS INTEGER)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
TEXTBOXSETQUEUE handle, UBOUND(textboxes)
END SUB

SUB TEXTBOXCLEARTEXT (handle)
DIM cleartextboxindex AS INTEGER
DIM cleartextboxindexoriginal AS INTEGER
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXCLEARTEXT handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        FOR cleartextboxindex = 1 TO textboxes(handle).textlines
            textbox_index(handle, cleartextboxindex) = 0
        NEXT cleartextboxindex
        FOR cleartextboxindexoriginal = 1 TO textboxes(handle).textstrings
            textbox_index_original(handle, cleartextboxindexoriginal) = 0
        NEXT cleartextboxindexoriginal
        textbox_text(handle) = ""
        textboxes(handle).textstrings = 0
        textboxes(handle).textlines = 0
        textboxes(handle).screenline = 0
END SELECT
END SUB

SUB TEXTBOXFORMATTEXT (handle AS INTEGER, textstring AS INTEGER, tickspeed AS _BYTE)
DIM formattext AS INTEGER
DIM newlines AS INTEGER
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF textboxes(handle).ticktimer > 0 THEN
    DO
        TEXTBOXTICK handle
    LOOP UNTIL textboxes(handle).ticktimer = 0
END IF
IF tickspeed > 0 THEN
    textboxes(handle).tickstring = MID$(textbox_text(handle), textbox_index_original(handle, textstring), textbox_index_original(handle, textstring + 1) - textbox_index_original(handle, textstring))
    textboxes(handle).tickindex = 2
    IF textboxes(handle).textlines = 0 THEN textboxes(handle).tickindex = textboxes(handle).tickindex - 1
    textboxes(handle).textlines = textboxes(handle).textlines + 1
    IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
    IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
        IF textboxes(handle).textlines > textboxes(handle).rows THEN
            textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
        ELSE
            textboxes(handle).screenline = 1
        END IF
    END IF
    textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines) + 1
    textboxes(handle).ticktimer = _FREETIMER
    ON TIMER(textboxes(handle).ticktimer, 1 / tickspeed) TEXTBOXTICK handle
    TIMER(textboxes(handle).ticktimer) ON
ELSE
    newlines = textboxes(handle).textlines
    textboxes(handle).textlines = textboxes(handle).textlines + 1
    textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index_original(handle, textstring) + textboxes(handle).cols - 2
    DO
        IF textbox_index(handle, textboxes(handle).textlines + 1) > textbox_index_original(handle, textstring + 1) THEN
            textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index_original(handle, textstring + 1) + 1
            EXIT DO
        ELSE
            FOR formattext = textbox_index(handle, textboxes(handle).textlines + 1) TO textbox_index(handle, textboxes(handle).textlines) STEP -1
                IF MID$(textbox_text(handle), formattext, 1) = CHR$(32) THEN
                    textbox_index(handle, textboxes(handle).textlines + 1) = formattext + 1
                    EXIT FOR
                END IF
            NEXT formattext
            IF formattext = textbox_index(handle, textboxes(handle).textlines) THEN textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines) + textboxes(handle).cols - 1
        END IF
        textboxes(handle).textlines = textboxes(handle).textlines + 1
        textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines) + textboxes(handle).cols - 2
    LOOP
    newlines = textboxes(handle).textlines - newlines
    IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + newlines
    IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
        IF textboxes(handle).textlines > textboxes(handle).rows THEN
            textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
        ELSE
            textboxes(handle).screenline = 1
        END IF
    END IF
END IF
END SUB

SUB TEXTBOXREFORMATTEXT (handle AS INTEGER)
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
DIM reformattext AS INTEGER
DIM restoreline AS INTEGER
DIM saveline AS INTEGER
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
saveline = textbox_index(handle, textboxes(handle).screenline)
textboxes(handle).textlines = 0
FOR reformattext = 1 TO textboxes(handle).textstrings
    TEXTBOXFORMATTEXT handle, reformattext, 0
NEXT reformattext
FOR restoreline = textboxes(handle).textlines TO 1 STEP -1
    IF textbox_index(handle, restoreline + 1) > saveline AND textbox_index(handle, restoreline) < saveline THEN textboxes(handle).screenline = restoreline
NEXT restoreline
FOR clearoldindex = textboxes(handle).textlines + 2 TO 32640
    textbox_index(handle, clearoldindex) = 0
NEXT clearoldindex
END SUB

SUB TEXTBOXRESIZE (handle AS INTEGER, left AS LONG, top AS LONG, right AS LONG, bottom AS LONG)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF left > 1 THEN textboxes(handle).area.left = left
IF top > 1 THEN textboxes(handle).area.top = top
IF right > 1 THEN textboxes(handle).area.right = right
IF bottom > 1 THEN textboxes(handle).area.bottom = bottom
IF textboxes(handle).background THEN _FREEIMAGE textboxes(handle).background
textboxes(handle).background = TEXTBOXBACKGROUND(handle)
IF textboxes(handle).custombackground THEN TEXTBOXSETBACKGROUND handle, textboxes(handle).custombackground, textboxes(handle).custombackgroundbehaviour
textboxes(handle).rows = INT((textboxes(handle).area.bottom - textboxes(handle).area.top - 8) / _FONTHEIGHT(textboxes(handle).font))
textboxes(handle).cols = INT(((textboxes(handle).area.right - textboxes(handle).area.left) / _FONTWIDTH(textboxes(handle).font)) + (2 * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)))
TEXTBOXREFORMATTEXT handle
END SUB

SUB TEXTBOXSENDTOBACK (handle AS INTEGER)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
TEXTBOXSETQUEUE handle, 1
END SUB

SUB TEXTBOXSETBACKGROUND (handle AS INTEGER, background AS LONG, behaviour AS _BYTE)
DIM scale AS SINGLE
DIM hd AS LONG
DIM hs AS LONG
DIM wd AS LONG
DIM ws AS LONG
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF background < -1 THEN
    textboxes(handle).custombackground = background
    textboxes(handle).custombackgroundbehaviour = behaviour
    IF textboxes(handle).background THEN
        _FREEIMAGE textboxes(handle).background
        textboxes(handle).background = TEXTBOXBACKGROUND(handle)
    END IF
    wd = _WIDTH(textboxes(handle).background)
    hd = _HEIGHT(textboxes(handle).background)
    ws = _WIDTH(background)
    hs = _HEIGHT(background)
    SELECT CASE behaviour
        CASE TEXTBOX_BACKGROUND_STRETCH
            stretch:
            _PUTIMAGE (0, 0)-(wd, hd), background, textboxes(handle).background
        CASE TEXTBOX_BACKGROUND_FIT
            fit:
            IF ws > hs THEN
                scale = (wd / ws) * hs
                _PUTIMAGE (0, hd - scale)-STEP(wd, scale), background, textboxes(handle).background
            ELSE
                scale = (hd / hs) * ws
                _PUTIMAGE (wd - scale, 0)-STEP(scale, hd), background, textboxes(handle).background
            END IF
        CASE TEXTBOX_BACKGROUND_CENTER
            center:
            IF ws <= wd AND hs <= hd THEN
                _PUTIMAGE((wd - ws) / 2, (hd - hs) / 2), background, textboxes(handle).background
            ELSE
                GOTO fit
            END IF
    END SELECT
END IF
END SUB

SUB TEXTBOXSETQUEUE (handle AS INTEGER, queue AS INTEGER)
DIM findqueue AS INTEGER
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF queue < 1 OR queue > UBOUND(textboxes) THEN EXIT SUB
FOR findqueue = 1 TO UBOUND(textboxes)
    IF textboxes(findqueue).queue = queue THEN SWAP textboxes(handle).queue, textboxes(findqueue).queue
NEXT findqueue
END SUB

SUB TEXTBOXSETTICK (handle AS INTEGER, tick AS LONG)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXSETTICK handle, tick
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF tick THEN textboxes(handle).tick = tick
END SELECT
END SUB

SUB TEXTBOXTICK (handle AS INTEGER)
DIM findnextspace AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
textboxes(handle).tickindex = textboxes(handle).tickindex + 1
IF MID$(textboxes(handle).tickstring, textboxes(handle).tickindex - 1, 1) = " " THEN
    DO
        FOR findnextspace = textboxes(handle).tickindex + 0 TO LEN(RTRIM$(textboxes(handle).tickstring))
            IF MID$(textboxes(handle).tickstring, findnextspace, 1) = " " THEN
                IF textboxes(handle).cols - 2 - (textbox_index(handle, textboxes(handle).textlines + 1) - textbox_index(handle, textboxes(handle).textlines)) <= findnextspace - textboxes(handle).tickindex THEN
                    IF textboxes(handle).textlines = 1 THEN textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines + 1) + 1
                    textboxes(handle).textlines = textboxes(handle).textlines + 1
                    textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines)
                    IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
                    IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
                        IF textboxes(handle).textlines > textboxes(handle).rows THEN
                            textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
                        ELSE
                            textboxes(handle).screenline = 1
                        END IF
                    END IF
                END IF
                EXIT DO
            END IF
        NEXT findnextspace
        IF textboxes(handle).cols - 2 - (textbox_index(handle, textboxes(handle).textlines + 1) - textbox_index(handle, textboxes(handle).textlines)) <= LEN(RTRIM$(textboxes(handle).tickstring)) - textboxes(handle).tickindex THEN
            textboxes(handle).textlines = textboxes(handle).textlines + 1
            textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines)
            IF textboxes(handle).type = TEXTBOXTEXT_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
            IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
                IF textboxes(handle).textlines > textboxes(handle).rows THEN
                    textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
                ELSE
                    textboxes(handle).screenline = 1
                END IF
            END IF
        END IF
        EXIT DO
    LOOP
END IF
textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines + 1) + 1
IF textboxes(handle).tick THEN _SNDPLAY textboxes(handle).tick
IF textboxes(handle).tickindex >= LEN(RTRIM$(textboxes(handle).tickstring)) THEN
    textbox_index(handle, textboxes(handle).textlines + 1) = textbox_index(handle, textboxes(handle).textlines + 1) + 1
    TIMER(textboxes(handle).ticktimer) OFF
    TIMER(textboxes(handle).ticktimer) FREE
    textboxes(handle).ticktimer = 0
END IF
END SUB

SUB SCROLLBARHIDE (handle AS INTEGER)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SCROLLBARHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_OFF
        textboxes(handle).cols = INT(((textboxes(handle).area.right - textboxes(handle).area.left) / _FONTWIDTH(textboxes(handle).font)) + (2 * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)))
        TEXTBOXREFORMATTEXT handle
END SELECT
END SUB

SUB SCROLLBARSET (handle AS INTEGER, scollbarcolor AS _UNSIGNED LONG, scrollbartype AS _BYTE, scrollbarline AS _BYTE, scrollbarbox AS _BYTE)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SCROLLBARHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF scrollbarcolor <> 1 THEN
            IF scrollbarcolor THEN
                textboxes(handle).scrollbarcolor = scrollbarcolor
            ELSE
                IF textboxes(handle).frame > TEXTBOX_NOFRAME THEN
                    textboxes(handle).scrollbarcolor = textboxes(handle).frame
                ELSE
                    textboxes(handle).scrollbarcolor = TEXTBOX_DEFAULTCOLOR
                END IF
            END IF
        END IF
        IF scrollbartype > 0 THEN textboxes(handle).scrollbartype = scrollbartype
        IF scrollbarline < 1 THEN textboxes(handle).scrollbarline = scrollbarline
        IF scrollbarbox < 1 THEN textboxes(handle).scrollbarbox = scrollbarbox
END SELECT
END SUB

SUB SCROLLBARSHOW (handle AS INTEGER)
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SCROLLBARHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON
        textboxes(handle).rows = INT((textboxes(handle).area.bottom - textboxes(handle).area.top) / _FONTHEIGHT(textboxes(handle).font))
        textboxes(handle).cols = INT(((textboxes(handle).area.right - textboxes(handle).area.left) / _FONTWIDTH(textboxes(handle).font)) + (2 * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)))
        TEXTBOXREFORMATTEXT handle
END SELECT
END SUB

SUB SWAPTEXTBOX (handle1 AS INTEGER, handle2 AS INTEGER)
DIM swapcolory AS _BYTE
DIM swaplinky AS _BYTE
DIM swapcolorx AS INTEGER
DIM swapindex AS INTEGER
DIM swapindexoriginal AS INTEGER
DIM swaplinkx AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle1) THEN EXIT SUB
IF NOT VALIDTEXTBOX(handle2) THEN EXIT SUB
OPEN "temp\textboxswap.tmp" FOR BINARY AS #1
PUT #1, , textboxes(handle1)
PUT #1, , textboxes(handle2)
SEEK #1, 1
GET #1, , textboxes(handle2)
GET #1, , textboxes(handle1)
CLOSE #1
KILL "temp\textboxswap.tmp"
FOR swapindex = -127 TO 32640
    SWAP textbox_index(handle1, swapindex), textbox_index(handle2, swapindex)
    IF textbox_index(handle1, swapindex) = 0 AND textbox_index(handle2, swapindex) = 0 AND swapindex > 1 THEN EXIT FOR
NEXT swapindex
FOR swapindexoriginal = 1 TO 32768
    SWAP textbox_index_original(handle1, swapindexoriginal), textbox_index_original(handle2, swapindexoriginal)
    IF textbox_index_original(handle1, swapindexoriginal) = 0 AND textbox_index_original(handle2, swapindexoriginal) = 0 AND swapindexoriginal > 1 THEN EXIT FOR
NEXT swapindexoriginal
FOR swapcolorx = 1 TO 1024
    FOR swapcolory = 1 TO 3
        SWAP textbox_color(handle1, swapcolorx, swapcolory), textbox_color(handle2, swapcolorx, swapcolory)
    NEXT swapcolory
    IF textbox_color(handle1, swapcolorx, 1) = 0 AND textbox_color(handle2, swapcolorx, 1) = 0 AND swapcolorx > 1 THEN EXIT FOR
NEXT swapcolorx
FOR swaplinkx = 1 TO 1024
    FOR swaplinky = 1 TO 5
        SWAP textbox_link(handle1, swaplinkx, swaplinky), textbox_link(handle2, swaplinkx, swaplinky)
    NEXT swaplinky
    IF textbox_link(handle1, swaplinkx, 1) = 0 AND textbox_link(handle2, swaplinkx, 1) = 0 AND swaplinkx > 1 THEN EXIT FOR
NEXT swaplinkx
SWAP textbox_text(handle1), textbox_text(handle2)
END SUB

SUB UPDATETEXTBOX (handle AS INTEGER)
DIM linkvalue AS _BYTE
DIM checklines AS INTEGER
DIM endline AS INTEGER
DIM updatelinks AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            UPDATETEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        DO
            SELECT CASE textboxes(handle).input
                CASE TEXTBOX_INPUT_DIRECT
                    IF NOT (isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left, textboxes(handle).area.top, textboxes(handle).area.right, textboxes(handle).area.bottom)) THEN EXIT DO
                CASE TEXTBOX_INPUT_INDIRECT
                    IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left, textboxes(handle).area.top, textboxes(handle).area.right, textboxes(handle).area.bottom) THEN EXIT DO
            END SELECT
            textboxes(handle).screenline = textboxes(handle).screenline + Mousedata(8) * setting.scrollmultiplier
            IF Akey(1) THEN textboxes(handle).screenline = textboxes(handle).screenline - 1
            IF Akey(4) THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
            IF Pup THEN textboxes(handle).screenline = textboxes(handle).screenline - textboxes(handle).rows + 1
            IF Pdn THEN textboxes(handle).screenline = textboxes(handle).screenline + textboxes(handle).rows - 1
            IF Hme THEN textboxes(handle).screenline = 1
            IF Edk THEN textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
            IF K$ > "" AND textboxes(handle).ticktimer > 0 THEN
                DO
                    TEXTBOXTICK handle
                LOOP UNTIL textboxes(handle).ticktimer = 0
            END IF
            EXIT DO
        LOOP
        IF textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON AND isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.right - (2 * (_FONTWIDTH(textboxes(handle).font))) + 1, textboxes(handle).area.top - 1, textboxes(handle).area.right - 1, textboxes(handle).area.bottom + 1) AND Click THEN textboxes(handle).scrollbargrab = -1
        IF NOT Click THEN textboxes(handle).scrollbargrab = 0
        IF textboxes(handle).scrollbargrab THEN textboxes(handle).screenline = (Mousedata(2) - textboxes(handle).area.top - 2) / (textboxes(handle).area.bottom - textboxes(handle).area.top - 4) * (textboxes(handle).textlines - textboxes(handle).rows + 1)
        IF textboxes(handle).screenline < 1 THEN textboxes(handle).screenline = 1
        IF textboxes(handle).screenline > textboxes(handle).textlines - textboxes(handle).rows + 1 THEN textboxes(handle).screenline = textboxes(handle).textlines - textboxes(handle).rows + 1
        IF textboxes(handle).textlines <= textboxes(handle).rows THEN
            IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE OR textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
                textboxes(handle).screenline = -textboxes(handle).rows + textboxes(handle).textlines + 1
            ELSE
                textboxes(handle).screenline = 1
            END IF
        END IF
        IF textboxes(handle).link = TEXTBOX_LINK_ON AND (Movemouse OR Clickchange) THEN
            endline = textboxes(handle).screenline + textboxes(handle).rows - 1
            IF textboxes(handle).textlines < textboxes(handle).screenline + textboxes(handle).rows THEN endline = textboxes(handle).screenline + textboxes(handle).rows
            FOR updatelinks = 1 TO textboxes(handle).links
                linkvalue = TEXTBOX_LINKSTATUS_STAND
                FOR checklines = textboxes(handle).screenline TO endline
                    IF NOT ((textbox_link(handle, updatelinks, 1) < textbox_index(handle, checklines) AND textbox_link(handle, updatelinks, 2) < textbox_index(handle, checklines)) OR (textbox_link(handle, updatelinks, 1) > textbox_index(handle, checklines + 1) AND textbox_link(handle, updatelinks, 2) > textbox_index(handle, checklines + 1))) AND textbox_link(handle, updatelinks, 3) > TEXTBOX_LINK_OFF THEN
                        IF textbox_link(handle, updatelinks, 1) < textbox_index(handle, checklines) THEN
                            IF textbox_link(handle, updatelinks, 2) > textbox_index(handle, checklines + 1) THEN
                                IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left + 8, textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + 8 + (textbox_index(handle, checklines + 1) - textbox_index(handle, checklines) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF Click THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            ELSE
                                IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left + 8, textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + 8 + (textbox_link(handle, updatelinks, 2) - textbox_index(handle, checklines)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) + 1) THEN
                                    IF Click THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            END IF
                        ELSE
                            IF textbox_link(handle, updatelinks, 2) > textbox_index(handle, checklines + 1) THEN
                                IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left + 8 + (textbox_link(handle, updatelinks, 1) - textbox_index(handle, checklines)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + 8 + (textbox_index(handle, checklines + 1) - textbox_index(handle, checklines) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF Click THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            ELSE
                                IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left + 8 + (textbox_link(handle, updatelinks, 1) - textbox_index(handle, checklines) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + 8 + (textbox_link(handle, updatelinks, 2) - textbox_index(handle, checklines) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + 8 + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF Click THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            END IF
                        END IF
                    END IF
                NEXT checklines
                textbox_link(handle, updatelinks, 5) = linkvalue
                SELECT CASE linkvalue
                    CASE TEXTBOX_LINKSTATUS_STAND
                        textbox_color(handle, textbox_link(handle, updatelinks, 4), 3) = textboxes(handle).linkstand
                    CASE TEXTBOX_LINKSTATUS_HOVER
                        textbox_color(handle, textbox_link(handle, updatelinks, 4), 3) = textboxes(handle).linkhover
                    CASE TEXTBOX_LINKSTATUS_ACTIVE
                        textbox_color(handle, textbox_link(handle, updatelinks, 4), 3) = textboxes(handle).linkactive
                END SELECT
            NEXT updatelinks
        END IF
END SELECT
END SUB

FUNCTION CLONETEXTBOX% (handle AS INTEGER)
DIM clonecolory AS _BYTE
DIM clonelinky AS _BYTE
DIM clonecolorx AS INTEGER
DIM cloneindex AS INTEGER
DIM cloneindexoriginal AS INTEGER
DIM clonelinkx AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
DO
    IF UBOUND(textboxes) THEN
        FOR CLONETEXTBOX = 1 TO UBOUND(textboxes)
            IF textboxes(CLONETEXTBOX).inuse = 0 THEN EXIT DO
        NEXT CLONETEXTBOX
    END IF
    CLONETEXTBOX = CLONETEXTBOX + 1
    REDIM _PRESERVE textbox_index(CLONETEXTBOX, -127 TO 32640) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_index_original(CLONETEXTBOX, 32768) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_color(CLONETEXTBOX, 1024, 3) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_link(CLONETEXTBOX, 1024, 5) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_text(CLONETEXTBOX) AS STRING
    REDIM _PRESERVE textboxes(CLONETEXTBOX) AS textbox
    EXIT DO
LOOP
OPEN "temp\clonetextbox.tmp" FOR BINARY AS #1
PUT #1, , textboxes(handle)
SEEK #1, 1
GET #1, , textboxes(CLONETEXTBOX)
CLOSE #1
FOR cloneindex = -127 TO 32640
    textbox_index(CLONETEXTBOX, cloneindex) = textbox_index(handle, cloneindex)
    IF textbox_index(CLONETEXTBOX, cloneindex) = 0 AND textbox_index(handle, cloneindex) = 0 AND cloneindex > 1 THEN EXIT FOR
NEXT cloneindex
FOR cloneindexoriginal = 1 TO 32768
    textbox_index_original(CLONETEXTBOX, cloneindexoriginal) = textbox_index_original(handle, cloneindexoriginal)
    IF textbox_index_original(CLONETEXTBOX, cloneindexoriginal) = 0 AND textbox_index_original(handle, cloneindexoriginal) = 0 AND cloneindexoriginal > 1 THEN EXIT FOR
NEXT cloneindexoriginal
FOR clonecolorx = 1 TO 1024
    FOR clonecolory = 1 TO 3
        textbox_color(CLONETEXTBOX, clonecolorx, clonecolory) = textbox_color(handle, clonecolorx, clonecolory)
    NEXT clonecolory
    IF textbox_color(CLONETEXTBOX, clonecolorx, 1) = 0 AND textbox_color(handle, clonecolorx, 1) = 0 AND clonecolorx > 1 THEN EXIT FOR
NEXT clonecolorx
FOR clonelinkx = 1 TO 1024
    FOR clonelinky = 1 TO 5
        textbox_link(CLONETEXTBOX, clonelinkx, clonelinky) = textbox_link(handle, clonelinkx, clonelinky)
    NEXT clonelinky
    IF textbox_link(CLONETEXTBOX, clonelinkx, 1) = 0 AND textbox_link(handle, clonelinkx, 1) = 0 AND clonelinkx > 1 THEN EXIT FOR
NEXT clonelinkx
textbox_text(CLONETEXTBOX) = textbox_text(handle)
END FUNCTION

FUNCTION LINKSTATUS%% (handle AS INTEGER, linkcode AS INTEGER)
DIM findlink AS INTEGER
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
FOR findlink = 1 TO textboxes(handle).links
    IF textbox_link(handle, findlink, 3) = linkcode THEN
        LINKSTATUS = textbox_link(handle, findlink, 5)
        textbox_link(handle, findlink, 5) = TEXTBOX_LINKSTATUS_STAND
        EXIT FOR
    END IF
NEXT findlink
END FUNCTION

FUNCTION NEWTEXTBOX% (left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, status AS _BYTE, boxtype AS _BYTE, inputtype AS _BYTE, dest AS LONG, font AS LONG, fontcolor AS _UNSIGNED LONG, backgroundcolor AS _UNSIGNED LONG, frame AS _UNSIGNED LONG, scrollbar AS _BYTE, link AS _BYTE)
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_index_original() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS textbox
IF status < TEXTBOX_SHOW THEN status = TEXTBOX_SHOW
IF status > TEXTBOX_HIDE THEN status = TEXTBOX_HIDE
IF behaviour < TEXTBOX_TEXT_STATIC THEN behaviour = TEXTBOX_TEXT_STATIC
IF behaviour > TEXTBOX_TITLE THEN behaviour = TEXTBOX_TITLE
IF scrollbar < TEXTBOX_SCROLLBAR_ON THEN scrollbar = TEXTBOX_SCROLLBAR_ON
IF scrollbar > TEXTBOX_SCROLLBAR_OFF THEN scrollbar = TEXTBOX_SCROLLBAR_OFF
DO
    IF UBOUND(textboxes) THEN
        FOR NEWTEXTBOX = 1 TO UBOUND(textboxes)
            IF textboxes(NEWTEXTBOX).inuse = 0 THEN EXIT DO
        NEXT NEWTEXTBOX
    END IF
    NEWTEXTBOX = NEWTEXTBOX + 1
    REDIM _PRESERVE textbox_index(NEWTEXTBOX, -127 TO 32640) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_index_original(NEWTEXTBOX, 32768) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_color(NEWTEXTBOX, 1024, 3) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_link(NEWTEXTBOX, 1024, 5) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_text(NEWTEXTBOX) AS STRING
    REDIM _PRESERVE textboxes(NEWTEXTBOX) AS textbox
    EXIT DO
LOOP
textboxes(NEWTEXTBOX).inuse = -1
textboxes(NEWTEXTBOX).queue = NEWTEXTBOX
textboxes(NEWTEXTBOX).status = status
textboxes(NEWTEXTBOX).type = boxtype
textboxes(NEWTEXTBOX).behaviour = behaviour
textboxes(NEWTEXTBOX).input = inputtype
textboxes(NEWTEXTBOX).area.left = left
textboxes(NEWTEXTBOX).area.top = top
textboxes(NEWTEXTBOX).area.right = right
textboxes(NEWTEXTBOX).area.bottom = bottom
textboxes(NEWTEXTBOX).dest = dest
textboxes(NEWTEXTBOX).font = font
textboxes(NEWTEXTBOX).color = fontcolor
textboxes(NEWTEXTBOX).backgroundcolor = backgroundcolor
textboxes(NEWTEXTBOX).background = TEXTBOXBACKGROUND(NEWTEXTBOX)
textboxes(NEWTEXTBOX).custombackground = 0
textboxes(NEWTEXTBOX).custombackgroundbehaviour = 0
textboxes(NEWTEXTBOX).scrollbar = scrollbar
IF scrollbar = TEXTBOX_SCROLLBAR_ON THEN SCROLLBARSET NEWTEXTBOX, 0, TEXTBOX_SCROLLBAR_BOX_DEFAULT, TEXTBOX_SCROLLBAR_NOLINE, TEXTBOX_SCROLLBAR_BOX
textboxes(NEWTEXTBOX).link = link
LINKSET NEWTEXTBOX, TEXTBOX_LINK_STAND, TEXTBOX_LINK_HOVER, TEXTBOX_LINK_ACTIVE, TEXTBOX_LINK_UNDERLINE_ON
textboxes(NEWTEXTBOX).frame = frame
textboxes(NEWTEXTBOX).textstrings = 0
textboxes(NEWTEXTBOX).textlines = 0
textboxes(NEWTEXTBOX).screenline = 1
textboxes(NEWTEXTBOX).rows = INT((bottom - top - 8) / _FONTHEIGHT(font))
textboxes(NEWTEXTBOX).cols = INT(((right - left) / _FONTWIDTH(font)) + (2 * (scrollbar = TEXTBOX_SCROLLBAR_ON)))
textboxes(NEWTEXTBOX).ticktimer = 0
textboxes(NEWTEXTBOX).tick = 0
END FUNCTION

FUNCTION TEXTBOXBACKGROUND& (handle AS INTEGER)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXBACKGROUND& = _NEWIMAGE(textboxes(handle).area.right - textboxes(handle).area.left, textboxes(handle).area.bottom - textboxes(handle).area.top, 32)
_DEST TEXTBOXBACKGROUND&
PAINT (1, 1), textboxes(handle).backgroundcolor
_DEST 0
END FUNCTION

FUNCTION TEXTBOXQUEUE% (handle AS INTEGER)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXQUEUE = textboxes(handle).queue
END FUNCTION

FUNCTION TEXTBOXSTATUS` (handle AS INTEGER)
SHARED textboxes() AS textbox
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXSTATUS = textboxes(handle).status
END FUNCTION

FUNCTION VALIDTEXTBOX` (handle AS INTEGER)
SHARED textboxes() AS textbox
IF handle = 0 THEN EXIT FUNCTION
IF handle > UBOUND(textboxes) THEN EXIT FUNCTION
IF textboxes(handle).inuse = 0 THEN EXIT FUNCTION
VALIDTEXTBOX = -1
END FUNCTION