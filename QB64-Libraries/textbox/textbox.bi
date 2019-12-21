'******************************************************************************
'
'Gorlock's QB64 Textbox Library
'v0.51
'
'******************************************************************************
'
'Version 0.1  - 2013-09-21
'Version 0.2  - 2013-09-25
'  - added tick, queue, swap, and clone commands
'  - added custom background capabilities
'Version 0.21 - 2013-10-04
'  - added tutorial and init include file
'Version 0.3  - 2014-01-06
'  - fixed arrays to exapand properly
'  - addes images in textboxes
'Version 0.4  - 2014-01-28
'  - added ability to wrap text to images
'  - compensated images and textboxes after using TEXTBOXRESIZE
'Version 0.5  - 2013-02-17
'  - added nested textboxes
'  - added images and bookmarks to TBML
'Version 0.51 - 2013-02-23
'  - fixed minor errors
'
'Future Updates
'  - allow resizing with sub-textboxes
'  - add more elements to TBML
'
'******************************************************************************
'
'SUB       HIDETEXTBOX           (handle%)                                                                                                                                          Hides a textbox
'SUB       LINKHIDE              (handle%)                                                                                                                                          Hides links in a textbox
'SUB       LINKSET               (handle%, standcolor~&, hovercolor~&, activecolor~&, linkul%%)                                                                                     Sets metrics for a textbox's links
'SUB       LINKSHOW              (handle%)                                                                                                                                          Shows links in a textbox
'SUB       PRINTTEXTBOX          (handle%)                                                                                                                                          Prints a textbox to the screen
'SUB       SCROLLBARHIDE         (handle%)                                                                                                                                          Hides the scrollbar
'SUB       SCROLLBARSET          (handle%, scrollbarcolor~&, scrollbartype%%, scrollbarline%%, scrollbarbox%%)                                                                       Sets metrics for a textbox's scrollbar
'SUB       SCROLLBARSHOW         (handle%)                                                                                                                                          Shows the scrollbar
'SUB       SHOWTEXTBOX           (handle%)                                                                                                                                          Shows a textbox that had been hidden
'SUB       SWAPTEXTBOX           (handle1%, handle2%)                                                                                                                               Swaps the position of two textboxes in the array
'SUB       TEXTBOXADDIMAGE       (handle%, row%, left&, top&, right&, bottom&, image&, border~&, behaviour%%)                                                                       Adds an image to a textbox
'SUB       TEXTBOXADDTEXT        (handle%, text$, tickspeed%%)                                                                                                                      Adds text to a textbox
'SUB       TEXTBOXBOUND          (handle%, left&, top&, right&, bottom&)                                                                                                            Sets the boundary area for a textbox
'SUB       TEXTBOXBOUNDOFF       (handle%)                                                                                                                                          Turns on the bound of a textbox
'SUB       TEXTBOXBOUNDON        (handle%)                                                                                                                                          Turns off the bound of a textbox
'SUB       TEXTBOXBRINGTOFRONT   (handle%)                                                                                                                                          Moves a textbox to the font of the queue
'SUB       TEXTBOXCLEAR          (handle%)                                                                                                                                          Clears a textbox
'SUB       TEXTBOXCLEARTEXT      (handle%)                                                                                                                                          Clears the text from a textbox
'SUB       TEXTBOXFORMATTEXT     (handle%, textstring%, tickspeed%%)                                                                                                                Formats a textstring
'SUB       TEXTBOXFREE           (handle%)                                                                                                                                          Frees a textbox handle
'SUB       TEXTBOXREFORMAT       (handle%)                                                                                                                                          Reformats a textbox
'SUB       TEXTBOXREFORMATTEXT   (handle%)                                                                                                                                          Reformats all the text in a textbox
'SUB       TEXTBOXRESIZE         (handle%, left&, top&, right&, bottom&)                                                                                                            Fits a textbox to a different area
'SUB       TEXTBOXSENDTOBACK     (handle%)                                                                                                                                          Moves a textbox to the back of the queue
'SUB       TEXTBOXSETBACKGROUND  (handle%, background&, behaviour%%)                                                                                                                Sets a custom background for a textbox
'SUB       TEXTBOXSETQUEUE       (handle%, queue%)                                                                                                                                  Sets a textbox's queue to a certain value
'SUB       TEXTBOXSETTICK        (handle%, tick&)                                                                                                                                   Sets the sound for ticking of a textbox
'SUB       TEXTBOXTICK           (handle%)                                                                                                                                          Progresses the autotyping of a textbox
'SUB       UPDATETEXTBOX         (handle%)                                                                                                                                          Updates a textbox
'FUNCTION  CLONETEXTBOX%         (handle%)                                                                                                                                          Creates a copy of a textbox
'FUNCTION  LINKSTATUS%%          (handle%, linkcode%)                                                                                                                               Returns a link's status
'FUNCTION  NEWTEXTBOX%           (left&, top&, right&, bottom&, status%%, boxtype%%, inputtype%%, dest&, font&, fontcolor~&, backgroundcolor~&, framecolor~&, scrollbar%%, link%%)  Creates a new textbox
'FUNCTION  TEXTBOXADDTEXTBOX%    (handle%, row%, left&, top&, right&, bottom&, boxtype&, fontcolor~&, backgroundcolor~&, framecolor~&, scrollbar%%, link%%)                         Creates a nested textbox
'FUNCTION  TEXTBOXBACKGROUND&    (handle%)                                                                                                                                          Creates a background image for a textbox
'FUNCTION  TEXTBOXQUEUE%         (handle%)                                                                                                                                          Returns a textbox's value in the queue
'FUNCTION  TEXTBOXSTATUS`        (handle%)                                                                                                                                          Returns the hidden status of a textbox
'FUNCTION  VALIDTEXTBOX`         (handle%)                                                                                                                                          Returns if a textbox handle is valid of not
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/WTJ8A1NG
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'This library requires Gorlock's QB64 Input Library: https://db.tt/DffOJN6z
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/agVZc8oq
'
'******************************************************************************

SUB HIDETEXTBOX (handle AS INTEGER)
DIM hidesubtextbox AS INTEGER
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            HIDETEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).status = TEXTBOX_HIDE
        FOR hidesubtextbox = 1 TO textboxes(handle).textboxes
            HIDETEXTBOX textbox_textbox(1, hidesubtextbox, handle)
        NEXT hidesubtextbox
END SELECT
END SUB

SUB LINKHIDE (handle AS INTEGER)
DIM hidelinks AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).link = TEXTBOX_LINK_OFF
        FOR hidelinks = 1 TO textboxes(handle).links
            textbox_color(3, textbox_link(4, hidelinks, handle), handle) = TEXTBOX_LINK_OFF
        NEXT hidelinks
END SELECT
END SUB

SUB LINKSET (handle AS INTEGER, standcolor AS _UNSIGNED LONG, hovercolor AS _UNSIGNED LONG, activecolor AS _UNSIGNED LONG, linkul AS _BYTE)
SHARED textboxes() AS TEXTBOX
IF linkul < TEXTBOX_LINK_UNDERLINE_ON THEN linkul = TEXTBOX_LINK_UNDERLINE_ON
IF linkul > TEXTBOX_LINK_UNDERLINE_OFF THEN linkul = TEXTBOX_LINK_UNDERLINE_OFF
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKSET handle, standcolor, hovercolor, activecolor, linkul
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF standcolor = TEXTBOX_KEEPCOLOR THEN standcolor = textboxes(handle).linkstand
        IF standcolor = 0 THEN standcolor = TEXTBOX_LINK_STAND
        IF hovercolor = TEXTBOX_KEEPCOLOR THEN hovercolor = textboxes(handle).linkhover
        IF hovercolor = 0 THEN hovercolor = TEXTBOX_LINK_HOVER
        IF activecolor = TEXTBOX_KEEPCOLOR THEN activecolor = textboxes(handle).linkactive
        IF activecolor = 0 THEN activecolor = TEXTBOX_LINK_ACTIVE
        textboxes(handle).linkstand = standcolor
        textboxes(handle).linkhover = hovercolor
        textboxes(handle).linkactive = activecolor
        textboxes(handle).linkul = linkul
END SELECT
END SUB

SUB LINKSHOW (handle AS INTEGER)
DIM showlinks AS INTEGER
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            LINKSHOW handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).link = TEXTBOX_LINK_ON
        FOR showlinks = 1 TO textboxes(handle).links
            textbox_color(3, textbox_link(4, showlinks, handle), handle) = TEXTBOX_LINK_STAND
        NEXT showlinks
END SELECT
END SUB

SUB PRINTTEXTBOX (handle AS INTEGER)
DIM thickenbox AS _BYTE
DIM checkcolors AS INTEGER
DIM checklinks AS INTEGER
DIM findsubtextbox AS INTEGER
DIM printbox AS INTEGER
DIM printimage AS INTEGER
DIM printlines AS INTEGER
DIM printsubtextbox AS INTEGER
DIM queue AS INTEGER
DIM i AS LONG
DIM savedest AS LONG
DIM endline AS _UNSIGNED LONG
DIM imagearea AS RECT
DIM pagearea AS RECT
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR queue = 1 TO UBOUND(textboxes)
            FOR handle = 1 TO UBOUND(textboxes)
                IF textboxes(handle).queue = queue THEN
                    IF textboxes(handle).subtextbox = FALSE THEN PRINTTEXTBOX handle
                    EXIT FOR
                END IF
            NEXT handle
        NEXT queue
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF textboxes(handle).status = TEXTBOX_SHOW THEN
            savedest = _DEST
            _DEST textboxes(handle).page
            CLS
            _PUTIMAGE (0, 0), textboxes(handle).background
            IF textboxes(handle).frame THEN LINE (0, 0)-((_WIDTH(textboxes(handle).page) - 1), (_HEIGHT(textboxes(handle).page) - 1)), textboxes(handle).frame, B
            IF textboxes(handle).font THEN _FONT textboxes(handle).font
            _PRINTMODE _KEEPBACKGROUND
            SELECT CASE textboxes(handle).type
                CASE TEXTBOX_TITLE
                    _PRINTSTRING ((_WIDTH(textboxes(handle).page) - 1) - _PRINTWIDTH(MID$(textbox_text(handle), textbox_index(1, handle), textbox_index(2, handle) - textbox_index(1, handle))) / 2, ((_HEIGHT(textboxes(handle).page) - 1) - _FONTHEIGHT(textboxes(handle).font)) / 2), MID$(textbox_text(handle), textbox_index(1, handle), textbox_index(2, handle) - textbox_index(1, handle))
                CASE ELSE
                    endline = textboxes(handle).screenline + textboxes(handle).rows - 1
                    IF (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) < textboxes(handle).screenline + textboxes(handle).rows THEN endline = textboxes(handle).screenline + textboxes(handle).rows
                    FOR printlines = textboxes(handle).screenline TO endline
                        COLOR textboxes(handle).color
                        _PRINTSTRING (_FONTWIDTH(textboxes(handle).font), _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(printlines, handle), textbox_index(printlines + 1, handle) - textbox_index(printlines, handle))
                        FOR checkcolors = 1 TO textboxes(handle).colors
                            IF NOT ((textbox_color(1, checkcolors, handle) < textbox_index(printlines, handle) AND textbox_color(2, checkcolors, handle) < textbox_index(printlines, handle)) OR (textbox_color(1, checkcolors, handle) > textbox_index(printlines + 1, handle) AND textbox_color(2, checkcolors, handle) > textbox_index(printlines + 1, handle))) AND textbox_color(3, checkcolors, handle) > TEXTBOX_LINK_OFF THEN
                                COLOR textbox_color(3, checkcolors, handle)
                                IF textbox_color(1, checkcolors, handle) < textbox_index(printlines, handle) THEN
                                    IF textbox_color(2, checkcolors, handle) > textbox_index(printlines + 1, handle) THEN
                                        _PRINTSTRING (_FONTWIDTH(textboxes(handle).font), _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(printlines, handle), textbox_index(printlines + 1, handle) - textbox_index(printlines, handle))
                                    ELSE
                                        _PRINTSTRING (_FONTWIDTH(textboxes(handle).font), _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_index(printlines, handle), textbox_color(2, checkcolors, handle) - textbox_index(printlines, handle))
                                    END IF
                                ELSE
                                    IF textbox_color(2, checkcolors, handle) > textbox_index(printlines + 1, handle) THEN
                                        _PRINTSTRING (_FONTWIDTH(textboxes(handle).font) + (textbox_color(1, checkcolors, handle) - textbox_index(printlines, handle) + 1 * (textbox_index(printlines, handle) = 0)) * _FONTWIDTH(textboxes(handle).font), _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_color(1, checkcolors, handle), textbox_index(printlines + 1, handle) - textbox_color(1, checkcolors, handle))
                                    ELSE
                                        _PRINTSTRING (_FONTWIDTH(textboxes(handle).font) + (textbox_color(1, checkcolors, handle) - textbox_index(printlines, handle) + 1 * (textbox_index(printlines, handle) = 0)) * _FONTWIDTH(textboxes(handle).font), _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (printlines - textboxes(handle).screenline)), MID$(textbox_text(handle), textbox_color(1, checkcolors, handle), textbox_color(2, checkcolors, handle) - textbox_color(1, checkcolors, handle))
                                    END IF
                                END IF
                            END IF
                        NEXT checkcolors
                        IF textboxes(handle).link = TEXTBOX_LINK_ON AND textboxes(handle).linkul = TEXTBOX_LINK_UNDERLINE_ON THEN
                            FOR checklinks = 1 TO textboxes(handle).links
                                IF NOT ((textbox_link(1, checklinks, handle) < textbox_index(printlines, handle) AND textbox_link(2, checklinks, handle) < textbox_index(printlines, handle)) OR (textbox_link(1, checklinks, handle) > textbox_index(printlines + 1, handle) AND textbox_link(2, checklinks, handle) > textbox_index(printlines + 1, handle))) AND textbox_link(3, checklinks, handle) > TEXTBOX_LINK_OFF THEN
                                    IF textbox_link(1, checklinks, handle) < textbox_index(printlines, handle) + 1 THEN
                                        IF textbox_link(2, checklinks, handle) > textbox_index(printlines + 1, handle) - 1 THEN
                                            LINE (_FONTWIDTH(textboxes(handle).font), 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_index(printlines + 1, handle) - textbox_index(printlines, handle) - 1) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(3, textbox_link(4, checklinks, handle), handle)
                                        ELSE
                                            LINE (_FONTWIDTH(textboxes(handle).font), 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_link(2, checklinks, handle) - textbox_index(printlines, handle)) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(3, textbox_link(4, checklinks, handle), handle)
                                        END IF
                                    ELSE
                                        IF textbox_link(2, checklinks, handle) > textbox_index(printlines + 1, handle) - 1 THEN
                                            LINE (_FONTWIDTH(textboxes(handle).font) + (textbox_link(1, checklinks, handle) - textbox_index(printlines, handle) + 1 * (textboxes(handle).screenline < 1 AND printlines = 1)) * _FONTWIDTH(textboxes(handle).font), 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_index(printlines + 1, handle) - textbox_index(printlines, handle) - 1) * _FONTWIDTH(textboxes(handle).font) - (textbox_link(1, checklinks, handle) - textbox_index(printlines, handle)) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(3, textbox_link(4, checklinks, handle), handle)
                                        ELSE
                                            LINE (_FONTWIDTH(textboxes(handle).font) + (textbox_link(1, checklinks, handle) - textbox_index(printlines, handle) + 1 * (textboxes(handle).screenline < 1 AND printlines = 1)) * _FONTWIDTH(textboxes(handle).font), 5 + _FONTHEIGHT * (printlines - textboxes(handle).screenline + 1))-STEP((textbox_link(2, checklinks, handle) - textbox_index(printlines, handle) - 1) * _FONTWIDTH(textboxes(handle).font) - (textbox_link(1, checklinks, handle) - textbox_index(printlines, handle) - 1) * _FONTWIDTH(textboxes(handle).font) - 1, 0), textbox_color(3, textbox_link(4, checklinks, handle), handle)
                                        END IF
                                    END IF
                                END IF
                            NEXT checklinks
                        END IF
                    NEXT printlines
                    FOR printimage = 1 TO textboxes(handle).images
                        DO
                            i = -textbox_image(6, printimage, handle)
                            imagearea.left = textbox_image(2, printimage, handle)
                            imagearea.top = (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, printimage, handle) - textboxes(handle).screenline + 1)) + textbox_image(3, printimage, handle)
                            imagearea.right = textbox_image(4, printimage, handle)
                            imagearea.bottom = (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, printimage, handle) - textboxes(handle).screenline + 1)) + textbox_image(5, printimage, handle)
                            FOR findsubtextbox = 1 TO textboxes(handle).textboxes
                                IF textbox_textbox(2, findsubtextbox, handle) = printimage THEN
                                    textboxes(textbox_textbox(1, findsubtextbox, handle)).area.left = textboxes(handle).area.left + imagearea.left
                                    textboxes(textbox_textbox(1, findsubtextbox, handle)).area.top = textboxes(handle).area.top + imagearea.top
                                    textboxes(textbox_textbox(1, findsubtextbox, handle)).area.right = textboxes(handle).area.left + imagearea.right
                                    textboxes(textbox_textbox(1, findsubtextbox, handle)).area.bottom = textboxes(handle).area.top + imagearea.bottom
                                    IF textboxes(handle).area.left + 1 > textboxes(handle).boundary.left OR textboxes(handle).bound = FALSE THEN
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.left = textboxes(handle).area.left + 1
                                    ELSE
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.left = textboxes(handle).boundary.left
                                    END IF
                                    IF textboxes(handle).area.top + 1 > textboxes(handle).boundary.top OR textboxes(handle).bound = FALSE THEN
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.top = textboxes(handle).area.top + 1
                                    ELSE
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.top = textboxes(handle).boundary.top
                                    END IF
                                    IF textboxes(handle).area.right - 2 < textboxes(handle).boundary.right OR textboxes(handle).bound = FALSE THEN
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.right = textboxes(handle).area.right - 2
                                    ELSE
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.right = textboxes(handle).boundary.right
                                    END IF
                                    IF textboxes(handle).area.bottom - 2 < textboxes(handle).boundary.bottom OR textboxes(handle).bound = FALSE THEN
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.bottom = textboxes(handle).area.bottom - 2
                                    ELSE
                                        textboxes(textbox_textbox(1, findsubtextbox, handle)).boundary.bottom = textboxes(handle).boundary.bottom
                                    END IF
                                    textboxes(textbox_textbox(1, findsubtextbox, handle)).bound = TRUE
                                END IF
                            NEXT findsubtextbox
                            IF i > -2 THEN EXIT DO
                            IF imagearea.top >= (_HEIGHT(textboxes(handle).page) - 1) THEN EXIT DO
                            IF imagearea.bottom <= 0 THEN EXIT DO
                            SELECT CASE textbox_image(1, printimage, handle)
                                CASE textboxes(handle).screenline
                                    IF imagearea.bottom < (_HEIGHT(textboxes(handle).page) - 1) THEN
                                        MAPRECT i, 0, 0, _WIDTH(i), _HEIGHT(i), textboxes(handle).page, imagearea.left, imagearea.top, imagearea.right, imagearea.bottom
                                        IF textbox_image(7, printimage, handle) THEN LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.right + 1, imagearea.bottom + 1), textbox_image(7, printimage, handle), B
                                    ELSE
                                        MAPRECT i, 0, 0, _WIDTH(i), ((_HEIGHT(textboxes(handle).page) - 1) - imagearea.top) / (imagearea.bottom - imagearea.top) * _HEIGHT(i), textboxes(handle).page, imagearea.left, imagearea.top, imagearea.right, (_HEIGHT(textboxes(handle).page) - 1)
                                        IF textbox_image(7, printimage, handle) THEN
                                            LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.right + 1, imagearea.top - 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.left - 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.right + 1, imagearea.top - 1)-(imagearea.right + 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                        END IF
                                    END IF
                                CASE IS < textboxes(handle).screenline
                                    IF imagearea.bottom < (_HEIGHT(textboxes(handle).page) - 1) THEN
                                        MAPRECT i, 0, _HEIGHT(i) - ((imagearea.bottom) / (imagearea.bottom - imagearea.top) * _HEIGHT(i)), _WIDTH(i), _HEIGHT(i), textboxes(handle).page, imagearea.left, 1, imagearea.right, imagearea.bottom
                                        IF textbox_image(7, printimage, handle) THEN
                                            LINE (imagearea.left - 1, 1)-(imagearea.left - 1, imagearea.bottom + 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.right + 1, 1)-(imagearea.right + 1, imagearea.bottom + 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.left - 1, imagearea.bottom + 1)-(imagearea.right + 1, imagearea.bottom + 1), textbox_image(7, printimage, handle)
                                        END IF
                                    ELSE
                                        MAPRECT i, 0, (0 - imagearea.top) / (imagearea.bottom - imagearea.top) * _HEIGHT(i), _WIDTH(i), ((_HEIGHT(textboxes(handle).page) - 1) - imagearea.top) / (imagearea.bottom - imagearea.top) * _HEIGHT(i), textboxes(handle).page, imagearea.left, 1, imagearea.right, (_HEIGHT(textboxes(handle).page) - 1)
                                        IF textbox_image(7, printimage, handle) THEN
                                            LINE (imagearea.left - 1, 1)-(imagearea.left - 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.right + 1, 1)-(imagearea.right + 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                        END IF
                                    END IF
                                CASE IS < endline
                                    IF imagearea.bottom < (_HEIGHT(textboxes(handle).page) - 1) THEN
                                        MAPRECT i, 0, 0, _WIDTH(i), _HEIGHT(i), textboxes(handle).page, imagearea.left, imagearea.top, imagearea.right, imagearea.bottom
                                        IF textbox_image(7, printimage, handle) THEN LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.right + 1, imagearea.bottom + 1), textbox_image(7, printimage, handle), B
                                    ELSE
                                        MAPRECT i, 0, 0, _WIDTH(i), ((_HEIGHT(textboxes(handle).page) - 1) - imagearea.top) / (imagearea.bottom - imagearea.top) * _HEIGHT(i), textboxes(handle).page, imagearea.left, imagearea.top, imagearea.right, (_HEIGHT(textboxes(handle).page) - 1)
                                        IF textbox_image(7, printimage, handle) THEN
                                            LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.right + 1, imagearea.top - 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.left - 1, imagearea.top - 1)-(imagearea.left - 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                            LINE (imagearea.right + 1, imagearea.top - 1)-(imagearea.right + 1, (_HEIGHT(textboxes(handle).page) - 1) - 1), textbox_image(7, printimage, handle)
                                        END IF
                                    END IF
                            END SELECT
                            EXIT DO
                        LOOP
                    NEXT printimage
                    IF textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON THEN
                        IF textboxes(handle).scrollbarbox = TEXTBOX_SCROLLBAR_BOX THEN LINE ((_WIDTH(textboxes(handle).page) - 1) - (2 * (_FONTWIDTH(textboxes(handle).font))), 1)-STEP(0, ((_HEIGHT(textboxes(handle).page) - 1) - 0) - 2), textboxes(handle).frame
                        SELECT CASE textboxes(handle).scrollbartype
                            CASE TEXTBOX_SCROLLBAR_BOX_DEFAULT
                                IF textboxes(handle).textlines = 0 THEN
                                    scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                ELSE
                                    scrollbarsize = textboxes(handle).rows / (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) * ((_HEIGHT(textboxes(handle).page) - 1))
                                END IF
                                IF scrollbarsize < TEXTBOX_SCROLLBAR_MINSIZE THEN scrollbarsize = TEXTBOX_SCROLLBAR_MINSIZE
                                IF scrollbarsize > (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                IF scrollbarsize < (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize)) + 2
                                ELSE
                                    scrollbarloc = 2
                                END IF
                                LINE ((_WIDTH(textboxes(handle).page) - 1) - 2, scrollbarloc)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2, scrollbarsize), textboxes(handle).scrollbarcolor, BF
                                IF scrollbarsize > TEXTBOX_SCROLLBAR_MINSIZE THEN LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2))-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                IF scrollbarsize > 2 * TEXTBOX_SCROLLBAR_MINSIZE THEN
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - 3)-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) + 3)-STEP(_FONTWIDTH(textboxes(handle).font), 0), textboxes(handle).backgroundcolor
                                END IF
                            CASE TEXTBOX_SCROLLBAR_BOX_HOLLOW
                                IF textboxes(handle).textlines = 0 THEN
                                    scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                ELSE
                                    scrollbarsize = textboxes(handle).rows / (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) * ((_HEIGHT(textboxes(handle).page) - 1))
                                END IF
                                IF scrollbarsize < TEXTBOX_SCROLLBAR_MINSIZE THEN scrollbarsize = TEXTBOX_SCROLLBAR_MINSIZE
                                IF scrollbarsize > (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                IF scrollbarsize < (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize)) + 2
                                ELSE
                                    scrollbarloc = 2
                                END IF
                                FOR thickenbox = 0 TO 3
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 - thickenbox, scrollbarloc + thickenbox)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2 + 2 * thickenbox, scrollbarsize - 2 * thickenbox), textboxes(handle).scrollbarcolor, B
                                NEXT thickenbox
                                IF scrollbarsize > TEXTBOX_SCROLLBAR_MINSIZE THEN LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbarcolor, BF
                                IF scrollbarsize > 2 * TEXTBOX_SCROLLBAR_MINSIZE THEN
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) - (scrollbarsize / 4) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbarcolor, BF
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 * _FONTWIDTH(textboxes(handle).font) + 1 + ((_FONTWIDTH(textboxes(handle).font) - 2) / 2), scrollbarloc + (scrollbarsize / 2) + (scrollbarsize / 4) - 1)-STEP(_FONTWIDTH(textboxes(handle).font), 2), textboxes(handle).scrollbarcolor, BF
                                END IF
                            CASE TEXTBOX_SCROLLBAR_BOX_HOLLOW_NOLINE
                                IF textboxes(handle).textlines = 0 THEN
                                    scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                ELSE
                                    scrollbarsize = textboxes(handle).rows / (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) * ((_HEIGHT(textboxes(handle).page) - 1))
                                END IF
                                IF scrollbarsize < TEXTBOX_SCROLLBAR_MINSIZE THEN scrollbarsize = TEXTBOX_SCROLLBAR_MINSIZE
                                IF scrollbarsize > (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN scrollbarsize = (_HEIGHT(textboxes(handle).page) - 1) - 4
                                IF scrollbarsize < (_HEIGHT(textboxes(handle).page) - 1) - 4 THEN
                                    scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize)) + 2
                                ELSE
                                    scrollbarloc = 2
                                END IF
                                FOR thickenbox = 0 TO 3
                                    LINE ((_WIDTH(textboxes(handle).page) - 1) - 2 - thickenbox, scrollbarloc + thickenbox)-STEP(-2 * (_FONTWIDTH(textboxes(handle).font) - 1) + 2 + 2 * thickenbox, scrollbarsize - 2 * thickenbox), textboxes(handle).scrollbarcolor, B
                                NEXT thickenbox
                            CASE TEXTBOX_SCROLLBAR_BALL_DEFAULT
                                scrollbarsize = _FONTWIDTH(textboxes(handle).font) * 1.5
                                scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize * 2)) + 2 + scrollbarsize / 2
                                CIRCLE ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), scrollbarsize / 2, textboxes(handle).scrollbarcolor
                                PAINT ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), textboxes(handle).scrollbarcolor
                            CASE TEXTBOX_SCROLLBAR_BALL_HOLLOW
                                scrollbarsize = _FONTWIDTH(textboxes(handle).font) * 1.5
                                scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize * 2)) + 2 + scrollbarsize / 2
                                CIRCLE ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), scrollbarsize / 2, textboxes(handle).scrollbarcolor
                            CASE TEXTBOX_SCROLLBAR_DOT
                                scrollbarsize = 1
                                scrollbarloc = ((textboxes(handle).screenline - 1) / ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows) * ((_HEIGHT(textboxes(handle).page) - 1) - 4 - scrollbarsize * 2)) + 2 + scrollbarsize / 2
                                PSET ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize / 2), textboxes(handle).scrollbarcolor
                        END SELECT
                        IF textboxes(handle).scrollbarline = TEXTBOX_SCROLLBAR_LINE THEN
                            LINE ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), 2)-((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc), textboxes(handle).scrollbarcolor
                            LINE ((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), scrollbarloc + scrollbarsize)-((_WIDTH(textboxes(handle).page) - 1) - _FONTWIDTH(textboxes(handle).font), (_HEIGHT(textboxes(handle).page) - 1) - 2), textboxes(handle).scrollbarcolor
                        END IF
                    END IF
            END SELECT
            COLOR textboxes(handle).color
            IF textboxes(handle).font THEN _FONT fonts(FONT_DEFAULT)
            _DEST savedest
        END IF
        IF textboxes(handle).bound = TRUE THEN
            pagearea.left = 0
            pagearea.top = 0
            pagearea.right = _WIDTH(textboxes(handle).page)
            pagearea.bottom = _HEIGHT(textboxes(handle).page)
            IF textboxes(handle).area.left < textboxes(handle).boundary.left THEN pagearea.left = textboxes(handle).boundary.left - textboxes(handle).area.left
            IF textboxes(handle).area.top < textboxes(handle).boundary.top THEN pagearea.top = textboxes(handle).boundary.top - textboxes(handle).area.top
            IF textboxes(handle).area.right > textboxes(handle).boundary.right THEN pagearea.right = pagearea.right - (textboxes(handle).area.right - textboxes(handle).boundary.right)
            IF textboxes(handle).area.bottom > textboxes(handle).boundary.bottom THEN pagearea.bottom = pagearea.bottom - (textboxes(handle).area.bottom - textboxes(handle).boundary.bottom)
            _PUTIMAGE (textboxes(handle).area.left + pagearea.left, textboxes(handle).area.top + pagearea.top), textboxes(handle).page, textboxes(handle).dest, (pagearea.left, pagearea.top)-(pagearea.right, pagearea.bottom)
        ELSE
            _PUTIMAGE (textboxes(handle).area.left, textboxes(handle).area.top), textboxes(handle).page, textboxes(handle).dest
        END IF
        FOR printsubtextbox = 1 TO textboxes(handle).textboxes
            PRINTTEXTBOX textbox_textbox(1, printsubtextbox, handle)
        NEXT printsubtextbox
END SELECT
END SUB

SUB SCROLLBARHIDE (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SCROLLBARHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_OFF
        textboxes(handle).cols = INT(((textboxes(handle).area.right - textboxes(handle).area.left) / _FONTWIDTH(textboxes(handle).font)) + (2 * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)))
        TEXTBOXREFORMAT handle
END SELECT
END SUB

SUB SCROLLBARSET (handle AS INTEGER, scrollbarcolor AS _UNSIGNED LONG, scrollbartype AS _BYTE, scrollbarline AS _BYTE, scrollbarbox AS _BYTE)
SHARED textboxes() AS TEXTBOX
IF scrollbartype < TEXTBOX_SCROLLBAR_BOX_DEFAULT THEN scrollbartype = TEXTBOX_SCROLLBAR_BOX_DEFAULT
IF scrollbartype > TEXTBOX_SCROLLBAR_DOT THEN scrollbartype = TEXTBOX_SCROLLBAR_DOT
IF scrollbarline < TEXTBOX_SCROLLBAR_LINE THEN scrollbarline = TEXTBOX_SCROLLBAR_LINE
IF scrollbarline > TEXTBOX_SCROLLBAR_NOLINE THEN scrollbarline = TEXTBOX_SCROLLBAR_NOLINE
IF scrollbarbox < TEXTBOX_SCROLLBAR_BOX THEN scrollbarbox = TEXTBOX_SCROLLBAR_BOX
IF scrollbarbox > TEXTBOX_SCROLLBAR_NOBOX THEN scrollbarbox = TEXTBOX_SCROLLBAR_NOBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SCROLLBARHIDE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF scrollbarcolor = TEXTBOX_KEEPCOLOR THEN scrollbarcolor = textboxes(handle).scrollbarcolor
        IF scrollbarcolor = 0 THEN scrollbarcolor = textboxes(handle).frame
        IF scrollbarcolor = 0 THEN scrollbarcolor = TEXTBOX_DEFAULTCOLOR
        textboxes(handle).scrollbarcolor = scrollbarcolor
        textboxes(handle).scrollbartype = scrollbartype
        textboxes(handle).scrollbarline = scrollbarline
        textboxes(handle).scrollbarbox = scrollbarbox
END SELECT
END SUB

SUB SCROLLBARSHOW (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
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
        TEXTBOXREFORMAT handle
END SELECT
END SUB

SUB SHOWTEXTBOX (handle AS INTEGER)
DIM showsubtextbox AS INTEGER
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            SHOWTEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).status = TEXTBOX_SHOW
        FOR showsubtextbox = 1 TO textboxes(handle).textboxes
            HIDETEXTBOX textbox_textbox(1, showsubtextbox, handle)
        NEXT showsubtextbox
END SELECT
END SUB

SUB SWAPTEXTBOX (handle1 AS INTEGER, handle2 AS INTEGER)
DIM swapbookmarky AS _BYTE
DIM swapcolory AS _BYTE
DIM swapcontenty AS _BYTE
DIM swaplinky AS _BYTE
DIM swapimagey AS _BYTE
DIM swaptextboxy AS _BYTE
DIM swapbookmarkx AS INTEGER
DIM swapcolorx AS INTEGER
DIM swapcontentx AS INTEGER
DIM swapimagex AS INTEGER
DIM swapindex AS INTEGER
DIM swaplinkx AS INTEGER
DIM swapstrings AS INTEGER
DIM swaptextboxx AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle1) THEN EXIT SUB
IF NOT VALIDTEXTBOX(handle2) THEN EXIT SUB
SWAP textboxes(handle1), textboxes(handle2)
FOR swapbookmarkx = 1 TO 1024
    FOR swapbookmarky = 1 TO 2
        SWAP textbox_bookmark(swapbookmarky, swapbookmarkx, handle1), textbox_bookmark(swapbookmarky, swapbookmarkx, handle2)
    NEXT swapbookmarky
    IF textbox_bookmark(1, swapbookmarkx, handle1) = FALSE AND textbox_bookmark(1, swapbookmarkx, handle2) = FALSE AND swapbookmarkx > 1 THEN EXIT FOR
NEXT swapbookmarkx
FOR swapcolorx = 1 TO 1024
    FOR swapcolory = 1 TO 3
        SWAP textbox_color(swapcolory, swapcolorx, handle1), textbox_color(swapcolory, swapcolorx, handle2)
    NEXT swapcolory
    IF textbox_color(1, swapcolorx, handle1) = FALSE AND textbox_color(1, swapcolorx, handle2) = FALSE AND swapcolorx > 1 THEN EXIT FOR
NEXT swapcolorx
FOR swapimagex = 1 TO 1024
    FOR swapimagey = 1 TO 8
        SWAP textbox_image(swapimagey, swapimagex, handle1), textbox_image(swapimagey, swapimagex, handle2)
    NEXT swapimagey
    IF textbox_image(1, swapimagex, handle1) = FALSE AND textbox_image(1, swapimagex, handle2) = FALSE AND swapimagex > 1 THEN EXIT FOR
NEXT swapimagex
FOR swapindex = -127 TO 32640
    SWAP textbox_index(swapindex, handle1), textbox_index(swapindex, handle2)
    IF textbox_index(swapindex, handle1) = FALSE AND textbox_index(swapindex, handle2) = FALSE AND swapindex > 1 THEN EXIT FOR
NEXT swapindex
FOR swapcontentx = 1 TO 32768
    FOR swapcontenty = 1 TO 9
        SWAP textbox_content(swapcontenty, swapcontentx, handle1), textbox_content(swapcontenty, swapcontentx, handle2)
    NEXT swapcontenty
    IF textbox_content(1, swapcontentx, handle1) = FALSE AND textbox_content(1, swapcontentx, handle2) = FALSE AND swapcontentx > 1 THEN EXIT FOR
NEXT swapcontentx
FOR swaplinkx = 1 TO 1024
    FOR swaplinky = 1 TO 5
        SWAP textbox_link(swaplinky, swaplinkx, handle1), textbox_link(swaplinky, swaplinkx, handle2)
    NEXT swaplinky
    IF textbox_link(1, swaplinkx, handle1) = FALSE AND textbox_link(1, swaplinkx, handle2) = FALSE AND swaplinkx > 1 THEN EXIT FOR
NEXT swaplinkx
FOR swapstrings = 0 TO 32767
    SWAP textbox_strings(swapstrings, handle1), textbox_strings(swapstrings, handle2)
NEXT swapstrings
FOR swaptextboxx = 1 TO 1024
    FOR swaptextboxy = 1 TO 2
        SWAP textbox_textbox(swaptextboxy, swaptextboxx, handle1), textbox_textbox(swaptextboxy, swaptextboxx, handle2)
    NEXT swaptextboxy
    IF textbox_textbox(1, swaptextboxx, handle1) = FALSE AND textbox_textbox(1, swaptextboxx, handle2) = FALSE AND swaptextboxx > 1 THEN EXIT FOR
NEXT swaptextboxx
SWAP textbox_text(handle1), textbox_text(handle2)
END SUB

SUB TEXTBOXADDIMAGE (handle AS INTEGER, row AS INTEGER, left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, image AS LONG, border AS _UNSIGNED LONG, behaviour AS _BYTE)
DIM centerv AS _BYTE
DIM filltext AS INTEGER
DIM tempimage AS LONG
DIM area AS RECT
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF image = FALSE THEN EXIT SUB
tempimage = image
IF image > FALSE THEN
    image = FALSE
    tempimage = _NEWIMAGE(right - left, bottom - top, 32)
END IF
IF row < -textboxes(handle).textlines + 1 THEN row = -textboxes(handle).textlines + 1
IF behaviour < TEXTBOX_IMAGE_BLOCK THEN behaviour = TEXTBOX_IMAGE_BLOCK
IF behaviour > TEXTBOX_IMAGE_WRAP THEN behaviour = TEXTBOX_IMAGE_WRAP
textboxes(handle).content = textboxes(handle).content + 1
textbox_content(1, textboxes(handle).content, handle) = TEXTBOX_CONTENT_IMAGE
textbox_content(2, textboxes(handle).content, handle) = ABS(image)
textbox_content(3, textboxes(handle).content, handle) = row
textbox_content(4, textboxes(handle).content, handle) = left
textbox_content(5, textboxes(handle).content, handle) = top
textbox_content(6, textboxes(handle).content, handle) = right
textbox_content(7, textboxes(handle).content, handle) = bottom
textbox_content(8, textboxes(handle).content, handle) = border
textbox_content(9, textboxes(handle).content, handle) = behaviour
area.left = left
area.top = top
area.right = right
area.bottom = bottom
IF area.left = 0 THEN
    IF _WIDTH(tempimage) >= (textboxes(handle).area.right - textboxes(handle).area.left) + 2 * TEXTBOX_IMAGE_DEFAULTBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)) THEN
        area.left = TEXTBOX_IMAGE_DEFAULTBORDER
    ELSE
        area.left = ((textboxes(handle).area.right - textboxes(handle).area.left) - _WIDTH(tempimage) - TEXTBOX_IMAGE_DEFAULTBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON))) \ 2
    END IF
ELSE
    IF area.left < TEXTBOX_IMAGE_MINIMUMBORDER THEN area.left = TEXTBOX_IMAGE_MINIMUMBORDER
    IF area.left > (textboxes(handle).area.right - textboxes(handle).area.left) - 2 * TEXTBOX_IMAGE_MINIMUMBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)) THEN area.left = TEXTBOX_IMAGE_DEFAULTBORDER
END IF
IF area.right < area.left THEN area.right = 0
IF area.top = 0 THEN
    area.top = TEXTBOX_IMAGE_DEFAULTBORDER
ELSE
    row = row + area.top \ _FONTHEIGHT(textboxes(handle).font)
    area.top = area.top MOD _FONTHEIGHT(textboxes(handle).font)
    IF area.top < TEXTBOX_IMAGE_MINIMUMBORDER THEN area.top = TEXTBOX_IMAGE_MINIMUMBORDER
END IF
IF area.bottom < area.top THEN area.bottom = 0
IF area.right = 0 THEN
    IF area.left + _WIDTH(tempimage) > (textboxes(handle).area.right - textboxes(handle).area.left) - TEXTBOX_IMAGE_DEFAULTBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)) THEN
        area.right = (textboxes(handle).area.right - textboxes(handle).area.left) - TEXTBOX_IMAGE_DEFAULTBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON))
    ELSE
        area.right = area.left + _WIDTH(tempimage)
    END IF
ELSE
    IF area.right > (textboxes(handle).area.right - textboxes(handle).area.left) - 2 * TEXTBOX_IMAGE_MINIMUMBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)) THEN
        IF area.left + _WIDTH(tempimage) > (textboxes(handle).area.right - textboxes(handle).area.left) - TEXTBOX_IMAGE_MINIMUMBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)) THEN
            area.right = textboxes(handle).area.right - TEXTBOX_IMAGE_DEFAULTBORDER + (2 * _FONTWIDTH(textboxes(handle).font) * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON))
        ELSE
            area.right = area.left + _WIDTH(tempimage)
        END IF
    END IF
END IF
IF area.bottom = 0 THEN
    IF area.right - area.left = _WIDTH(tempimage) THEN
        area.bottom = area.top + _HEIGHT(tempimage)
    ELSE
        area.bottom = area.top + ((area.right - area.left) / _WIDTH(tempimage)) * _HEIGHT(tempimage)
    END IF
    centerv = area.bottom MOD _FONTHEIGHT(textboxes(handle).font)
    IF centerv = 0 THEN centerv = _FONTHEIGHT(textboxes(handle).font)
    area.bottom = area.bottom - (area.top - (centerv \ 2))
    area.top = centerv \ 2
END IF
textboxes(handle).images = textboxes(handle).images + 1
textbox_image(1, textboxes(handle).images, handle) = textboxes(handle).textlines + row - 1
textbox_image(2, textboxes(handle).images, handle) = area.left
textbox_image(3, textboxes(handle).images, handle) = area.top
textbox_image(4, textboxes(handle).images, handle) = area.right
textbox_image(5, textboxes(handle).images, handle) = area.bottom
textbox_image(6, textboxes(handle).images, handle) = ABS(image)
textbox_image(7, textboxes(handle).images, handle) = border
textbox_image(8, textboxes(handle).images, handle) = behaviour
IF behaviour = TEXTBOX_IMAGE_BLOCK THEN
IF textboxes(handle).lasttext = TEXTBOX_NOTICK THEN textboxes(handle).textlines = textboxes(handle).textlines - 1
    FOR filltext = 1 TO ((area.bottom - area.top) \ _FONTHEIGHT(textboxes(handle).font)) + row + 1
        textboxes(handle).textlines = textboxes(handle).textlines + 1
        textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
    NEXT filltext
    textboxes(handle).lasttext = TEXTBOX_NOTICK
    textboxes(handle).textlinehold = FALSE
ELSE
    IF textboxes(handle).textlinehold < ((area.bottom - area.top) \ _FONTHEIGHT(textboxes(handle).font)) + row + 1 THEN textboxes(handle).textlinehold = ((area.bottom - area.top) \ _FONTHEIGHT(textboxes(handle).font)) + row + 1
END IF
IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + textboxes(handle).textlinehold
IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN textboxes(handle).screenline = 32640
IF image = FALSE THEN _FREEIMAGE tempimage
END SUB

SUB TEXTBOXADDTEXT (handle AS INTEGER, text AS STRING, tickspeed AS _BYTE)
DIM linkcolor AS _BIT
DIM terminateindex AS INTEGER
DIM findlinelength AS LONG
DIM linelength AS LONG
DIM movetext AS LONG
DIM saveindex AS _UNSIGNED LONG
DIM valuesave AS STRING
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
saveindex = LEN(textbox_text(handle))
textboxes(handle).textstrings = textboxes(handle).textstrings + 1
textboxes(handle).content = textboxes(handle).content + 1
textbox_content(1, textboxes(handle).content, handle) = TEXTBOX_CONTENT_TEXT
textbox_content(2, textboxes(handle).content, handle) = saveindex
textbox_content(3, textboxes(handle).content, handle) = saveindex
textbox_content(4, textboxes(handle).content, handle) = textboxes(handle).textstrings
textbox_strings(textboxes(handle).textstrings, handle) = text
movetext = 0
DO
    movetext = movetext + 1
    IF MID$(text, movetext, 2) = "<#" THEN
        SELECT CASE MID$(text, movetext + 2, 1)
            CASE "b"
                textboxes(handle).bookmarks = textboxes(handle).bookmarks + 1
                textbox_bookmark(2, textboxes(handle).bookmarks, handle) = textboxes(handle).textlines
                movetext = movetext + 2
                valuesave = ""
                DO
                    movetext = movetext + 1
                    IF MID$(text, movetext, 1) = ">" THEN EXIT DO
                    valuesave = valuesave + MID$(text, movetext, 1)
                LOOP UNTIL movetext >= LEN(text)
                textbox_bookmark(1, textboxes(handle).bookmarks, handle) = VAL(valuesave)
            CASE "c"
                textboxes(handle).colors = textboxes(handle).colors + 1
                textbox_color(1, textboxes(handle).colors, handle) = textbox_content(3, textboxes(handle).content, handle) + 1
                textbox_color(2, textboxes(handle).colors, handle) = saveindex + LEN(text)
                movetext = movetext + 2
                valuesave = ""
                DO
                    movetext = movetext + 1
                    IF MID$(text, movetext, 1) = ">" THEN EXIT DO
                    valuesave = valuesave + MID$(text, movetext, 1)
                LOOP UNTIL movetext >= LEN(text)
                valuesave = TRIM$(valuesave)
                IF movetext < LEN(text) THEN
                    IF VAL(valuesave) OR LEFT$(valuesave, 1) = "0" THEN
                        IF VAL(valuesave) <= 15 THEN
                            textbox_color(3, textboxes(handle).colors, handle) = ctorgb(VAL(valuesave))
                        ELSE
                            textbox_color(3, textboxes(handle).colors, handle) = VAL(valuesave)
                        END IF
                    ELSE IF LEFT$(valuesave, 1) = "(" THEN
                            textbox_color(3, textboxes(handle).colors, handle) = _RGB32(VAL(MID$(valuesave, 2, 3)), VAL(MID$(valuesave, 6, 3)), VAL(MID$(valuesave, 10, 3)))
                        ELSE
                            SELECT CASE UCASE$(valuesave)
                                CASE "BLACK"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(0)
                                CASE "DARK BLUE", "DARKBLUE", "DARK_BLUE"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(1)
                                CASE "DARK LIME", "DARKLIME", "DARK_LIME"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(2)
                                CASE "DARK CYAN", "DARKCYAN", "DARK_CYAN"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(3)
                                CASE "RED"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(4)
                                CASE "DARK MAGENTA", "DARKMAGENTA", "DARK_MAGENTA"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(5)
                                CASE "GOLDEN ROD", "GOLDENROD", "GOLDEN_ROD"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(6)
                                CASE "LIGHT GRAY", "LIGHTGRAY", "LIGHT_GRAY"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(7)
                                CASE "GRAY"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(8)
                                CASE "BLUE"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(9)
                                CASE "GREEN"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(10)
                                CASE "CYAN"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(11)
                                CASE "ORANGE"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(12)
                                CASE "MAGENTA"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(13)
                                CASE "YELLOW"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(14)
                                CASE "WHITE"
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(15)
                                CASE ELSE
                                    textbox_color(3, textboxes(handle).colors, handle) = ctorgb(0)
                            END SELECT
                        END IF
                    END IF
                END IF
            CASE "l"
                textboxes(handle).links = textboxes(handle).links + 1
                textbox_link(1, textboxes(handle).links, handle) = textbox_content(3, textboxes(handle).content, handle) + 1
                textbox_link(2, textboxes(handle).links, handle) = saveindex + LEN(text)
                movetext = movetext + 2
                valuesave = ""
                DO
                    movetext = movetext + 1
                    IF MID$(text, movetext, 1) = ">" THEN EXIT DO
                    valuesave = valuesave + MID$(text, movetext, 1)
                LOOP UNTIL movetext >= LEN(text)
                IF movetext < LEN(text) THEN
                    textbox_link(3, textboxes(handle).links, handle) = VAL(valuesave)
                ELSE
                    textbox_link(3, textboxes(handle).links, handle) = textboxes(handle).links
                END IF
                textboxes(handle).colors = textboxes(handle).colors + 1
                textbox_color(1, textboxes(handle).colors, handle) = textbox_link(1, textboxes(handle).links, handle)
                textbox_color(2, textboxes(handle).colors, handle) = textbox_link(2, textboxes(handle).links, handle)
                textbox_color(3, textboxes(handle).colors, handle) = textboxes(handle).linkstand
                textbox_link(4, textboxes(handle).links, handle) = textboxes(handle).colors
                textbox_link(5, textboxes(handle).links, handle) = TEXTBOX_LINKSTATUS_STAND
            CASE "m"
                valuesave = ""
                FOR freshline = 1 TO LEN(text)
                    IF MID$(text, freshline, 1) <> CHR$(255) THEN valuesave = valuesave + MID$(text, freshline, 1)
                NEXT freshline
                text = valuesave
                FOR findlinelength = 1 TO LEN(text)
                    IF MID$(text, findlinelength, 1) = "<" THEN
                        IF MID$(text, findlinelength + 1, 1) = "#" THEN
                            FOR findlinelength = findlinelength + 2 TO LEN(text)
                                IF MID$(text, findlinelength, 1) = ">" THEN EXIT FOR
                            NEXT findlinelength
                        ELSE
                            IF MID$(text, findlinelength + 1, 1) = "/" THEN
                                IF MID$(text, findlinelength + 2, 1) = "#" THEN
                                    FOR findlinelength = findlinelength + 3 TO LEN(text)
                                        IF MID$(text, findlinelength, 1) = ">" THEN EXIT FOR
                                    NEXT findlinelength
                                ELSE
                                    linelength = linelength + 1
                                END IF
                            ELSE
                                linelength = linelength + 1
                            END IF
                        END IF
                    ELSE
                        linelength = linelength + 1
                    END IF
                NEXT findlinelength
                movetext = movetext + 3
                IF textboxes(handle).cols > linelength + 2 THEN text = LEFT$(text, movetext) + STRING$((textboxes(handle).cols - linelength - 1) \ 2, 255) + MID$(text, movetext + 1)
        END SELECT
    ELSE IF MID$(text, movetext, 3) = "</#" THEN
            SELECT CASE MID$(text, movetext + 3, 1)
                CASE "c"
                    FOR terminateindex = textboxes(handle).colors TO 1 STEP -1
                        linkcolor = FALSE
                        FOR checklinks = 1 TO textboxes(handle).links
                            IF textbox_link(4, checklinks, handle) = terminateindex THEN
                                linkcolor = -1
                                EXIT FOR
                            END IF
                        NEXT checklinks
                        IF NOT linkcolor THEN
                            IF textbox_color(2, terminateindex, handle) = saveindex + LEN(text) THEN
                                textbox_color(2, terminateindex, handle) = textbox_content(3, textboxes(handle).content, handle) + 1
                                EXIT FOR
                            END IF
                        END IF
                    NEXT terminateindex
                    movetext = movetext + 4
                CASE "l"
                    FOR terminateindex = textboxes(handle).links TO 1 STEP -1
                        IF textbox_link(2, terminateindex, handle) = saveindex + LEN(text) THEN
                            textbox_link(2, terminateindex, handle) = textbox_content(3, textboxes(handle).content, handle) + 1
                            textbox_color(2, textbox_link(4, terminateindex, handle), handle) = textbox_content(3, textboxes(handle).content, handle) + 1
                            EXIT FOR
                        END IF
                    NEXT terminateindex
                    movetext = movetext + 4
            END SELECT
        ELSE
            textbox_text(handle) = textbox_text(handle) + MID$(text, movetext, 1)
            textbox_content(3, textboxes(handle).content, handle) = LEN(textbox_text(handle))
        END IF
    END IF
LOOP UNTIL movetext >= LEN(text)
TEXTBOXFORMATTEXT handle, textboxes(handle).content, tickspeed
END SUB

SUB TEXTBOXBOUND (handle AS INTEGER, left AS LONG, top AS LONG, right AS LONG, bottom AS LONG)
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXBOUND handle, left, top, right, bottom
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).bound = TRUE
        textboxes(handle).boundary.left = left
        textboxes(handle).boundary.top = top
        textboxes(handle).boundary.right = right
        textboxes(handle).boundary.bottom = bottom
END SELECT
END SUB

SUB TEXTBOXBOUNDOFF (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXBOUNDON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).bound = FALSE
END SELECT
END SUB

SUB TEXTBOXBOUNDON (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXBOUNDON handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        textboxes(handle).bound = TRUE
END SELECT
END SUB

SUB TEXTBOXBRINGTOFRONT (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
TEXTBOXSETQUEUE handle, UBOUND(textboxes)
END SUB

SUB TEXTBOXCLEAR (handle AS INTEGER)
DIM cleartextboxbookmarky AS _BYTE
DIM cleartextboxcolory AS _BYTE
DIM cleartextboxcontenty AS _BYTE
DIM cleartextboximagey AS _BYTE
DIM cleartextboxlinky AS _BYTE
DIM cleartextboxtextboxy AS _BYTE
DIM cleartextboxbookmarkx AS INTEGER
DIM cleartextboxcolorx AS INTEGER
DIM cleartextboxcontentx AS INTEGER
DIM cleartextboximagex AS INTEGER
DIM cleartextboxlinkx AS INTEGER
DIM cleartextboxindex AS INTEGER
DIM cleartextboxtextboxx AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXCLEAR handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        FOR cleartextboxbookmarkx = 1 TO textboxes(handle).bookmarks
            FOR cleartextboxbookmarky = 1 TO 2
                textbox_bookmark(cleartextboxbookmarky, cleartextboxbookmarkx, handle) = FALSE
        NEXT cleartextboxbookmarky, cleartextboxbookmarkx
        FOR cleartextboxcolorx = 1 TO textboxes(handle).colors
            FOR cleartextboxcolory = 1 TO 3
                textbox_color(cleartextboxcolory, cleartextboxcolorx, handle) = FALSE
        NEXT cleartextboxcolory, cleartextboxcolorx
        FOR cleartextboxindex = 1 TO textboxes(handle).textlines
            textbox_index(cleartextboxindex, handle) = FALSE
        NEXT cleartextboxindex
        FOR cleartextboximagex = 1 TO textboxes(handle).images
            FOR cleartextboximagey = 1 TO 8
                textbox_image(cleartextboximagey, cleartextboximagex, handle) = FALSE
        NEXT cleartextboximagey, cleartextboximagex
        FOR cleartextboxlinkx = 1 TO textboxes(handle).links
            FOR cleartextboxlinky = 1 TO 5
                textbox_link(cleartextboxlinky, cleartextboxlinkx, handle) = FALSE
        NEXT cleartextboxlinky, cleartextboxlinkx
        FOR cleartextboxtextboxx = 1 TO textboxes(handle).textboxes
            TEXTBOXFREE textbox_textbox(1, cleartextboxtextboxx, handle)
            FOR cleartextboxtextboxy = 1 TO 2
                textbox_textbox(cleartextboxtextboxy, cleartextboxtextboxx, handle) = FALSE
        NEXT cleartextboxtextboxy, cleartextboxtextboxx
        FOR cleartextboxcontentx = 1 TO textboxes(handle).content
            FOR cleartextboxcontenty = 1 TO 9
                textbox_content(cleartextboxcontenty, cleartextboxcontentx, handle) = FALSE
        NEXT cleartextboxcontenty, cleartextboxcontentx
        textbox_text(handle) = ""
        textboxes(handle).textstrings = 0
        textboxes(handle).textlines = 0
        textboxes(handle).textlinehold = FALSE
        textboxes(handle).lasttext = TEXTBOX_TICK
        textboxes(handle).colors = FALSE
        textboxes(handle).images = FALSE
        textboxes(handle).links = FALSE
        textboxes(handle).textboxes = FALSE
        textboxes(handle).content = FALSE
END SELECT
END SUB

SUB TEXTBOXCLEARTEXT (handle)
DIM cleartextboxbookmarky AS _BYTE
DIM cleartextboxcolory AS _BYTE
DIM cleartextboxcontenty AS _BYTE
DIM cleartextboxlinky AS _BYTE
DIM cleartextboxbookmarkx AS INTEGER
DIM cleartextboxcolorx AS INTEGER
DIM cleartextboxcontentx AS INTEGER
DIM cleartextboxindex AS INTEGER
DIM cleartextboxlinkx AS INTEGER
DIM clearsubtextboxtext AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXCLEARTEXT handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        FOR cleartextboxbookmarkx = 1 TO textboxes(handle).bookmarks
            FOR cleartextboxbookmarky = 1 TO 2
                textbox_bookmark(cleartextboxbookmarky, cleartextboxbookmarkx, handle) = FALSE
        NEXT cleartextboxbookmarky, cleartextboxbookmarkx
        FOR cleartextboxcolorx = 1 TO textboxes(handle).colors
            FOR cleartextboxcolory = 1 TO 3
                textbox_color(cleartextboxcolory, cleartextboxcolorx, handle) = FALSE
        NEXT cleartextboxcolory, cleartextboxcolorx
        FOR cleartextboxindex = 1 TO textboxes(handle).textlines
            textbox_index(cleartextboxindex, handle) = FALSE
        NEXT cleartextboxindex
        FOR cleartextboxlinkx = 1 TO textboxes(handle).links
            FOR cleartextboxlinky = 1 TO 5
                textbox_link(cleartextboxlinky, cleartextboxlinkx, handle) = FALSE
        NEXT cleartextboxlinky, cleartextboxlinkx
        FOR cleartextboxcontentx = 1 TO textboxes(handle).content
            IF textbox_content(1, cleartextboxcontentx, handle) = TEXTBOX_CONTENT_TEXT THEN
                FOR cleartextboxcontenty = 1 TO 9
                    textbox_content(cleartextboxcontenty, cleartextboxcontentx, handle) = FALSE
                NEXT cleartextboxcontenty
            END IF
        NEXT cleartextboxcontentx
        textboxes(handle).textstrings = 0
        textboxes(handle).textlines = 0
        textboxes(handle).textlinehold = FALSE
        textboxes(handle).lasttext = TEXTBOX_TICK
        textboxes(handle).colors = FALSE
        textboxes(handle).links = FALSE
        FOR clearsubtextboxtext = 1 TO textboxes(handle).textboxes
            TEXTBOXCLEARTEXT textbox_textbox(1, clearsubtextboxtext, handle)
        NEXT clearsubtextboxtext
END SELECT
END SUB

SUB TEXTBOXFORMATTEXT (handle AS INTEGER, textstring AS INTEGER, tickspeed AS _BYTE)
DIM foundimage AS _BIT
DIM istrans AS _BIT
DIM checkforimage AS INTEGER
DIM checkimage AS INTEGER
DIM checkonimage AS INTEGER
DIM coverindex AS INTEGER
DIM formattext AS INTEGER
DIM linechange AS INTEGER
DIM n AS INTEGER
DIM newlines AS INTEGER
DIM xscl AS SINGLE
DIM yscl AS SINGLE
DIM x AS LONG
DIM y AS LONG
DIM lastword AS _UNSIGNED LONG
DIM imagearea AS RECT
DIM letterarea AS RECT
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF textboxes(handle).ticktimer > 0 THEN
    DO
        TEXTBOXTICK handle
    LOOP UNTIL textboxes(handle).ticktimer = FALSE
END IF
IF tickspeed > 0 THEN
    DO
        newlines = textboxes(handle).textlines
        textboxes(handle).textlines = textboxes(handle).textlines + 1
    LOOP UNTIL newlines > 0
    textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
    lastword = textbox_index(textboxes(handle).textlines, handle)
    formattext = textbox_index(textboxes(handle).textlines, handle) - 1
    DO
        formattext = formattext + 1
        foundimage = FALSE
        FOR checkimage = 1 TO textboxes(handle).images
            IF textboxes(handle).textlines >= textbox_image(1, checkimage, handle) AND textboxes(handle).textlines <= textbox_image(1, checkimage, handle) + ((textbox_image(5, checkimage, handle) - textbox_image(3, checkimage, handle)) \ _FONTHEIGHT(textboxes(handle).font)) + 2 THEN
                foundimage = TRUE
                EXIT FOR
            END IF
        NEXT checkimage
        IF foundimage = TRUE THEN
            IF textbox_image(8, checkimage, handle) = TEXTBOX_BLOCK THEN
                textboxes(handle).textlines = textboxes(handle).textlines + 1
                textbox_index(textboxes(handle).textlines, handle) = textbox_index(textboxes(handle).textlines - 1, handle)
                textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
            ELSE
                FOR checkonimage = 1 TO textboxes(handle).images
                    imagearea.left = textboxes(handle).area.left + textbox_image(2, checkonimage, handle)
                    imagearea.top = textboxes(handle).area.top + (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, checkonimage, handle) - textboxes(handle).screenline + 2)) + textbox_image(3, checkonimage, handle)
                    imagearea.right = textboxes(handle).area.left + textbox_image(4, checkonimage, handle) + _FONTWIDTH(textboxes(handle).font) * 2
                    imagearea.bottom = textboxes(handle).area.top + (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, checkonimage, handle) - textboxes(handle).screenline + 3)) + textbox_image(5, checkonimage, handle)
                    letterarea.left = textboxes(handle).area.left + (formattext - textbox_index(textboxes(handle).textlines, handle)) * _FONTWIDTH(textboxes(handle).font)
                    letterarea.top = textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + (textboxes(handle).textlines - textboxes(handle).screenline) * _FONTHEIGHT(textboxes(handle).font)
                    letterarea.right = letterarea.left + _FONTWIDTH(textboxes(handle).font)
                    letterarea.bottom = letterarea.top + _FONTHEIGHT(textboxes(handle).font)
                    IF RECTcollide(letterarea, imagearea) THEN
                        istrans = FALSE
                        IF textbox_image(8, checkonimage, handle) = TEXTBOX_IMAGE_WRAP THEN
                            _SOURCE -textbox_image(6, checkonimage, handle)
                            istrans = TRUE
                            xscl = _WIDTH(-textbox_image(6, checkonimage, handle)) / (textbox_image(4, checkonimage, handle) - textbox_image(2, checkonimage, handle))
                            yscl = _HEIGHT(-textbox_image(6, checkonimage, handle)) / (textbox_image(5, checkonimage, handle) - textbox_image(3, checkonimage, handle))
                            FOR x = letterarea.left - imagearea.left TO letterarea.right - imagearea.left
                                FOR y = letterarea.top - imagearea.top TO letterarea.bottom - imagearea.top
                                    IF x > -1 AND y > -1 THEN
                                        istrans = (_ALPHA32(POINT(x * xscl, y * yscl)) = 0)
                                    END IF
                                    IF istrans = FALSE THEN EXIT FOR
                                NEXT y
                                IF istrans = FALSE THEN EXIT FOR
                            NEXT x
                            _SOURCE 0
                        END IF
                        IF istrans = FALSE THEN
                            IF letterarea.right <= imagearea.right AND letterarea.bottom <= imagearea.bottom THEN
                                n = formattext - lastword
                                textbox_text(handle) = LEFT$(textbox_text(handle), lastword - 1) + STRING$(n, 255) + MID$(textbox_text(handle), lastword)
                                textbox_content(3, textstring, handle) = LEN(textbox_text(handle))
                                textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines + 1, handle) + n
                                lastword = formattext
                            END IF
                        END IF
                    END IF
                NEXT checkonimage
            END IF
        END IF
        IF formattext - textbox_index(textboxes(handle).textlines, handle) > textboxes(handle).cols - 2 THEN
            IF lastword = textbox_index(textboxes(handle).textlines, handle) THEN
                DO
                    FOR checkforimage = 1 TO textboxes(handle).images
                        IF textboxes(handle).textlines >= textbox_image(1, checkforimage, handle) AND textboxes(handle).textlines <= textbox_image(1, checkforimage, handle) + ((textbox_image(5, checkforimage, handle) - textbox_image(3, checkforimage, handle)) \ _FONTHEIGHT(textboxes(handle).font) + 1) THEN EXIT DO
                    NEXT checkforimage
                    textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle) + textboxes(handle).cols - 2
                    lastword = textbox_index(textboxes(handle).textlines + 1, handle)
                    EXIT DO
                LOOP
            END IF
            IF MID$(textbox_text(handle), textbox_index(textboxes(handle).textlines + 1, handle), 1) = " " THEN textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines + 1, handle) + 1
            textboxes(handle).textlines = textboxes(handle).textlines + 1
            textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
        END IF
        IF MID$(textbox_text(handle), formattext, 1) = CHR$(32) OR formattext = LEN(textbox_text(handle)) THEN
            textbox_index(textboxes(handle).textlines + 1, handle) = formattext + 1
            lastword = textbox_index(textboxes(handle).textlines + 1, handle)
        END IF
    LOOP UNTIL formattext >= textbox_content(3, textstring, handle)
    textbox_strings(0, handle) = ""
    FOR n = newlines + 1 TO textboxes(handle).textlines
        textbox_strings(0, handle) = textbox_strings(0, handle) + MID$(textbox_text(handle), textbox_index(n, handle), textbox_index(n + 1, handle) - textbox_index(n, handle)) + CHR$(13)
    NEXT n
    FOR n = newlines + 1 TO 32768
        IF textbox_index(n, handle) = 0 THEN
            EXIT FOR
        ELSE
            textbox_index(n, handle) = 0
        END IF
    NEXT n
    IF newlines = 1 THEN textbox_index(3, handle) = 0
    textboxes(handle).textlines = newlines
    textboxes(handle).tickindex = 0
    textboxes(handle).ticktimer = _FREETIMER
    IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + textboxes(handle).textlinehold
    IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN textboxes(handle).screenline = 32640
    ON TIMER(textboxes(handle).ticktimer, 1 / tickspeed) TEXTBOXTICK handle
    TIMER(textboxes(handle).ticktimer) ON
    TEXTBOXTICK handle
ELSE
    IF textboxes(handle).textlines > 0 THEN textboxes(handle).textlines = textboxes(handle).textlines - 1
    newlines = textboxes(handle).textlines
    textboxes(handle).textlines = textboxes(handle).textlines + 1
    textboxes(handle).textlinehold = textboxes(handle).textlinehold - 1
    IF textboxes(handle).textlinehold < 0 THEN textboxes(handle).textlinehold = 0
    textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
    lastword = textbox_index(textboxes(handle).textlines, handle)
    formattext = textbox_index(textboxes(handle).textlines, handle) - 1
    DO
        formattext = formattext + 1
        foundimage = FALSE
        FOR checkimage = 1 TO textboxes(handle).images
            IF textboxes(handle).textlines >= textbox_image(1, checkimage, handle) AND textboxes(handle).textlines <= textbox_image(1, checkimage, handle) + ((textbox_image(5, checkimage, handle) - textbox_image(3, checkimage, handle)) \ _FONTHEIGHT(textboxes(handle).font)) + 1 THEN
                foundimage = TRUE
                EXIT FOR
            END IF
        NEXT checkimage
        IF foundimage = TRUE THEN
            IF textbox_image(8, checkimage, handle) = TEXTBOX_BLOCK THEN
                textboxes(handle).textlines = textboxes(handle).textlines + 1
                textbox_index(textboxes(handle).textlines, handle) = textbox_index(textboxes(handle).textlines - 1, handle)
                textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
            ELSE
                FOR checkonimage = 1 TO textboxes(handle).images
                    imagearea.left = textboxes(handle).area.left + textbox_image(2, checkonimage, handle)
                    imagearea.top = textboxes(handle).area.top + (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, checkonimage, handle) - textboxes(handle).screenline + 1)) + textbox_image(3, checkonimage, handle)
                    imagearea.right = textboxes(handle).area.left + textbox_image(4, checkonimage, handle) + _FONTWIDTH(textboxes(handle).font) * 2
                    imagearea.bottom = textboxes(handle).area.top + (_FONTHEIGHT(textboxes(handle).font) * (textbox_image(1, checkonimage, handle) - textboxes(handle).screenline + 2)) + textbox_image(5, checkonimage, handle)
                    letterarea.left = textboxes(handle).area.left + (formattext - textbox_index(textboxes(handle).textlines, handle)) * _FONTWIDTH(textboxes(handle).font)
                    letterarea.top = textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + (textboxes(handle).textlines - textboxes(handle).screenline) * _FONTHEIGHT(textboxes(handle).font)
                    letterarea.right = letterarea.left + _FONTWIDTH(textboxes(handle).font)
                    letterarea.bottom = letterarea.top + _FONTHEIGHT(textboxes(handle).font)
                    IF RECTcollide(letterarea, imagearea) THEN
                        istrans = FALSE
                        IF textbox_image(8, checkonimage, handle) = TEXTBOX_IMAGE_WRAP THEN
                            _SOURCE -textbox_image(6, checkonimage, handle)
                            istrans = TRUE
                            xscl = _WIDTH(-textbox_image(6, checkonimage, handle)) / (textbox_image(4, checkonimage, handle) - textbox_image(2, checkonimage, handle))
                            yscl = _HEIGHT(-textbox_image(6, checkonimage, handle)) / (textbox_image(5, checkonimage, handle) - textbox_image(3, checkonimage, handle))
                            FOR x = letterarea.left - imagearea.left TO letterarea.right - imagearea.left
                                FOR y = letterarea.top - imagearea.top TO letterarea.bottom - imagearea.top
                                    IF x > -1 AND y > -1 THEN
                                        istrans = (_ALPHA32(POINT(x * xscl, y * yscl)) = 0)
                                    END IF
                                    IF istrans = FALSE THEN EXIT FOR
                                NEXT y
                                IF istrans = FALSE THEN EXIT FOR
                            NEXT x
                            _SOURCE 0
                        END IF
                        IF istrans = FALSE THEN
                            IF letterarea.right <= imagearea.right AND letterarea.bottom <= imagearea.bottom THEN
                                n = formattext - lastword
                                textbox_text(handle) = LEFT$(textbox_text(handle), lastword - 1) + STRING$(n, 255) + MID$(textbox_text(handle), lastword)
                                textbox_content(3, textstring, handle) = LEN(textbox_text(handle))
                                textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines + 1, handle) + n
                                lastword = formattext
                            END IF
                        END IF
                    END IF
                NEXT checkonimage
            END IF
        END IF
        IF formattext - textbox_index(textboxes(handle).textlines, handle) > textboxes(handle).cols - 2 THEN
            IF lastword = textbox_index(textboxes(handle).textlines, handle) THEN
                DO
                    FOR checkforimage = 1 TO textboxes(handle).images
                        IF textboxes(handle).textlines >= textbox_image(1, checkforimage, handle) AND textboxes(handle).textlines <= textbox_image(1, checkforimage, handle) + ((textbox_image(5, checkforimage, handle) - textbox_image(3, checkforimage, handle)) \ _FONTHEIGHT(textboxes(handle).font) + 1) THEN EXIT DO
                    NEXT checkforimage
                    textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle) + textboxes(handle).cols - 2
                    lastword = textbox_index(textboxes(handle).textlines + 1, handle)
                    EXIT DO
                LOOP
            END IF
            textboxes(handle).textlines = textboxes(handle).textlines + 1
            IF MID$(textbox_text(handle), textbox_index(textboxes(handle).textlines, handle), 1) = " " THEN textbox_index(textboxes(handle).textlines, handle) = textbox_index(textboxes(handle).textlines, handle) + 1
            textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
        END IF
        IF MID$(textbox_text(handle), formattext, 1) = CHR$(32) OR formattext = LEN(textbox_text(handle)) THEN
            textbox_index(textboxes(handle).textlines + 1, handle) = formattext + 1
            lastword = textbox_index(textboxes(handle).textlines + 1, handle)
        END IF
    LOOP UNTIL formattext >= textbox_content(3, textstring, handle)
    linechange = textboxes(handle).textlines - newlines
    textboxes(handle).textlinehold = textboxes(handle).textlinehold - (linechange - 1)
    IF textboxes(handle).textlinehold < 0 THEN textboxes(handle).textlinehold = 0
    IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + newlines
    IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN textboxes(handle).screenline = 32640
    textboxes(handle).textlines = textboxes(handle).textlines + 1
    textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
    textboxes(handle).lasttext = TEXTBOX_NOTICK
END IF
END SUB

SUB TEXTBOXFREE (handle AS INTEGER)
DIM freesubtextbox AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            TEXTBOXFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        IF textboxes(handle).inuse = TRUE THEN
            TEXTBOXCLEAR handle
            IF textboxes(handle).background THEN _FREEIMAGE (textboxes(handle).background)
            IF textboxes(handle).page THEN _FREEIMAGE(textboxes(handle).page)
            textboxes(handle).page = FALSE
            IF textboxes(handle).ticktimer THEN TIMER(textboxes(handle).ticktimer) FREE
            FOR freesubtextbox = 1 TO textboxes(handle).textboxes
                TEXTBOXFREE textbox_textbox(1, freesubtextbox, handle)
            NEXT freesubtextbox
            IF handle = UBOUND(textboxes) AND handle > 1 THEN
                REDIM _PRESERVE textbox_bookmark(1 TO 2, 1 TO 1024, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_color(1 TO 3, 1 TO 1024, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_content(1 TO 9, 1 TO 32768, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_image(1 TO 8, 1 TO 1024, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_index(-127 TO 32640, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_link(1 TO 5, 1 TO 1024, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_textbox(1 TO 2, 1 TO 1024, 1 TO handle - 1) AS _UNSIGNED LONG
                REDIM _PRESERVE textbox_strings(0 TO 32767, 1 TO handle - 1) AS STRING
                REDIM _PRESERVE textbox_text(1 TO handle - 1) AS STRING
                REDIM _PRESERVE textboxes(1 TO handle - 1) AS TEXTBOX
            ELSE
                textboxes(handle).inuse = FALSE
            END IF
        END IF
END SELECT
END SUB

SUB TEXTBOXREFORMAT (handle AS INTEGER)
DIM copycontenty AS _BYTE
DIM copycontentx AS INTEGER
DIM reformatting AS INTEGER
DIM restoreline AS INTEGER
DIM savecontent AS INTEGER
DIM saveline AS INTEGER
DIM savetext AS STRING
DIM tempcontent(1 TO 9, 1 TO 32768) AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
saveline = textbox_index(textboxes(handle).screenline, handle)
savecontent = textboxes(handle).content
savetext = textbox_text(handle)
FOR copycontentx = 1 TO savecontent
    FOR copycontenty = 1 TO 9
        tempcontent(copycontenty, copycontentx) = textbox_content(copycontenty, copycontentx, handle)
        textbox_content(copycontenty, copycontentx, handle) = FALSE
NEXT copycontenty, copycontentx
TEXTBOXCLEAR handle
FOR reformatting = 1 TO savecontent
    SELECT CASE tempcontent(1, reformatting)
        CASE TEXTBOX_CONTENT_TEXT
            TEXTBOXADDTEXT handle, textbox_strings(tempcontent(4, reformatting), handle), 0 
        CASE TEXTBOX_CONTENT_IMAGE
            TEXTBOXADDIMAGE handle, tempcontent(3, reformatting), tempcontent(4, reformatting), tempcontent(5, reformatting), tempcontent(6, reformatting), tempcontent(7, reformatting), -tempcontent(2, reformatting), tempcontent(8, reformatting), tempcontent(9, reformatting)
        CASE TEXTBOX_CONTENT_TEXTBOX
    END SELECT
NEXT reformatting
FOR restoreline = textboxes(handle).textlines TO 1 STEP -1
    IF textbox_index(restoreline + 1, handle) > saveline AND textbox_index(restoreline, handle) < saveline THEN textboxes(handle).screenline = restoreline
NEXT restoreline
END SUB

SUB TEXTBOXREFORMATTEXT (handle AS INTEGER)
DIM reformatting AS INTEGER
DIM restoreline AS INTEGER
DIM saveline AS INTEGER
DIM savetext AS STRING
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
saveline = textbox_index(textboxes(handle).screenline, handle)
savetext = textbox_text(handle)
TEXTBOXCLEARTEXT handle
FOR reformatting = 1 TO textboxes(handle).content
    IF textbox_content(1, reformatting, handle) = TEXTBOX_CONTENT_TEXT THEN TEXTBOXADDTEXT handle, textbox_strings(textbox_content(4, reformatting, handle), handle), 0 
NEXT reformatting
FOR restoreline = textboxes(handle).textlines TO 1 STEP -1
    IF textbox_index(restoreline + 1, handle) > saveline AND textbox_index(restoreline, handle) < saveline THEN textboxes(handle).screenline = restoreline
NEXT restoreline
END SUB

SUB TEXTBOXRESIZE (handle AS INTEGER, left AS LONG, top AS LONG, right AS LONG, bottom AS LONG)
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF left > 1 THEN textboxes(handle).area.left = left
IF top > 1 THEN textboxes(handle).area.top = top
IF right > 1 THEN textboxes(handle).area.right = right
IF bottom > 1 THEN textboxes(handle).area.bottom = bottom
IF textboxes(handle).background THEN _FREEIMAGE textboxes(handle).background
textboxes(handle).background = TEXTBOXBACKGROUND(handle)
IF textboxes(handle).custombackground THEN TEXTBOXSETBACKGROUND handle, textboxes(handle).custombackground, textboxes(handle).custombackgroundbehaviour
IF textboxes(handle).page THEN _FREEIMAGE textboxes(handle).page
textboxes(handle).page = _NEWIMAGE(textboxes(handle).area.right - textboxes(handle).area.left + 1, textboxes(handle).area.bottom - textboxes(handle).area.top + 1, 32)
textboxes(handle).rows = INT((textboxes(handle).area.bottom - textboxes(handle).area.top - _FONTWIDTH(textboxes(handle).font)) / _FONTHEIGHT(textboxes(handle).font))
textboxes(handle).cols = INT(((textboxes(handle).area.right - textboxes(handle).area.left) / _FONTWIDTH(textboxes(handle).font)) + (2 * (textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON)))
TEXTBOXREFORMAT handle
END SUB

SUB TEXTBOXSENDTOBACK (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
TEXTBOXSETQUEUE handle, 1
END SUB

SUB TEXTBOXSETBACKGROUND (handle AS INTEGER, background AS LONG, behaviour AS _BYTE)
DIM scale AS SINGLE
DIM hd AS LONG
DIM hs AS LONG
DIM savedest AS LONG
DIM wd AS LONG
DIM ws AS LONG
SHARED textboxes() AS TEXTBOX
IF behaviour < TEXTBOX_BACKGROUND_STRETCH THEN behaviour = TEXTBOX_BACKGROUND_STRETCH
IF behaviour > TEXTBOX_BACKGROUND_CENTER THEN behaviour = TEXTBOX_BACKGROUND_CENTER
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF background > -2 THEN EXIT SUB
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
savedest = _DEST
_DEST textboxes(handle).background
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
_DEST savedest
END SUB

SUB TEXTBOXSETQUEUE (handle AS INTEGER, queue AS INTEGER)
DIM findqueue AS INTEGER
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
IF queue < 1 OR queue > UBOUND(textboxes) THEN EXIT SUB
FOR findqueue = 1 TO UBOUND(textboxes)
    IF textboxes(findqueue).queue = queue THEN SWAP textboxes(handle).queue, textboxes(findqueue).queue
NEXT findqueue
END SUB

SUB TEXTBOXSETTICK (handle AS INTEGER, tick AS LONG)
SHARED textboxes() AS TEXTBOX
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
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
DO
    textboxes(handle).tickindex = textboxes(handle).tickindex + 1
    IF textboxes(handle).tickindex = 1 THEN textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
    SELECT CASE MID$(textbox_strings(0, handle), textboxes(handle).tickindex, 1)
        CASE CHR$(13)
            textboxes(handle).textlines = textboxes(handle).textlines + 1
            textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines, handle)
            textboxes(handle).textlinehold = textboxes(handle).textlinehold - 1
            IF textboxes(handle).textlinehold < 0 THEN textboxes(handle).textlinehold = 0
            IF textboxes(handle).type = TEXTBOX_TEXT_DYNAMIC THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
            IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN textboxes(handle).screenline = 32640
        CASE CHR$(255)
            textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines + 1, handle) + 1
        CASE ELSE
            textbox_index(textboxes(handle).textlines + 1, handle) = textbox_index(textboxes(handle).textlines + 1, handle) + 1
            EXIT DO
    END SELECT
LOOP UNTIL textboxes(handle).tickindex >= LEN(textbox_strings(0, handle))
IF textbox_index(textboxes(handle).textlines + 1, handle) >= LEN(textbox_text(handle)) THEN 
    textbox_index(textboxes(handle).textlines + 1, handle) = LEN(textbox_text(handle)) + 1
ELSE
    IF textboxes(handle).tick THEN _SNDPLAY textboxes(handle).tick
END IF
IF textboxes(handle).tickindex >= LEN(textbox_strings(0, handle)) THEN
    TIMER(textboxes(handle).ticktimer) OFF
    TIMER(textboxes(handle).ticktimer) FREE
    textboxes(handle).ticktimer = FALSE
    textboxes(handle).lasttext = TEXTBOX_TICK
END IF
END SUB

SUB UPDATETEXTBOX (handle AS INTEGER)
DIM linkvalue AS _BYTE
DIM checkbookmarks AS INTEGER
DIM checklines AS INTEGER
DIM checksubtextbox AS INTEGER
DIM endline AS INTEGER
DIM updatelinks AS INTEGER
DIM updatesubtextbox AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
SELECT CASE handle
    CASE TEXTBOX_ALLTEXTBOXES
        FOR handle = 1 TO UBOUND(textboxes)
            IF textboxes(handle).subtextbox = FALSE THEN UPDATETEXTBOX handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
        DO
            SELECT CASE textboxes(handle).input
                CASE TEXTBOX_INPUT_DIRECT
                    IF NOT (isonbox(MOUSEX, MOUSEY, textboxes(handle).area.left, textboxes(handle).area.top, textboxes(handle).area.right, textboxes(handle).area.bottom)) THEN EXIT DO
                CASE TEXTBOX_INPUT_INDIRECT
                    IF isonbox(MOUSEX, MOUSEY, textboxes(handle).area.left, textboxes(handle).area.top, textboxes(handle).area.right, textboxes(handle).area.bottom) THEN EXIT DO
            END SELECT
            FOR checksubtextbox = 1 TO textboxes(handle).textboxes
                IF isonbox(MOUSEX, MOUSEY, textboxes(textbox_textbox(1, checksubtextbox, handle)).area.left, textboxes(textbox_textbox(1, checksubtextbox, handle)).area.top, textboxes(textbox_textbox(1, checksubtextbox, handle)).area.right, textboxes(textbox_textbox(1, checksubtextbox, handle)).area.bottom) THEN EXIT DO
            NEXT checksubtextbox
            textboxes(handle).screenline = textboxes(handle).screenline + MOUSEWHEEL
            IF DKEY(DKEY_UP) THEN textboxes(handle).screenline = textboxes(handle).screenline - 1
            IF DKEY(DKEY_DOWN) THEN textboxes(handle).screenline = textboxes(handle).screenline + 1
            IF K = Null + CHR$(73) THEN textboxes(handle).screenline = textboxes(handle).screenline - textboxes(handle).rows + 1
            IF K = Null + CHR$(81) THEN textboxes(handle).screenline = textboxes(handle).screenline + textboxes(handle).rows - 1
            IF HME THEN textboxes(handle).screenline = 1
            IF EDK THEN textboxes(handle).screenline = (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1
            IF K > "" AND textboxes(handle).ticktimer > 0 THEN
                DO
                    TEXTBOXTICK handle
                LOOP UNTIL textboxes(handle).ticktimer = FALSE
            END IF
            EXIT DO
        LOOP
        IF textboxes(handle).scrollbar = TEXTBOX_SCROLLBAR_ON AND isonbox(MOUSEX, MOUSEY, textboxes(handle).area.right - (2 * (_FONTWIDTH(textboxes(handle).font))) + 1, textboxes(handle).area.top - 1, textboxes(handle).area.right - 1, textboxes(handle).area.bottom + 1) AND CLICK THEN textboxes(handle).scrollbargrab = TRUE
        IF NOT CLICK THEN textboxes(handle).scrollbargrab = FALSE
        IF textboxes(handle).scrollbargrab THEN textboxes(handle).screenline = (MOUSEY - textboxes(handle).area.top - 2) / (textboxes(handle).area.bottom - textboxes(handle).area.top - 4) * ((textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1)
        IF textboxes(handle).screenline < 1 THEN textboxes(handle).screenline = 1
        IF textboxes(handle).screenline > (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1 THEN textboxes(handle).screenline = (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1
        IF (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) <= textboxes(handle).rows THEN
            IF textboxes(handle).type = TEXTBOX_TEXT_REVERSE OR textboxes(handle).type = TEXTBOX_TEXT_REVERSE_STRICT THEN
                textboxes(handle).screenline = -textboxes(handle).rows + (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) + 1
            ELSE
                textboxes(handle).screenline = 1
            END IF
        END IF
        IF textboxes(handle).link = TEXTBOX_LINK_ON AND (MOVEMOUSE OR CLICKCHANGE) THEN
            endline = textboxes(handle).screenline + textboxes(handle).rows - 1

            IF (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) < textboxes(handle).screenline + textboxes(handle).rows THEN endline = textboxes(handle).screenline + textboxes(handle).rows
            FOR updatelinks = 1 TO textboxes(handle).links
                linkvalue = TEXTBOX_LINKSTATUS_STAND

                FOR checklines = textboxes(handle).screenline TO endline
                    IF NOT ((textbox_link(1, updatelinks, handle) < textbox_index(checklines, handle) AND textbox_link(2, updatelinks, handle) < textbox_index(checklines, handle)) OR (textbox_link(1, updatelinks, handle) > textbox_index(checklines + 1, handle) AND textbox_link(2, updatelinks, handle) > textbox_index(checklines + 1, handle))) AND textbox_link(3, updatelinks, handle) > TEXTBOX_LINK_OFF THEN
                        IF textbox_link(1, updatelinks, handle) < textbox_index(checklines, handle) THEN
                            IF textbox_link(2, updatelinks, handle) > textbox_index(checklines + 1, handle) THEN
                                IF isonbox(MOUSEX, MOUSEY, textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_index(checklines + 1, handle) - textbox_index(checklines, handle) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF CLICK THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            ELSE
                                IF isonbox(Mousedata(1), Mousedata(2), textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_link(2, updatelinks, handle) - textbox_index(checklines, handle)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) + 1) THEN
                                    IF CLICK THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            END IF
                        ELSE
                            IF textbox_link(2, updatelinks, handle) > textbox_index(checklines + 1, handle) THEN
                                IF isonbox(MOUSEX, MOUSEY, textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_link(1, updatelinks, handle) - textbox_index(checklines, handle)) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_index(checklines + 1, handle) - textbox_index(checklines, handle) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF CLICK THEN
                                        IF linkvalue < TEXTBOX_LINKSTATUS_ACTIVE THEN linkvalue = TEXTBOX_LINKSTATUS_ACTIVE
                                    ELSE
                                        IF linkvalue < TEXTBOX_LINKSTATUS_HOVER THEN linkvalue = TEXTBOX_LINKSTATUS_HOVER
                                    END IF
                                ELSE
                                    IF linkvalue < TEXTBOX_LINKSTATUS_STAND THEN linkvalue = TEXTBOX_LINKSTATUS_STAND
                                END IF
                            ELSE
                                IF isonbox(MOUSEX, MOUSEY, textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_link(1, updatelinks, handle) - textbox_index(checklines, handle) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline), textboxes(handle).area.left + _FONTWIDTH(textboxes(handle).font) + (textbox_link(2, updatelinks, handle) - textbox_index(checklines, handle) - 1) * _FONTWIDTH(textboxes(handle).font), textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + _FONTHEIGHT * (checklines - textboxes(handle).screenline + 1) - 1) THEN
                                    IF CLICK THEN
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
                textbox_link(5, updatelinks, handle) = linkvalue
                SELECT CASE linkvalue
                    CASE TEXTBOX_LINKSTATUS_STAND
                        textbox_color(3, textbox_link(4, updatelinks, handle), handle) = textboxes(handle).linkstand
                    CASE TEXTBOX_LINKSTATUS_HOVER
                        textbox_color(3, textbox_link(4, updatelinks, handle), handle) = textboxes(handle).linkhover
                    CASE TEXTBOX_LINKSTATUS_ACTIVE
                        textbox_color(3, textbox_link(4, updatelinks, handle), handle) = textboxes(handle).linkactive
                END SELECT
            NEXT updatelinks
        END IF
        FOR checkbookmarks = 1 TO textboxes(handle).bookmarks
            IF LINKSTATUS(handle, textbox_bookmark(1, checkbookmarks, handle)) = TEXTBOX_LINKSTATUS_ACTIVE THEN
                textboxes(handle).screenline = textbox_bookmark(2, checkbookmarks, handle) + 1
                IF textboxes(handle).screenline < 1 THEN textboxes(handle).screenline = 1
                IF textboxes(handle).screenline > (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1 THEN textboxes(handle).screenline = (textboxes(handle).textlines + textboxes(handle).textlinehold + ((textboxes(handle).ticktimer = FALSE) AND (textboxes(handle).textlinehold = FALSE))) - textboxes(handle).rows + 1
            END IF
        NEXT checkbookmarks
        FOR updatesubtextbox = 1 TO textboxes(handle).textboxes
            UPDATETEXTBOX textbox_textbox(1, updatesubtextbox, handle)
        NEXT updatesubtextbox
END SELECT
END SUB

FUNCTION CLONETEXTBOX% (handle AS INTEGER)
DIM clonebookmarky AS _BYTE
DIM clonecolory AS _BYTE
DIM clonecontenty AS _BYTE
DIM clonelinky AS _BYTE
DIM cloneimagey AS _BYTE
DIM clonebookmarkx AS INTEGER
DIM clonecolorx AS INTEGER
DIM clonecontentx AS INTEGER
DIM cloneimagex AS INTEGER
DIM cloneindex AS INTEGER
DIM clonelinkx AS INTEGER
DIM clonestrings AS INTEGER
DIM clonetb AS INTEGER
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
DO
    IF UBOUND(textboxes) THEN
        FOR CLONETEXTBOX = 1 TO UBOUND(textboxes)
            IF textboxes(CLONETEXTBOX).inuse = FALSE THEN EXIT DO
        NEXT CLONETEXTBOX
    END IF
    CLONETEXTBOX = CLONETEXTBOX + 1
    REDIM _PRESERVE textbox_bookmark(1 TO 2, 1 TO 1024, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_color(1 TO 3, 1 TO 1024, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_content(1 TO 9, 1 TO 32768, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_image(1 TO 8, 1 TO 1024, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_index(-127 TO 32640, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_link(1 TO 5, 1 TO 1024, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_textbox(1 TO 2, 1 TO 1024, 1 TO CLONETEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_strings(0 TO 32767, 1 TO CLONETEXTBOX) AS STRING
    REDIM _PRESERVE textbox_text(1 TO CLONETEXTBOX) AS STRING
    REDIM _PRESERVE textboxes(1 TO CLONETEXTBOX) AS TEXTBOX
    EXIT DO
LOOP
textboxes(handle) = textboxes(CLONETEXTBOX)
textboxes(handle).page = _NEWIMAGE(_WIDTH(textboxes(CLONETEXTBOX).page), _HEIGHT(textboxes(CLONETEXTBOX).page), 32)
PCOPY textboxes(CLONETEXTBOX).page, textboxes(handle).page
FOR clonebookmarkx = 1 TO 1024
    FOR clonebookmarky = 1 TO 2
        textbox_bookmark(clonebookmarky, clonebookmarkx, CLONETEXTBOX) = textbox_bookmark(clonebookmarky, clonebookmarkx, handle)
    NEXT clonebookmarky
    IF textbox_bookmark(1, clonebookmarkx, CLONETEXTBOX) = FALSE AND textbox_bookmark(1, clonebookmarkx, handle) = FALSE AND clonebookmarkx > 1 THEN EXIT FOR
NEXT clonebookmarkx
FOR clonecolorx = 1 TO 1024
    FOR clonecolory = 1 TO 3
        textbox_color(clonecolory, clonecolorx, CLONETEXTBOX) = textbox_color(clonecolory, clonecolorx, handle)
    NEXT clonecolory
    IF textbox_color(1, clonecolorx, CLONETEXTBOX) = FALSE AND textbox_color(1, clonecolorx, handle) = FALSE AND clonecolorx > 1 THEN EXIT FOR
NEXT clonecolorx
FOR cloneimagex = 1 TO 1024
    FOR cloneimagey = 1 TO 8
        textbox_image(cloneimagey, cloneimagex, CLONETEXTBOX) = textbox_image(cloneimagey, cloneimagex, handle)
    NEXT cloneimagey
    IF textbox_image(1, cloneimagex, CLONETEXTBOX) = FALSE AND textbox_image(1, cloneimagex, handle) = FALSE AND cloneimagex > 1 THEN EXIT FOR
NEXT cloneimagex
FOR cloneindex = -127 TO 32640
    textbox_index(cloneindex, CLONETEXTBOX) = textbox_index(cloneindex, handle)
    IF textbox_index(cloneindex, CLONETEXTBOX) = FALSE AND textbox_index(cloneindex, handle) = FALSE AND cloneindex > 1 THEN EXIT FOR
NEXT cloneindex
FOR clonecontentx = 1 TO 32768
    FOR clonecontenty = 1 TO 9
        textbox_content(clonecontenty, clonecontentx, CLONETEXTBOX) = textbox_content(clonecontenty, clonecontentx, handle)
    NEXT clonecontenty
    IF textbox_content(1, clonecontentx, CLONETEXTBOX) = FALSE AND textbox_content(1, clonecontentx, handle) = FALSE AND clonecontentx > 1 THEN EXIT FOR
NEXT clonecontentx
FOR clonelinkx = 1 TO 1024
    FOR clonelinky = 1 TO 5
        textbox_link(clonelinky, clonelinkx, CLONETEXTBOX) = textbox_link(clonelinky, clonelinkx, handle)
    NEXT clonelinky
    IF textbox_link(1, clonelinkx, CLONETEXTBOX) = FALSE AND textbox_link(1, clonelinkx, handle) = FALSE AND clonelinkx > 1 THEN EXIT FOR
NEXT clonelinkx
FOR clonestrings = 0 TO 32767
    textbox_strings(clonestrings, CLONETEXTBOX) = textbox_strings(clonestrings, handle)
NEXT clonestrings
FOR clonetb = 1 TO 1024
    IF textbox_textbox(1, clonetb, CLONETEXTBOX) = FALSE AND textbox_textbox(1, clonetb, handle) = FALSE AND clonetb > 1 THEN EXIT FOR
    textbox_textbox(1, clonetb, CLONETEXTBOX) = CLONETEXTBOX(textbox_textbox(1, clonetb, handle))
    textbox_textbox(2, clonetb, CLONETEXTBOX) = textboxes(CLONETEXTBOX).images
NEXT clonetb
textbox_text(CLONETEXTBOX) = textbox_text(handle)
END FUNCTION

FUNCTION LINKSTATUS%% (handle AS INTEGER, linkcode AS INTEGER)
DIM findlink AS INTEGER
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
FOR findlink = 1 TO textboxes(handle).links
    IF textbox_link(3, findlink, handle) = linkcode THEN
        LINKSTATUS = textbox_link(5, findlink, handle)
        textbox_link(5, findlink, handle) = TEXTBOX_LINKSTATUS_STAND
        EXIT FOR
    END IF
NEXT findlink
END FUNCTION

FUNCTION NEWTEXTBOX% (left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, status AS _BYTE, boxtype AS _BYTE, inputtype AS _BYTE, dest AS LONG, font AS LONG, fontcolor AS _UNSIGNED LONG, backgroundcolor AS _UNSIGNED LONG, framecolor AS _UNSIGNED LONG, scrollbar AS _BYTE, link AS _BYTE)
SHARED textbox_bookmark() AS _UNSIGNED LONG
SHARED textbox_color() AS _UNSIGNED LONG
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_index() AS _UNSIGNED LONG
SHARED textbox_link() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textbox_strings() AS STRING
SHARED textbox_text() AS STRING
SHARED textboxes() AS TEXTBOX
IF status < TEXTBOX_SHOW THEN status = TEXTBOX_SHOW
IF status > TEXTBOX_HIDE THEN status = TEXTBOX_HIDE
IF behaviour < TEXTBOX_TEXT_STATIC THEN behaviour = TEXTBOX_TEXT_STATIC
IF behaviour > TEXTBOX_TITLE THEN behaviour = TEXTBOX_TITLE
IF scrollbar < TEXTBOX_SCROLLBAR_ON THEN scrollbar = TEXTBOX_SCROLLBAR_ON
IF scrollbar > TEXTBOX_SCROLLBAR_OFF THEN scrollbar = TEXTBOX_SCROLLBAR_OFF
IF font = FALSE THEN font = TEXTBOX_DEFAULTFONT
DO
    IF UBOUND(textboxes) THEN
        FOR NEWTEXTBOX = 1 TO UBOUND(textboxes)
            IF textboxes(NEWTEXTBOX).inuse = FALSE THEN EXIT DO
        NEXT NEWTEXTBOX
    END IF
    NEWTEXTBOX = NEWTEXTBOX + 1
    REDIM _PRESERVE textbox_bookmark(1 TO 2, 1 TO 1024, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_color(1 TO 3, 1 TO 1024, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_content(1 TO 9, 1 TO 32768, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_image(1 TO 8, 1 TO 1024, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_index(-127 TO 32640, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_link(1 TO 5, 1 TO 1024, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_textbox(1 TO 2, 1 TO 1024, 1 TO NEWTEXTBOX) AS _UNSIGNED LONG
    REDIM _PRESERVE textbox_strings(0 TO 32767, 1 TO NEWTEXTBOX) AS STRING
    REDIM _PRESERVE textbox_text(1 TO NEWTEXTBOX) AS STRING
    REDIM _PRESERVE textboxes(1 TO NEWTEXTBOX) AS TEXTBOX
    EXIT DO
LOOP
textboxes(NEWTEXTBOX).inuse = TRUE
textboxes(NEWTEXTBOX).subtextbox = FALSE
textboxes(NEWTEXTBOX).queue = NEWTEXTBOX
textboxes(NEWTEXTBOX).status = status
textboxes(NEWTEXTBOX).type = boxtype
textboxes(NEWTEXTBOX).behaviour = behaviour
textboxes(NEWTEXTBOX).input = inputtype
textboxes(NEWTEXTBOX).area.left = left
textboxes(NEWTEXTBOX).area.top = top
textboxes(NEWTEXTBOX).area.right = right
textboxes(NEWTEXTBOX).area.bottom = bottom
textboxes(NEWTEXTBOX).bound = FALSE
textboxes(NEWTEXTBOX).boundary.left = FALSE
textboxes(NEWTEXTBOX).boundary.top = FALSE
textboxes(NEWTEXTBOX).boundary.right = FALSE
textboxes(NEWTEXTBOX).boundary.bottom = FALSE
IF textboxes(NEWTEXTBOX).page THEN _FREEIMAGE(textboxes(NEWTEXTBOX).page)
textboxes(NEWTEXTBOX).page = _NEWIMAGE(right - left +1, bottom - top + 1, 32)
textboxes(NEWTEXTBOX).dest = dest
textboxes(NEWTEXTBOX).font = font
textboxes(NEWTEXTBOX).color = fontcolor
textboxes(NEWTEXTBOX).backgroundcolor = backgroundcolor
textboxes(NEWTEXTBOX).background = TEXTBOXBACKGROUND(NEWTEXTBOX)
textboxes(NEWTEXTBOX).custombackground = FALSE
textboxes(NEWTEXTBOX).custombackgroundbehaviour = FALSE
textboxes(NEWTEXTBOX).scrollbar = scrollbar
IF scrollbar = TEXTBOX_SCROLLBAR_ON THEN SCROLLBARSET NEWTEXTBOX, 0, TEXTBOX_SCROLLBAR_BOX_DEFAULT, TEXTBOX_SCROLLBAR_NOLINE, TEXTBOX_SCROLLBAR_BOX
textboxes(NEWTEXTBOX).link = link
LINKSET NEWTEXTBOX, TEXTBOX_LINK_STAND, TEXTBOX_LINK_HOVER, TEXTBOX_LINK_ACTIVE, TEXTBOX_LINK_UNDERLINE_ON
textboxes(NEWTEXTBOX).frame = framecolor
textboxes(NEWTEXTBOX).textstrings = 0
textboxes(NEWTEXTBOX).textlines = 0
textboxes(NEWTEXTBOX).textlinehold = FALSE
textboxes(NEWTEXTBOX).lasttext = TEXTBOX_NOTICK
textboxes(NEWTEXTBOX).bookmarks = FALSE
textboxes(NEWTEXTBOX).colors = FALSE
textboxes(NEWTEXTBOX).images = FALSE
textboxes(NEWTEXTBOX).links = FALSE
textboxes(NEWTEXTBOX).textboxes = FALSE
textboxes(NEWTEXTBOX).screenline = 1
textboxes(NEWTEXTBOX).rows = INT((bottom - top - _FONTWIDTH(font)) / _FONTHEIGHT(font))
textboxes(NEWTEXTBOX).cols = INT(((right - left) / _FONTWIDTH(font)) + (2 * (scrollbar = TEXTBOX_SCROLLBAR_ON)))
textboxes(NEWTEXTBOX).ticktimer = FALSE
textboxes(NEWTEXTBOX).tick = FALSE
END FUNCTION

FUNCTION TEXTBOXADDTEXTBOX% (handle AS INTEGER, row AS INTEGER, left AS LONG, top AS LONG, right AS LONG, bottom AS LONG, boxtype AS _BYTE, fontcolor AS _UNSIGNED LONG, backgroundcolor AS _UNSIGNED LONG, framecolor AS _UNSIGNED LONG, scrollbar AS _BYTE, link AS _BYTE)
SHARED textbox_content() AS _UNSIGNED LONG
SHARED textbox_image() AS _UNSIGNED LONG
SHARED textbox_textbox() AS _UNSIGNED LONG
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT SUB
textboxes(handle).content = textboxes(handle).content + 1
textbox_content(1, textboxes(handle).content, handle) = TEXTBOX_CONTENT_TEXTBOX
textbox_content(3, textboxes(handle).content, handle) = row
textbox_content(4, textboxes(handle).content, handle) = left
textbox_content(5, textboxes(handle).content, handle) = top
textbox_content(6, textboxes(handle).content, handle) = right
textbox_content(7, textboxes(handle).content, handle) = bottom
bottom = bottom - top
top = top + textboxes(handle).area.top + _FONTWIDTH(textboxes(handle).font) + ((textboxes(handle).textlines + row - 1) * _FONTHEIGHT(textboxes(handle).font))
bottom = bottom + top
TEXTBOXADDTEXTBOX = NEWTEXTBOX(left, top, right, bottom, TEXTBOX_SHOW, boxtype, TEXTBOX_INPUT_DIRECT, textboxes(handle).dest, textboxes(handle).font, fontcolor, backgroundcolor, framecolor, scrollbar, link)
textbox_content(2, textboxes(handle).content, handle) = TEXTBOXADDTEXTBOX
textboxes(TEXTBOXADDTEXTBOX).subtextbox = handle
textboxes(handle).textboxes = textboxes(handle).textboxes + 1
TEXTBOXADDIMAGE handle, row, left - textboxes(handle).area.left, top, right - textboxes(handle).area.left, bottom, TEXTBOX_TEXTBOXIMAGE, TEXTBOX_NOBORDER, TEXTBOX_IMAGE_SURROUND
TEXTBOXRESIZE TEXTBOXADDTEXTBOX, textbox_image(2, textboxes(handle).images, handle) + textboxes(handle).area.left, textbox_image(3, textboxes(handle).images, handle) + textboxes(handle).area.top, textbox_image(4, textboxes(handle).images, handle) + textboxes(handle).area.left, textbox_image(5, textboxes(handle).images, handle) + textboxes(handle).area.top
TEXTBOXBOUND TEXTBOXADDTEXTBOX, textboxes(handle).area.left + 1, textboxes(handle).area.top + 1, textboxes(handle).area.right - 2, textboxes(handle).area.bottom - 2
textbox_textbox(1, textboxes(handle).textboxes, handle) = TEXTBOXADDTEXTBOX
textbox_textbox(2, textboxes(handle).textboxes, handle) = textboxes(handle).images
END FUNCTION

FUNCTION TEXTBOXBACKGROUND& (handle AS INTEGER)
DIM savedest AS LONG
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXBACKGROUND& = _NEWIMAGE(textboxes(handle).area.right - textboxes(handle).area.left, textboxes(handle).area.bottom - textboxes(handle).area.top, 32)
savedest = _DEST
_DEST TEXTBOXBACKGROUND&
PAINT (1, 1), textboxes(handle).backgroundcolor
_DEST savedest
END FUNCTION

FUNCTION TEXTBOXQUEUE% (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXQUEUE = textboxes(handle).queue
END FUNCTION

FUNCTION TEXTBOXSTATUS` (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
IF NOT VALIDTEXTBOX(handle) THEN EXIT FUNCTION
TEXTBOXSTATUS = textboxes(handle).status
END FUNCTION

FUNCTION VALIDTEXTBOX` (handle AS INTEGER)
SHARED textboxes() AS TEXTBOX
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(textboxes) THEN EXIT FUNCTION
IF textboxes(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDTEXTBOX = TRUE
END FUNCTION