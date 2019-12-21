'******************************************************************************
'
'Gorlock's QB64 Layer Library
'v0.4
'
'******************************************************************************
'
'Version 0.1  - 2013-08-17
'Version 0.11 - 2013-10-04
'  - added tutorial and init include file
'Version 0.2  - 2013-10-16
'  - added queue and swap commands
'Version 0.3  - 2013-10-24
'  - added fade commands
'Version 0.4  - 2013-12-02
'  - fixed minor errors
'  - added CLEARLAYER and MERGELAYER
'
'******************************************************************************
'
'SUB       CLEARLAYER            (handle%)                     Clears a layer's image
'SUB       GENERATETRANSLAYER    (handle%)                     Creates a transparent layer image for a layer
'SUB       LAYERBRINGTOFRONT     (handle%)                     Sets a layer's queue to be on top of all other layers
'SUB       LAYERFADE             (handle%, trans~%%, speed%)   Fades a layer to a transparency value
'SUB       LAYERFADESTART        (handle%)                     Starts the layer fade
'SUB       LAYERFADESTOP         (handle%)                     Stops the layer fade
'SUB       LAYERFADEUPDATE       (handle%)                     Updates a layer fade progression
'SUB       LAYERFREE             (handle%)                     Frees the handle of a layer
'SUB       LAYERHIDE             (handle%)                     Hides a layer that had been shown
'SUB       LAYERPUT              (x&, y&, handle%, dest&)      Sets a layer's location and destination
'SUB       LAYERSENDTOBACK       (handle%)                     Sets a layer's queue to be underneath all other layers
'SUB       LAYERSETQUEUE         (handle%, queue%)             Sets a layer's queue
'SUB       LAYERSETTRANSPARENCY  (handle%, trans~%%)           Sets the transparency of a layer to a certain value
'SUB       LAYERSHOW             (handle%)                     Shows a layer that had been hidden
'SUB       MARGELAYER            (handle1%, x&, y&, handle2%)  Merges one layer onto another layer
'SUB       PRINTLAYER            (handle%)                     Prints a layer image to the screen
'SUB       SWAPLAYER             (handle1%, handle2%)          Swaps two layers positions in the array
'FUNCTION  LAYER%                (handle%)                     Returns the handle of the image of a layer
'FUNCTION  LAYERDEST&            (handle%)                     Returns the destination of a layer
'FUNCTION  LAYERQUEUE&           (handle%)                     Returns the queue value of a layer
'FUNCTION  LAYERTRANSPARENCY~%%  (handle%)                     Returns the transparency of a layer
'FUNCTION  LAYERSTATUS%%         (handle%)                     Returns the status of a layer
'FUNCTION  LAYERX!               (handle%)                     Returns the x location of a layer
'FUNCTION  LAYERY!               (handle%)                     Returns the y lovation of a layer
'FUNCTION  NEWLAYER%             (x&, y&, mother%, status%%)   Creates a new layer
'FUNCTION  VALIDLAYER`           (handle%)                     Checks to see if a layer handle is valid or not
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/JhP7jsXO
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Animation Library: https://db.tt/NZjp4Y8y
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/vslFqs8F
'
'******************************************************************************

SUB CLEARLAYER (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle1) THEN EXIT SUB
SELECT CASE handle
    CASE LAYER_ALLLAYERS
        FOR handle = UBOUND(layers) TO 1 STEP -1
            CLEARLAYER handle
        NEXT handle
    CASE ELSE
        CLS LAYER(handle)
END SELECT
END SUB

SUB GENERATETRANSLAYER (handle AS INTEGER)
DIM a AS _UNSIGNED _BYTE
DIM o AS LONG
DIM m AS _MEM
DIM m1 AS _MEM
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
IF layers(handle).layerbak = FALSE THEN
    layers(handle).layerbak = _NEWIMAGE(_WIDTH(layers(handle).layer), _HEIGHT(layers(handle).layer), 32)
    PCOPY layers(handle).layer, layers(handle).layerbak
END IF
IF layers(handle).trans = 255 THEN
    PCOPY layers(handle).layerbak, layers(handle).layer
    _FREEIMAGE layers(handle).layerbak
    layers(handle).layerbak = FALSE
    EXIT SUB
END IF
m = _MEMIMAGE(layers(handle).layerbak)
m1 = _MEMIMAGE(layers(handle).layer)
$CHECKING:OFF
o = 3
DO
    _MEMGET m, m.OFFSET + o, a
    IF a <= 255 THEN
        a = a * layers(handle).trans \ 255
    ELSE
        a = layers(handle).trans
    END IF
    _MEMPUT m1, m1.OFFSET + o, a
    o = o + 4
LOOP UNTIL o > m.SIZE
$CHECKING:ON
_MEMFREE m
_MEMFREE m1
END SUB

SUB LAYERBRINGTOFRONT (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERSETQUEUE handle, UBOUND(layers)
END SUB

SUB LAYERFADE (handle AS INTEGER, trans AS _UNSIGNED _BYTE, speed AS INTEGER)
SHARED layers() AS LAYER
SHARED animations() AS ANIMATION
IF NOT VALIDLAYER(handle) THEN EXIT SUB
IF layers(handle).fade > 0 THEN
    STOPANIMATION layers(handle).fade
    LAYERSETTRANSPARENCY handle, animations(layers(handle).fade).x2
    ANIMATIONFREE layers(handle).fade
END IF
layers(handle).fade = NEWANIMATION(layers(handle).trans, 0, trans, 0, speed, "", ANIMATION_BASIC, ANIMATION_AUTOUPDATE_OFF, ANIMATION_AUTOTERMINATE_OFF)
layers(handle).timer = _FREETIMER
ON TIMER(layers(handle).timer, .025) LAYERFADEUPDATE handle
LAYERFADESTART handle
END SUB

SUB LAYERFADESTART (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
STARTANIMATION layers(handle).fade
TIMER(layers(handle).timer) ON
END SUB

SUB LAYERFADESTOP (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
STOPANIMATION layers(handle).fade
TIMER(layers(handle).timer) OFF
END SUB

SUB LAYERFADEUPDATE (handle AS INTEGER)
SHARED layers() AS LAYER
SHARED animations() AS ANIMATION
IF NOT VALIDLAYER(handle) THEN EXIT SUB
UPDATEANIMATION layers(handle).fade
IF ANIMATIONSTATUS(layers(handle).fade) = ANIMATION_FINISHED THEN
    ANIMATIONFREE layers(handle).fade
    layers(handle).fade = FALSE
    TIMER(layers(handle).timer) FREE
    layers(handle).timer = FALSE
ELSE
    LAYERSETTRANSPARENCY handle, ANIMATIONX(layers(handle).fade)
END IF
END SUB

SUB LAYERFREE (handle AS INTEGER)
DIM checkfonts aS _BYTE
SHARED layers() AS LAYER
SELECT CASE handle
    CASE LAYER_ALLLAYERS
        FOR handle = UBOUND(layers) TO 1 STEP -1
            LAYERFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDLAYER(handle) THEN EXIT SUB
        FOR checkfonts = 1 TO UBOUND(fonts)
            IF _FONT(layers(handle).layer) = fonts(checkfonts) THEN _FONT 16, layers(handle).layer
        NEXT checkfonts
        IF layers(handle).layer THEN _FREEIMAGE (layers(handle).layer)
        IF layers(handle).layerbak THEN _FREEIMAGE (layers(handle).layerbak)
        IF layers(handle).fade THEN ANIMATIONFREE layers(handle).fade
        IF layers(handle).timer THEN TIMER(layers(handle).timer) FREE
        IF handle = UBOUND(layers) AND handle > 1 THEN
            REDIM _PRESERVE layers(1 TO UBOUND(layers) - 1) AS LAYER
        ELSE
            layers(handle).inuse = FALSE
        END IF
END SELECT
END SUB

SUB LAYERHIDE (handle AS INTEGER)
SHARED layers() AS LAYER
SELECT CASE handle
    CASE LAYERS_ALLLAYERS
        FOR handle = UBOUND(layers) TO 1 STEP -1
            IF VALIDLAYER(handle) THEN layers(handle).status = LAYER_HIDE
        NEXT handle
    CASE ELSE
        IF NOT VALIDLAYER(handle) THEN EXIT SUB
        layers(handle).status = LAYER_HIDE
END SELECT
END SUB

SUB LAYERPUT (x AS LONG, y AS LONG, handle AS INTEGER, dest AS LONG)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
layers(handle).x = x
layers(handle).y = y
layers(handle).dest = dest
END SUB

SUB LAYERSENDTOBACK (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERSETQUEUE handle, 1
END SUB

SUB LAYERSETQUEUE (handle AS INTEGER, queue AS INTEGER)
DIM findqueue AS INTEGER
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
IF queue < 1 OR queue > UBOUND(layers) THEN EXIT SUB
FOR findqueue = 1 TO UBOUND(layers)
    IF layers(findqueue).queue = queue THEN SWAP layers(handle).queue, layers(findqueue).queue
NEXT findqueue
END SUB

SUB LAYERSETTRANSPARENCY (handle AS INTEGER, trans AS _UNSIGNED _BYTE)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
IF trans < 0 THEN trans = 0
IF trans > 255 THEN trans = 255
layers(handle).trans = trans
GENERATETRANSLAYER handle
END SUB

SUB LAYERSHOW (handle AS INTEGER)
SHARED layers() AS LAYER
SELECT CASE handle
    CASE LAYERS_ALLLAYERS
        FOR handle = UBOUND(layers) TO 1 STEP -1
            IF VALIDLAYER(handle) THEN layers(handle).status = LAYER_SHOW
        NEXT handle
    CASE ELSE
        IF NOT VALIDLAYER(handle) THEN EXIT SUB
        layers(handle).status = LAYER_SHOW
END SELECT
END SUB

SUB MERGELAYER (handle1 AS INTEGER, x AS LONG, y AS LONG, handle2 AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle1) THEN EXIT SUB
IF NOT VALIDLAYER(handle2) THEN EXIT SUB
_DEST LAYER(handle2)
_PUTIMAGE (x, y), LAYER(handle1)
_DEST 0
END SUB

SUB PRINTLAYER (handle AS INTEGER)
DIM queue AS INTEGER
SHARED layers() AS LAYER
SELECT CASE handle
    CASE LAYER_ALLLAYERS
        FOR queue = 1 TO UBOUND(layers)
                FOR handle = 1 TO UBOUND(layers)
                    IF layers(handle).queue = queue THEN PRINTLAYER handle
        NEXT handle, queue
    CASE ELSE
        IF NOT VALIDLAYER(handle) THEN EXIT SUB
        IF layers(handle).status = LAYER_HIDE THEN EXIT SUB
        IF layers(handle).dest THEN _DEST layers(handle).dest
        _PUTIMAGE (layers(handle).x, layers(handle).y), layers(handle).layer
        IF layers(handle).dest THEN _DEST 0
END SELECT
END SUB

SUB SWAPLAYER (handle1 AS INTEGER, handle2 AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle1) THEN EXIT SUB
IF NOT VALIDLAYER(handle2) THEN EXIT SUB
SWAP layers(handle1), layers(handle2)
END SUB

FUNCTION LAYER% (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYER = layers(handle).layer
END FUNCTION

FUNCTION LAYERDEST& (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERDEST = layers(handle).dest
END FUNCTION

FUNCTION LAYERQUEUE& (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERQUEUE = layers(handle).queue
END FUNCTION

FUNCTION LAYERSTATUS%% (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERSTATUS = layers(handle).status
END FUNCTION

FUNCTION LAYERTRANSPARENCY~%% (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERTRANSPARENCY = layers(handle).trans
END FUNCTION

FUNCTION LAYERX! (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERX = layers(handle).x
END FUNCTION

FUNCTION LAYERY! (handle AS INTEGER)
SHARED layers() AS LAYER
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERY = layers(handle).y
END FUNCTION

FUNCTION NEWLAYER% (x AS LONG, y AS LONG, mother AS INTEGER, status AS _BYTE)
SHARED layers() AS LAYER
IF status < LAYER_SHOW THEN status = LAYER_SHOW
IF status > LAYER_HIDE THEN status = LAYER_HIDE
DO
    FOR NEWLAYER = 1 TO UBOUND(layers)
        IF layers(NEWLAYER).inuse = FALSE THEN EXIT DO
    NEXT NEWLAYER
    NEWLAYER = NEWLAYER + 1
    REDIM _PRESERVE layers(1 TO NEWLAYER) AS LAYER
    EXIT DO
LOOP
IF x * y = FALSE THEN
    x = SCRX
    y = SCRY
END IF
layers(NEWLAYER).inuse = -1
layers(NEWLAYER).layer = _NEWIMAGE(x, y, 32)
IF mother THEN 
    IF VALIDLAYER(mother) THEN PCOPY layers(mother).layer, layers(NEWLAYER).layer
END IF
layers(NEWLAYER).layerbak = FALSE
layers(NEWLAYER).status = status
layers(NEWLAYER).queue = NEWLAYER
layers(NEWLAYER).x = 0
layers(NEWLAYER).y = 0
layers(NEWLAYER).trans = LAYER_NOTRANSPARENCY
layers(NEWLAYER).fade = FALSE
layers(NEWLAYER).dest = 0
layers(NEWLAYER).timer = FALSE
END FUNCTION

FUNCTION VALIDLAYER` (handle AS INTEGER)
SHARED layers() AS LAYER
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(layers) THEN EXIT FUNCTION
IF layers(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDLAYER = TRUE
END FUNCTION