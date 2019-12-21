'Gorlock's QB64 Layer Library
'2013-08-17
'
'SUB       LAYERFREE          (handle%)                                  Frees the handle of a layer
'SUB       LAYERHIDE          (handle%)                                  Hides a layer that had been shown
'SUB       LAYERPUT           (x&, y&, handle%, dest&)                   Puts the layer image to the screen
'SUB       LAYERSHOW          (handle%)                                  Shows a layer that had been hidden
'FUNCTION  LAYERDESCRIPTION$  (handle%)                                  Returns the description of a layer
'FUNCTION  LAYERSTATUS%%      (handle%)                                  Returns the status of a layer
'FUNCTION  NEWLAYER%          (x&, y&, description$, mother%, status%%)  Creates a new layer
'FUNCTION  VALIDLAYER`        (handle%)                                  Checks to see if a layer handle is valid or not
'
'Requires a 32bit color mode environment

SUB LAYERFREE (handle AS INTEGER)
SHARED layers() AS layer
SELECT CASE handle
    CASE LAYER_ALLLAYERS
        FOR handle = UBOUND(layers) TO 1 STEP -1
            IF VALIDLAYER(handle) THEN
                IF layers(handle).layer THEN _FREEIMAGE (layers(handle).layer)
                IF handle = UBOUND(layers) THEN
                    REDIM _PRESERVE layers(UBOUND(layers) - 1) AS layer
                ELSE
                    layers(handle).inuse = 0
                END IF
            END IF
        NEXT handle
    CASE ELSE
        IF NOT VALIDLAYER(handle) THEN EXIT SUB
        IF layers(handle).layer THEN _FREEIMAGE (layers(handle).layer)
        IF handle = UBOUND(layers) THEN
            REDIM _PRESERVE layers(UBOUND(layers) - 1) AS layer
        ELSE
            layers(handle).inuse = 0
        END IF
END SELECT
END SUB

SUB LAYERHIDE (handle AS INTEGER)
SHARED layers() AS layer
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
SHARED layers() AS layer
IF NOT VALIDLAYER(handle) THEN EXIT SUB
IF dest THEN _DEST dest
_PUTIMAGE (x, y), layers(handle).layer
IF dest THEN _DEST 0
END SUB

SUB LAYERSHOW (handle AS INTEGER)
SHARED layers() AS layer
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

FUNCTION LAYERDESCRIPTION$ (handle AS INTEGER)
SHARED layers() AS layer
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERDESCRIPTION = layers(handle).description
END FUNCTION

FUNCTION LAYERSTATUS%% (handle AS INTEGER)
SHARED layers() AS layer
IF NOT VALIDLAYER(handle) THEN EXIT SUB
LAYERSTATUS = layers(handle).status
END FUNCTION

FUNCTION NEWLAYER% (x AS LONG, y AS LONG, description AS STRING, mother AS INTEGER, status AS _BYTE)
DIM findfreelayer AS INTEGER
SHARED layers() AS layer
IF status < LAYER_SHOW THEN status = LAYER_SHOW
IF status > LAYER_HIDE THEN status = LAYER_HIDE
DO
    FOR NEWLAYER = 1 TO UBOUND(layers)
        IF layers(NEWLAYER).inuse = 0 THEN EXIT DO
    NEXT NEWLAYER
    NEWLAYER = NEWLAYER + 1
    REDIM _PRESERVE layers(NEWLAYER) AS layer
    EXIT DO
LOOP
IF x * y = 0 THEN
    x = SCRX
    y = SCRY
END IF
layers(NEWLAYER).inuse = -1
layers(NEWLAYER).layer = _NEWIMAGE(x, y, 32)
IF mother THEN
    IF VALIDLAYER(mother) THEN PCOPY layers(mother).layer, layers(NEWLAYER).layer
END IF
layers(mother).status = status
layers(mother).description = description
END FUNCTION

FUNCTION VALIDLAYER` (handle AS INTEGER)
SHARED layers() AS layer
IF handle > UBOUND(layers) OR handle = 0 THEN EXIT FUNCTION
IF layers(handle).inuse = 0 THEN EXIT FUNCTION
VALIDLAYER = -1
END FUNCTION