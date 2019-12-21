'******************************************************************************
'
'Gorlock's QB64 Frame Library
'v0.12
'
'******************************************************************************
'
'Version 0.1  - 2013-10-04
'Version 0.11 - 2013-10-04
'  - added tutorial and init include file
'Version 0.12 - 2013-10-05
'  - added new frame image sets
'
'******************************************************************************
'
'SUB       FRAME      (instruction$, thickness%%, dest&)  Draws a frame
'SUB       FREEFRAME  ()                                  Frees frame image handles loaded with LOADFRAME
'SUB       LOADFRAME  (set%%, floc$)                      Loads a frame image set
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/Z20O8fdi
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'
'This library requires the frame image sets resource: https://db.tt/t7hs0Qli
'Place these image sets somewhere in your program folder and don't move them,
'  anytime you call LOADFRAME, you will need to give the frameloc$ parameter
'  which is an absolute or relative folder location of the resources. For more
'  information about this see the tutorial.
'Credits for these images goes to Under the Moon - Open Game Art
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/G1Zgwh2j
'
'******************************************************************************

SUB FRAME (instruction AS STRING, thickness AS _BYTE, dest AS LONG)
DIM foundterminator AS _BIT
DIM horizontalsector AS _BYTE
DIM verticalsector AS _BYTE
DIM drawframe AS _UNSIGNED _BYTE
DIM checkunits AS INTEGER
DIM drawsides AS INTEGER
DIM findright AS INTEGER
DIM findtop AS INTEGER
DIM instructionpos AS INTEGER
DIM instructions AS INTEGER
DIM scaninstructions AS INTEGER
DIM units AS INTEGER
DIM placesides AS LONG
DIM sidemiddle AS LONG
DIM tracex AS LONG
DIM tracey AS LONG
DIM instructiontype AS STRING * 1
DIM side AS RECT
DIM sockets(1 TO 1024, 1 TO 4) AS _BIT
DIM units(1 TO 1024) AS _BYTE
DIM instructions(1 TO 1024) AS STRING
DIM unitloc(1 TO 1024) AS RECT
SHARED frameimg() AS LONG
IF instruction = "" THEN EXIT SUB
IF thickness < FRAME_SINGLE THEN thickness = FRAME_SINGLE
IF thickness > FRAME_DOUBLE THEN thickness = FRAME_DOUBLE
instructionpos = 1
DO
    instructions = instructions + 1
    instructions(instructions) = MID$(instruction, instructionpos, INSTR(instructionpos, instruction, CHR$(32)) - instructionpos)
    IF instructions(instructions) = "" THEN instructions(instructions) = MID$(instruction, instructionpos)
    instructionpos = instructionpos + LEN(instructions(instructions)) + 1
LOOP UNTIL instructionpos >= LEN(instruction)
IF dest THEN _DEST dest
tracex = VAL(instructions(1))
tracey = VAL(instructions(2))
FOR scaninstructions = 3 TO instructions
    instructiontype$ = LEFT$(instructions(scaninstructions), 1)
    SELECT CASE instructiontype$
        CASE "C"
            SELECT CASE INT(VAL(MID$(instructions(scaninstructions), 2)))
                CASE 1
                    units = units + 1
                    units(units) = 31
                    unitloc(units).left = tracex - _WIDTH(frameimg(thickness, 3, 1)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 3, 1))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 3, 1))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 3, 1)
                CASE 2
                    units = units + 1
                    units(units) = 32
                    unitloc(units).left = tracex - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 3, 2))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 3, 2))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 3, 2)
                CASE 3
                    units = units + 1
                    units(units) = 33
                    unitloc(units).left = tracex - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - _HEIGHT(frameimg(thickness, 3, 3)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 3, 3))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 3, 3))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = TRUE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 3, 3)
                CASE 4
                    units = units + 1
                    units(units) = 34
                    unitloc(units).left = tracex - _WIDTH(frameimg(thickness, 3, 4)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - _HEIGHT(frameimg(thickness, 3, 4)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 3, 4))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 3, 4))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = TURE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 3, 4)
            END SELECT
        CASE "D"
            tracey = tracey + INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "L"
            tracex = tracex - INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "N"
            SELECT CASE INT(VAL(MID$(instructions(scaninstructions), 2)))
                CASE 1
                    units = units + 1
                    units(units) = 61
                    unitloc(units).left = tracex - FRAME_BORDER
                    unitloc(units).top = tracey - INT(_HEIGHT(frameimg(thickness, 6, 1)) / 2)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 6, 1))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 6, 1))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = TRUE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 6, 1)
                CASE 2
                    units = units + 1
                    units(units) = 62
                    unitloc(units).left = tracex - INT(_WIDTH(frameimg(thickness, 6, 2)) / 2)
                    unitloc(units).top = tracey - FRAME_BORDER
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 6, 2))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 6, 2))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 6, 2)
                CASE 3
                    units = units + 1
                    units(units) = 63
                    unitloc(units).left = tracex - _WIDTH(frameimg(thickness, 6, 3)) + FRAME_BORDER
                    unitloc(units).top = tracey - INT(_HEIGHT(frameimg(thickness, 6, 3)) / 2)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 6, 3))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 6, 3))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = TRUE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 6, 3)
                CASE 4
                    units = units + 1
                    units(units) = 64
                    unitloc(units).left = tracex - INT(_WIDTH(frameimg(thickness, 6, 4)) / 2)
                    unitloc(units).top = tracey - _HEIGHT(frameimg(thickness, 6, 4)) / 2 + FRAME_BORDER
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 6, 4))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 6, 4))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = TRUE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 6, 4)
            END SELECT
        CASE "P"
            tracex = INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "Q"
            tracey = INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "R"
            tracex = tracex + INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "T"
            SELECT CASE INT(VAL(MID$(instructions(scaninstructions), 2)))
                CASE 1
                    units = units + 1
                    units(units) = 51
                    unitloc(units).left = tracex - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - INT(_HEIGHT(frameimg(thickness, 5, 1)) / 2)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 5, 1))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 5, 1))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = TRUE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 5, 1)
                CASE 2
                    units = units + 1
                    units(units) = 52
                    unitloc(units).left = tracex - INT(_WIDTH(frameimg(thickness, 5, 2)) / 2)
                    unitloc(units).top = tracey - FRAME_BORDER - 2 * thickness + (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 5, 2))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 5, 2))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = TRUE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 5, 2)
                CASE 3
                    units = units + 1
                    units(units) = 53
                    unitloc(units).left = tracex - _WIDTH(frameimg(thickness, 5, 3)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).top = tracey - INT(_HEIGHT(frameimg(thickness, 5, 3)) / 2)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 5, 3))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 5, 3))
                    sockets(units, 1) = TRUE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 5, 3)
                CASE 4
                    units = units + 1
                    units(units) = 54
                    unitloc(units).left = tracex - INT(_WIDTH(frameimg(thickness, 5, 4)) / 2)
                    unitloc(units).top = tracey - _HEIGHT(frameimg(thickness, 5, 4)) + FRAME_BORDER + 2 * thickness - (thickness = FRAME_DOUBLE)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 5, 4))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 5, 4))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = TRUE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 5, 4)
            END SELECT
        CASE "U"
            tracey = tracey - INT(VAL(MID$(instructions(scaninstructions), 2)))
        CASE "X"
            SELECT CASE INT(VAL(MID$(instructions(scaninstructions), 2)))
                CASE 1
                    units = units + 1
                    units(units) = 41
                    unitloc(units).left = tracex - INT(_WIDTH(frameimg(thickness, 4, 1)) / 2)
                    unitloc(units).top = tracey - INT(_HEIGHT(frameimg(thickness, 4, 1)) / 2)
                    unitloc(units).right = unitloc(units).left + _WIDTH(frameimg(thickness, 4, 1))
                    unitloc(units).bottom = unitloc(units).top + _HEIGHT(frameimg(thickness, 4, 1))
                    sockets(units, 1) = FALSE
                    sockets(units, 2) = FALSE
                    sockets(units, 3) = FALSE
                    sockets(units, 4) = FALSE
                    _PUTIMAGE (unitloc(units).left, unitloc(units).top), frameimg(thickness, 4, 1)
            END SELECT
    END SELECT
NEXT scaninstructions
FOR drawsides = 1 TO units
    IF sockets(drawsides, 1) = FALSE THEN
        side.left = unitloc(drawsides).right
        side.bottom = unitloc(drawsides).bottom
        SELECT CASE units(drawsides)
            CASE 31, 32, 52
                side.bottom = unitloc(drawsides).top + _HEIGHT(frameimg(thickness, 2, 2))
            CASE 41, 51, 53
                side.bottom = side.bottom - 9
        END SELECT
        side.top = side.bottom - _HEIGHT(frameimg(thickness, 2, 2))
        side.right = 0
        foundterminator = 0
        FOR findright = side.left TO _WIDTH STEP 10
            sidemiddle = ((side.bottom - side.top) / 2) + side.top
            FOR checkunits = 1 TO units
                IF checkunits <> drawsides THEN
                    IF isonbox(findright, sidemiddle, unitloc(checkunits).left, unitloc(checkunits).top, unitloc(checkunits).right, unitloc(checkunits).bottom) AND sockets(checkunits, 3) = FALSE THEN
                        side.right = unitloc(checkunits).left
                        sockets(checkunits, 3) = TRUE
                        foundterminator = -1
                        EXIT FOR
                    END IF
                END IF
            NEXT checkunits
            IF side.right THEN EXIT FOR
        NEXT findright
        IF foundterminator THEN
            sockets(drawsides, 1) = TRUE
            horizontalsector = 4
            FOR checkunits = 1 TO units
                IF unitloc(checkunits).bottom > unitloc(drawsides).bottom THEN
                    horizontalsector = 2
                    EXIT FOR
                END IF
            NEXT checkunits
            IF side.right - side.left > _WIDTH(frameimg(thickness, 2, horizontalsector)) THEN
                FOR placesides = side.left TO side.right STEP _WIDTH(frameimg(thickness, 2, horizontalsector))
                    IF placesides + _WIDTH(frameimg(thickness, 2, horizontalsector)) <= side.right THEN _PUTIMAGE (placesides, side.top), frameimg(thickness, 2, horizontalsector)
                NEXT placesides
                IF placesides > side.right THEN CALL MAPRECT(frameimg(thickness, 2, horizontalsector), 0, 0, side.right - (placesides - _WIDTH(frameimg(thickness, 2, horizontalsector))), _HEIGHT(frameimg(thickness, 2, horizontalsector)), dest, placesides - _WIDTH(frameimg(thickness, 2, horizontalsector)), side.top, side.right, side.bottom)
            ELSE
                CALL MAPRECT(frameimg(thickness, 2, horizontalsector), 0, 0, side.right, _HEIGHT(frameimg(thickness, 2, horizontalsector)), dest, side.left, side.top, side.right, side.bottom)
            END IF
        END IF
    END IF
    IF sockets(drawsides, 2) = FALSE THEN
        side.bottom = unitloc(drawsides).top
        side.right = unitloc(drawsides).right
        SELECT CASE units(drawsides)
            CASE 33, 51
                side.right = unitloc(drawsides).left + _WIDTH(frameimg(thickness, 2, 1))
            CASE 41, 54
                side.right = side.right - 9
        END SELECT
        side.left = side.right - _WIDTH(frameimg(thickness, 2, 1))
        side.top = 0
        foundterminator = 0
        FOR findtop = side.bottom TO 0 STEP -10
            sidemiddle = ((side.right - side.left) / 2) + side.left
            FOR checkunits = 1 TO units
                IF checkunits <> drawsides THEN
                    IF isonbox(sidemiddle, findtop, unitloc(checkunits).left, unitloc(checkunits).top, unitloc(checkunits).right, unitloc(checkunits).bottom) AND sockets(checkunits, 4) = FALSE THEN
                        side.top = unitloc(checkunits).bottom
                        sockets(checkunits, 4) = TRUE
                        foundterminator = -1
                        EXIT FOR
                    END IF
                END IF
            NEXT checkunits
            IF side.top THEN EXIT FOR
        NEXT findtop
        IF foundterminator THEN
            sockets(drawsides, 2) = TRUE
            verticalsector = 3
            FOR checkunits = 1 TO units
                IF unitloc(checkunits).right > unitloc(drawsides).right THEN
                    verticalsector = 1
                    EXIT FOR
                END IF
            NEXT checkunits
            IF side.bottom - side.top > _HEIGHT(frameimg(thickness, 2, verticalsector)) THEN
                FOR placesides = side.top TO side.bottom STEP _HEIGHT(frameimg(thickness, 2, verticalsector))
                    IF placesides + _HEIGHT(frameimg(thickness, 2, verticalsector)) <= side.bottom THEN _PUTIMAGE (side.left, placesides), frameimg(thickness, 2, verticalsector)
                NEXT placesides
                IF placesides > side.bottom THEN CALL MAPRECT(frameimg(thickness, 2, verticalsector), 0, 0, _WIDTH(frameimg(thickness, 2, verticalsector)), side.bottom - (placesides - _HEIGHT(frameimg(thickness, 2, verticalsector))), dest, side.left, placesides - _HEIGHT(frameimg(thickness, 2, verticalsector)), side.right, side.bottom)
            ELSE
                CALL MAPRECT(frameimg(thickness, 2, verticalsector), 0, 0, _WIDTH(frameimg(thickness, 2, verticalsector)), side.bottom - side.top, dest, side.left, side.top, side.right, side.bottom)
            END IF
        END IF
    END IF
NEXT drawsides
IF dest THEN _DEST 0
END SUB

SUB FREEFRAME
DIM thickness AS _BYTE
DIM orientation AS _BYTE
DIM unit AS _BYTE
SHARED frameimg() AS LONG
FOR thickness = FRAME_SINGLE TO FRAME_DOUBLE
    FOR unit = 1 TO UBOUND(frameimg, 2)
        FOR orientation = 1 TO UBOUND(frameimg, 3)
            IF frameimg(thickness, unit, orientation) THEN _FREEIMAGE frameimg(thickness, unit, orientation)
NEXT orientation, unit, thickness
END SUB

SUB LOADFRAME (set AS _BYTE, floc AS STRING)
DIM setname AS STRING
SHARED frameimg() AS LONG
IF floc = "" THEN floc = Frameloc
IF floc = "" THEN EXIT SUB
SELECT CASE set
    CASE FRAME_SILVER
        setname = "silver"
    CASE FRAME_GOLD
        setname = "gold"
    CASE FRAME_BRONZE
        setname = "bronze"
    CASE FRAME_COPPER
        setname = "copper"
    CASE FRAME_LAVA
        setname = "lava"
    CASE FRAME_WOOD
        setname = "wood"
    CASE FRAME_MAHOGANY
        setname = "mahogany"
    CASE FRAME_RUST
        setname = "rust"
    CASE ELSE
        setname = "gold"
END SELECT
FREEFRAME
frameimg(1, 1, 1) = _LOADIMAGE(floc$ + "\frame_bar_" + setname + ".png")
frameimg(1, 2, 1) = _LOADIMAGE(floc$ + "\frame_side_left_" + setname + ".png")
frameimg(1, 2, 2) = _LOADIMAGE(floc$ + "\frame_side_top_" + setname + ".png")
frameimg(1, 2, 3) = _LOADIMAGE(floc$ + "\frame_side_right_" + setname + ".png")
frameimg(1, 2, 4) = _LOADIMAGE(floc$ + "\frame_side_bottom_" + setname + ".png")
frameimg(1, 3, 1) = _LOADIMAGE(floc$ + "\frame_corner_1_" + setname + ".png")
frameimg(1, 3, 2) = _LOADIMAGE(floc$ + "\frame_corner_2_" + setname + ".png")
frameimg(1, 3, 3) = _LOADIMAGE(floc$ + "\frame_corner_3_" + setname + ".png")
frameimg(1, 3, 4) = _LOADIMAGE(floc$ + "\frame_corner_4_" + setname + ".png")
frameimg(1, 4, 1) = _LOADIMAGE(floc$ + "\frame_cross_" + setname + ".png")
frameimg(1, 5, 1) = _LOADIMAGE(floc$ + "\frame_t_left_" + setname + ".png")
frameimg(1, 5, 2) = _LOADIMAGE(floc$ + "\frame_t_top_" + setname + ".png")
frameimg(1, 5, 3) = _LOADIMAGE(floc$ + "\frame_t_right_" + setname + ".png")
frameimg(1, 5, 4) = _LOADIMAGE(floc$ + "\frame_t_bottom_" + setname + ".png")
frameimg(1, 6, 1) = _LOADIMAGE(floc$ + "\frame_cap_left_" + setname + ".png")
frameimg(1, 6, 2) = _LOADIMAGE(floc$ + "\frame_cap_top_" + setname + ".png")
frameimg(1, 6, 3) = _LOADIMAGE(floc$ + "\frame_cap_right_" + setname + ".png")
frameimg(1, 6, 4) = _LOADIMAGE(floc$ + "\frame_cap_bottom_" + setname + ".png")
frameimg(1, 7, 1) = _LOADIMAGE(floc$ + "\frame_box_large_left_" + setname + ".png")
frameimg(1, 7, 2) = _LOADIMAGE(floc$ + "\frame_box_large_top_" + setname + ".png")
frameimg(1, 7, 3) = _LOADIMAGE(floc$ + "\frame_box_large_right_" + setname + ".png")
frameimg(1, 7, 4) = _LOADIMAGE(floc$ + "\frame_box_large_bottom_" + setname + ".png")
frameimg(1, 8, 1) = _LOADIMAGE(floc$ + "\frame_box_small_left_" + setname + ".png")
frameimg(1, 8, 2) = _LOADIMAGE(floc$ + "\frame_box_small_top_" + setname + ".png")
frameimg(1, 8, 3) = _LOADIMAGE(floc$ + "\frame_box_small_right_" + setname + ".png")
frameimg(1, 8, 4) = _LOADIMAGE(floc$ + "\frame_box_small_bottom_" + setname + ".png")
frameimg(2, 1, 1) = _LOADIMAGE(floc$ + "\frame_bar_double_" + setname + ".png")
frameimg(2, 2, 1) = _LOADIMAGE(floc$ + "\frame_side_left_double_" + setname + ".png")
frameimg(2, 2, 2) = _LOADIMAGE(floc$ + "\frame_side_top_double_" + setname + ".png")
frameimg(2, 2, 3) = _LOADIMAGE(floc$ + "\frame_side_right_double_" + setname + ".png")
frameimg(2, 2, 4) = _LOADIMAGE(floc$ + "\frame_side_bottom_double_" + setname + ".png")
frameimg(2, 3, 1) = _LOADIMAGE(floc$ + "\frame_corner_1_double_" + setname + ".png")
frameimg(2, 3, 2) = _LOADIMAGE(floc$ + "\frame_corner_2_double_" + setname + ".png")
frameimg(2, 3, 3) = _LOADIMAGE(floc$ + "\frame_corner_3_double_" + setname + ".png")
frameimg(2, 3, 4) = _LOADIMAGE(floc$ + "\frame_corner_4_double_" + setname + ".png")
frameimg(2, 4, 1) = _LOADIMAGE(floc$ + "\frame_cross_double_" + setname + ".png")
frameimg(2, 5, 1) = _LOADIMAGE(floc$ + "\frame_t_left_double_" + setname + ".png")
frameimg(2, 5, 2) = _LOADIMAGE(floc$ + "\frame_t_top_double_" + setname + ".png")
frameimg(2, 5, 3) = _LOADIMAGE(floc$ + "\frame_t_right_double_" + setname + ".png")
frameimg(2, 5, 4) = _LOADIMAGE(floc$ + "\frame_t_bottom_double_" + setname + ".png")
frameimg(2, 6, 1) = _LOADIMAGE(floc$ + "\frame_cap_left_double_" + setname + ".png")
frameimg(2, 6, 2) = _LOADIMAGE(floc$ + "\frame_cap_top_double_" + setname + ".png")
frameimg(2, 6, 3) = _LOADIMAGE(floc$ + "\frame_cap_right_double_" + setname + ".png")
frameimg(2, 6, 4) = _LOADIMAGE(floc$ + "\frame_cap_bottom_double_" + setname + ".png")
END SUB