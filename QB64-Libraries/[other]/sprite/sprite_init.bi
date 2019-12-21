'declare sprite sub/func
DECLARE SUB SPRITEANIMATESET (handle AS INTEGER, startcell AS INTEGER, endcell AS INTEGER)
DECLARE SUB SPRITEANIMATION (handle AS INTEGER, onoff AS INTEGER, behavior AS INTEGER)
DECLARE SUB SPRITEANIMATIONCELLSET (handle AS INTEGER, cell AS INTEGER)
DECLARE SUB SPRITECOLLIDETYPE (handle AS INTEGER, behavior AS INTEGER)
DECLARE SUB SPRITEDIRECTIONSET (handle AS INTEGER, direction AS SINGLE)
DECLARE SUB SPRITEFLIP (handle AS INTEGER, behavior AS INTEGER)
DECLARE SUB SPRITEFREE (handle AS INTEGER)
DECLARE SUB SPRITEHIDE (handle AS INTEGER)
DECLARE SUB SPRITEMOTION (handle AS INTEGER, behavior AS INTEGER)
DECLARE SUB SPRITENEXT (handle AS INTEGER)
DECLARE SUB SPRITEPLACE (x AS SINGLE, y AS SINGLE, handle AS INTEGER)
DECLARE SUB SPRITEPREVIOUS (handle AS INTEGER)
DECLARE SUB SPRITEPUT (x AS SINGLE, y AS SINGLE, handle AS INTEGER)
DECLARE SUB SPRITESTRETCH (x AS SINGLE, y AS SINGLE, x2 AS SINGLE, y2 AS SINGLE, handle AS INTEGER)
DECLARE SUB SPRITEREVERSEX (handle AS INTEGER)
DECLARE SUB SPRITEREVERSEY (handle AS INTEGER)
DECLARE SUB SPRITEROTATE (handle AS INTEGER, degrees AS SINGLE)
DECLARE SUB SPRITESCORESET (handle AS INTEGER, value AS SINGLE)
DECLARE SUB SPRITESET (handle AS INTEGER, cell AS INTEGER)
DECLARE SUB SPRITESHOW (handle AS INTEGER)
DECLARE SUB SPRITESPEEDSET (handle AS INTEGER, speed AS SINGLE)
DECLARE SUB SPRITESPINSET (handle AS INTEGER, spin AS SINGLE)
DECLARE SUB SPRITESTAMP (x AS INTEGER, y AS INTEGER, handle AS INTEGER)
DECLARE SUB SPRITETRAVEL (handle AS INTEGER, direction AS SINGLE, speed AS SINGLE)
DECLARE SUB SPRITEZOOM (handle AS INTEGER, zoom AS INTEGER)
DECLARE FUNCTION SPRITECELLS (handle AS INTEGER)
DECLARE FUNCTION SPRITEANGLE (handle AS INTEGER, handle2 AS INTEGER)
DECLARE FUNCTION SPRITEANIMATIONCELL (handle AS INTEGER)
DECLARE FUNCTION SPRITEAX (handle AS INTEGER)
DECLARE FUNCTION SPRITEAY (handle AS INTEGER)
DECLARE FUNCTION SPRITECOLLIDE (handle AS INTEGER, handle2 AS INTEGER)
DECLARE FUNCTION SPRITECOLLIDEWITH (handle AS INTEGER)
DECLARE FUNCTION SPRITECOPY (handle AS INTEGER)
DECLARE FUNCTION SPRITECURRENTHEIGHT (handle AS INTEGER)
DECLARE FUNCTION SPRITECURRENTWIDTH (handle AS INTEGER)
DECLARE FUNCTION SPRITEDIRECTION (handle AS INTEGER)
DECLARE FUNCTION SPRITEFILEEXISTS (filename AS STRING)
DECLARE FUNCTION SPRITEMOUSE (handle AS INTEGER)
DECLARE FUNCTION SPRITEMOUSEAX (handle AS INTEGER)
DECLARE FUNCTION SPRITEMOUSEAY (handle AS INTEGER)
DECLARE FUNCTION SPRITEMOUSEX (handle AS INTEGER)
DECLARE FUNCTION SPRITEMOUSEY (handle AS INTEGER)
DECLARE FUNCTION SPRITENEW (sheet AS INTEGER, cell AS INTEGER, behavior AS INTEGER)
DECLARE FUNCTION SPRITEROTATION (handle AS INTEGER)
DECLARE FUNCTION SPRITESCORE (handle AS INTEGER)
DECLARE FUNCTION SPRITESHEETLOAD (filename AS STRING, spritewidth AS INTEGER, spriteheight AS INTEGER, transparent AS LONG)
DECLARE FUNCTION SPRITESHOWING (handle AS INTEGER)
DECLARE FUNCTION SPRITEX (handle AS INTEGER)
DECLARE FUNCTION SPRITEX1 (handle AS INTEGER)
DECLARE FUNCTION SPRITEX2 (handle AS INTEGER)
DECLARE FUNCTION SPRITEY (handle AS INTEGER)
DECLARE FUNCTION SPRITEY1 (handle AS INTEGER)
DECLARE FUNCTION SPRITEY2 (handle AS INTEGER)
DECLARE FUNCTION SPRITEZOOMLEVEL (handle AS INTEGER)

'sprite type declarations
TYPE SHEET
    inuse AS INTEGER '         sheet is in use              (true / false)
    sheetimage AS DOUBLE '     image handle of sheet
    sheetwidth AS INTEGER '    width of sheet
    sheetheight AS INTEGER '   height of sheet
    spritewidth AS INTEGER '   width of each sprite
    spriteheight AS INTEGER '  height of each sprite
    transparent AS DOUBLE '    transparent color on sheet   (negative = none, 0 and greater = color)
    columns AS INTEGER '       number of sprite columns
END TYPE
TYPE SPRITE
    inuse AS INTEGER '         sprite is in use             (true / false)
    sheet AS INTEGER '         what sheet is sprite on
    onscreen AS INTEGER '      sprite showing on screen     (true / false)
    visible AS INTEGER '       sprite hidden/showing        (true / false)
    currentwidth AS INTEGER '  current width of sprite      (width after zoom/rotate)
    currentheight AS INTEGER ' current height of sprite     (height after zoom/rotate)
    restore AS INTEGER '       sprite restores background   (true / false)
    image AS DOUBLE '          current image on screen      (use for pixel accurate detection)
    background AS DOUBLE '     sprite background image
    currentcell AS INTEGER '   current animation cell       (1 to cells)
    flip AS INTEGER '          flip vertical/horizonatal    (0 = none, 1 = horizontal, 2 = vertical, 3 = both)
    animation AS INTEGER '     automatic sprite animation   (true / false)
    animtype AS INTEGER '      automatic animation type     (0 = acsending loop, 1 = descending loop, 2 = forward/backward loop
    animdir AS INTEGER '       forward/backward loop dir    (1 = forward, -1 = backward)
    animstart AS INTEGER '     animation sequence start     (=> 1 to <= animend)
    animend AS INTEGER '       animation sequence end       (=> animstart to <= cells)
    transparent AS DOUBLE '    transparent color            (-1 = none, 0 and higher = color)
    zoom AS INTEGER '          zoom level in percentage     (1 to x%)
    rotation AS SINGLE '       rotation in degrees          (0 to 359.9999 degrees)
    motion AS INTEGER '        sprite auto motion           (true / false)
    speed AS SINGLE '          sprite auto motion speed     (any numeric value)
    direction AS SINGLE '      sprite auto motion angle     (0 to 359.9999 degrees)
    xdir AS SINGLE '           x vector for automotion
    ydir AS SINGLE '           y vector for automotion
    spindir AS SINGLE '        spin direction for automotion
    actualx AS SINGLE '        actual x location
    actualy AS SINGLE '        actual y location
    currentx AS INTEGER '      current x location on screen (INT(actualx))
    currenty AS INTEGER '      current y location on screen (INT(actualy))
    backx AS INTEGER '         x location of background image
    backy AS INTEGER '         y location of background image
    screenx1 AS INTEGER '      upper left x of sprite
    screeny1 AS INTEGER '      upper left y of sprite
    screenx2 AS INTEGER '      lower right x of sprite
    screeny2 AS INTEGER '      lower right y of sprite
    layer AS INTEGER '         layer the sprite resides on (1 to x, lower sprite layers drawn first)
    detect AS INTEGER '        collision detection          (true / false)
    detecttype AS INTEGER '    the type of detection use    (0 = do not detect collisions, 1 = box, 2 = pixel accurate)
    collx1 AS INTEGER '        upper left x collision area  (pixel accurate = x location of hit, box = upper left x)
    colly1 AS INTEGER '        upper left y collision area  (pixel accurate = y location of hit, box = upper left x)
    collx2 AS INTEGER '        lower right x collision area
    colly2 AS INTEGER '        lower right y collision area
    collsprite AS INTEGER '    sprite number colliding with (0 = none, 1 to x = sprite colliding with)
    pointer AS INTEGER '       mouse pointer interaction    (0 none, 1 left button, 2 right button, 3 hovering)
    mouseax AS INTEGER '       actual x location of pointer (x = 0 to screen width)
    mouseay AS INTEGER '       actual y location of pointer (y = 0 to screen height)
    mousecx AS INTEGER '       x location pointer on sprite (x = 0 to sprite width)
    mousecy AS INTEGER '       y location pointer on sprite (y = 0 to sprite height)
    score AS SINGLE '          sprite score value for games
END TYPE

'sprite constant definitions
CONST SPRITE_ALLSHEETS = TRUE '      all sheets
CONST SPRITE_ALLSPRITES = TRUE '     allsprites
CONST SPRITE_ALLSPRS = TRUE '        check all sprites for collision
CONST SPRITE_ANIMATE = TRUE '        animate
CONST SPRITE_AUTOTRANSPARENCY = -2 ' automatically discover transparency
CONST SPRITE_BACKFORTHLOOP = 2 '     animate an oscilating loop
CONST SPRITE_BACKWARDLOOP = 1 '      animate a backward loop
CONST SPRITE_BOTH = 3 '              flip both horiz/vertical
CONST SPRITE_BOXDETECT = 1 '         use rectangular detection
CONST SPRITE_DONTMOVE = FALSE '      disable automotion
CONST SPRITE_DONTSAVE = FALSE '      don't save background image
CONST SPRITE_FORWARDLOOP = 0 '       animate a forward loop
CONST SPRITE_HORIZONTAL = 1 '        flip sprite horizontal
CONST SPRITE_MOUSEHOVER = 3 '        mouse hovering over sprite
CONST SPRITE_MOUSELEFT = 1 '         left button clicked on sprite
CONST SPRITE_MOUSERIGHT = 2 '        right button clicked on sprite
CONST SPRITE_MOVE = TRUE '           enable automotion
CONST SPRITE_NOANIMATE = FALSE '     no animation
CONST SPRITE_NODETECT = FALSE '      do not detect collisions
CONST SPRITE_NOMOUSE = FALSE '       no current mouse interaction
CONST SPRITE_NONE = FALSE '          no sprite flipping
CONST SPRITE_NOTRANSPARENCY = -1 '   sheet has no transparency
CONST SPRITE_NOVALUE = -32767 '      variables with no value assigned
CONST SPRITE_PIXELDETECT = 2 '       use pixel accurate detection
CONST SPRITE_SAVE = TRUE '           save background image
CONST SPRITE_VERTICAL = 2 '          flip sprite vertical

'dimension shared arrays
REDIM sheets(0 TO 1) AS SHEET
REDIM sprites(0 TO 1) AS SPRITE