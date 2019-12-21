CONST SPRITE_ANIMATE = -1 '          animate
CONST SPRITE_BACKFORTHLOOP = 2 '     animate an oscilating loop
CONST SPRITE_BACKWARDLOOP = 1 '      animate a backward loop
CONST SPRITE_FORWARDLOOP = 0 '       animate a forward loop
CONST SPRITE_NOANIMATE = 0 '         no animation
CONST SPRITE_BOXDETECT = 1 '         use rectangular detection
CONST SPRITE_NODETECT = 0 '          do not detect collisions
CONST SPRITE_PIXELDETECT = 2 '       use pixel accurate detection
CONST SPRITE_ALLSPRS = -1 '          check all sprites for collision
CONST SPRITE_BOTH = 3 '              flip both horiz/vertical
CONST SPRITE_HORIZONTAL = 1 '        flip sprite horizontal
CONST SPRITE_NONE = 0 '              no sprite flipping
CONST SPRITE_VERTICAL = 2 '          flip sprite vertical
CONST SPRITE_DONTMOVE = 0 '          disable automotion
CONST SPRITE_MOVE = -1 '             enable automotion
CONST SPRITE_MOUSEHOVER = 3 '        mouse hovering over sprite
CONST SPRITE_MOUSELEFT = 1 '         left button clicked on sprite
CONST SPRITE_MOUSERIGHT = 2 '        right button clicked on sprite
CONST SPRITE_NOMOUSE = 0 '           no current mouse interaction
CONST SPRITE_DONTSAVE = 0 '          don't save background image
CONST SPRITE_SAVE = -1 '             save background image
CONST SPRITE_NOVALUE = -32767 '      variables with no value assigned
CONST SPRITE_AUTOTRANSPARENCY = -2 ' automatically discover transparency
CONST SPRITE_NOTRANSPARENCY = -1 '   sheet has no transparency

TYPE SPRLIB_SHEET
    inuse AS INTEGER '        sheet is in use              (true / false)
    sheetimage AS DOUBLE '    image handle of sheet
    sheetwidth AS INTEGER '   width of sheet
    sheetheight AS INTEGER '  height of sheet
    spritewidth AS INTEGER '  width of each sprite
    spriteheight AS INTEGER ' height of each sprite
    transparent AS DOUBLE '   transparent color on sheet   (negative = none, 0 and greater = color)
    columns AS INTEGER '      number of sprite columns
END TYPE
TYPE SPRLIB_SPRITE
    inuse AS INTEGER '        sprite is in use             (true / false)
    sheet AS INTEGER '        what sheet is sprite on
    onscreen AS INTEGER '     sprite showing on screen     (true / false)
    visible AS INTEGER '      sprite hidden/showing        (true / false)
    currentwidth AS INTEGER ' current width of sprite      (width after zoom/rotate)
    currentheight AS INTEGER 'current height of sprite     (height after zoom/rotate)
    restore AS INTEGER '      sprite restores background   (true / false)
    image AS DOUBLE '         current image on screen      (use for pixel accurate detection)
    background AS DOUBLE '    sprite background image
    currentcell AS INTEGER '  current animation cell       (1 to cells)
    flip AS INTEGER '         flip vertical/horizonatal    (0 = none, 1 = horizontal, 2 = vertical, 3 = both)
    animation AS INTEGER '    automatic sprite animation   (true / false)
    animtype AS INTEGER '     automatic animation type     (0 = acsending loop, 1 = descending loop, 2 = forward/backward loop
    animdir AS INTEGER '      forward/backward loop dir    (1 = forward, -1 = backward)
    animstart AS INTEGER '    animation sequence start     (=> 1 to <= animend)
    animend AS INTEGER '      animation sequence end       (=> animstart to <= cells)
    transparent AS DOUBLE '   transparent color            (-1 = none, 0 and higher = color)
    zoom AS INTEGER '         zoom level in percentage     (1 to x%)
    rotation AS SINGLE '      rotation in degrees          (0 to 359.9999 degrees)
    motion AS INTEGER '       sprite auto motion           (true / false)
    speed AS SINGLE '         sprite auto motion speed     (any numeric value)
    direction AS SINGLE '     sprite auto motion angle     (0 to 359.9999 degrees)
    xdir AS SINGLE '          x vector for automotion
    ydir AS SINGLE '          y vector for automotion
    spindir AS SINGLE '       spin direction for automotion
    actualx AS SINGLE '       actual x location
    actualy AS SINGLE '       actual y location
    currentx AS INTEGER '     current x location on screen (INT(actualx))
    currenty AS INTEGER '     current y location on screen (INT(actualy))
    backx AS INTEGER '        x location of background image
    backy AS INTEGER '        y location of background image
    screenx1 AS INTEGER '     upper left x of sprite
    screeny1 AS INTEGER '     upper left y of sprite
    screenx2 AS INTEGER '     lower right x of sprite
    screeny2 AS INTEGER '     lower right y of sprite
    layer AS INTEGER '        layer the sprite resides on (1 to x, lower sprite layers drawn first)
    detect AS INTEGER '       collision detection          (true / false)
    detecttype AS INTEGER '   the type of detection use    (0 = do not detect collisions, 1 = box, 2 = pixel accurate)
    collx1 AS INTEGER '       upper left x collision area  (pixel accurate = x location of hit, box = upper left x)
    colly1 AS INTEGER '       upper left y collision area  (pixel accurate = y location of hit, box = upper left x)
    collx2 AS INTEGER '       lower right x collision area
    colly2 AS INTEGER '       lower right y collision area
    collsprite AS INTEGER '   sprite number colliding with (0 = none, 1 to x = sprite colliding with)
    pointer AS INTEGER '      mouse pointer interaction    (0 none, 1 left button, 2 right button, 3 hovering)
    mouseax AS INTEGER '      actual x location of pointer (x = 0 to screen width)
    mouseay AS INTEGER '      actual y location of pointer (y = 0 to screen height)
    mousecx AS INTEGER '      x location pointer on sprite (x = 0 to sprite width)
    mousecy AS INTEGER '      y location pointer on sprite (y = 0 to sprite height)
    score AS SINGLE '         sprite score value for games
END TYPE

REDIM sprlib_sprite(1) AS SPRLIB_SPRITE
REDIM sprlib_sheet(1) AS SPRLIB_SHEET