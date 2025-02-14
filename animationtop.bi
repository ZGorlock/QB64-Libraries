DECLARE SUB ANIMATIONFREE (handle AS INTEGER)
DECLARE SUB STARTANIMATION (handle AS INTEGER)
DECLARE SUB STOPANIMATION (handle AS INTEGER)
DECLARE SUB UPDATEANIMATION (handle AS INTEGER)

DECLARE FUNCTION ANIMATIONSTATUS%% (handle AS INTEGER)
DECLARE FUNCTION ANIMATIONTIME! (handle AS INTEGER)
DECLARE FUNCTION ANIMATIONX! (handle AS INTEGER)
DECLARE FUNCTION ANIMATIONY! (handle AS INTEGER)
DECLARE FUNCTION NEWANIMATION% (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE, time AS SINGLE)
DECLARE FUNCTION VALIDANIMATION` (handle AS INTEGER)

TYPE animation
    inuse AS _BYTE
    status AS _BYTE
    x1 AS SINGLE
    y1 AS SINGLE
    x2 AS SINGLE
    y2 AS SINGLE
    speed AS SINGLE
    slope AS SINGLE
    x AS LONG
    y AS LONG
    time AS SINGLE
    timer AS LONG
    oldTIMER AS _FLOAT
END TYPE

CONST ANIMATION_UNCREATED = 0
CONST ANIMATION_CREATED = 1
CONST ANIMATION_STARTED = 2
CONST ANIMATION_PAUSED = 3
CONST ANIMATION_FINISHED = 4

REDIM animation(0) AS animation