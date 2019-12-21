'QB64 Animation Library by Gorlock
'2013-08-11
'
'SUB       ANIMATIONFREE    (handle%)                            Frees the handle of an animation
'SUB       STARTANIMATION   (handle%)                            Starts or resumes the animation
'SUB       STOPANIMATION    (handle%)                            Pauses the animation
'SUB       UPDATEANIMATION  (handle%)                            Updates the display coordinations of the animated object
'FUNCTION  ANIMATIONSTATUS  (handle%)                            Returns the status of an animation
'FUNCTION  ANIMATIONTIME    (handle%)                            Returns the time the animation has been going on for
'FUNCTION  ANIMATIONX       (handle%)                            Returns the x coordinate of the animated object
'FUNCTION  ANIMATIONY       (handle%)                            Returns the y coordinate of the animated object
'FUNCTION  NEWANIMATION     (x1!, y1!, x2!, y2!, time!, auto%%)  Creates a new animation
'FUNCTION  VALIDANIMATION   (handle%)                            Checks whether a animation handle is valid or not

SUB ANIMATIONFREE (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT SUB
IF animations(handle).timer THEN TIMER(animations(handle).timer) FREE
IF handle = UBOUND(animations) THEN
    REDIM _PRESERVE animations(UBOUND(animations) - 1) AS animation
ELSE
    animations(handle).inuse = 0
END IF
END SUB

SUB STARTANIMATION (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT SUB
animations(handle).status = 2
IF animations(handle).timer THEN
    ON TIMER(animations(handle).timer, .025) UPDATEANIMATION handle
    TIMER(animations(handle).timer) ON
END IF
animations(handle).oldTIMER = TIMER(.001)
UPDATEANIMATION handle
END SUB

SUB STOPANIMATION (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT SUB
animations(handle).status = 3
TIMER(animations(handle).timer) OFF
END SUB

SUB UPDATEANIMATION (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT SUB
IF animations(handle).time >= animations(handle).speed THEN
    STOPANIMATION handle
    animations(handle).status = 4
    EXIT SUB
END IF
IF TIMER < animations(handle).oldTIMER THEN animations(handle).oldTIMER = 0
animations(handle).time = animations(handle).time + (TIMER - animations(handle).oldTIMER)
animations(handle).oldTIMER = TIMER
IF animations(handle).time > animations(handle).speed THEN animations(handle).time = animations(handle).speed
animations(handle).x = animations(handle).x1 + animations(handle).time / animations(handle).speed * (animations(handle).x2 - animations(handle).x1)
animations(handle).y = animations(handle).slope * (animations(handle).x - animations(handle).x1) + animations(handle).y1
END SUB

FUNCTION ANIMATIONSTATUS%% (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONSTATUS = animations(handle).status
END FUNCTION

FUNCTION ANIMATIONTIME! (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONTIME = animations(handle).time
END FUNCTION

FUNCTION ANIMATIONX! (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONX = animations(handle).x
END FUNCTION

FUNCTION ANIMATIONY! (handle AS INTEGER)
SHARED animations() AS animation
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONY = animations(handle).y
END FUNCTION

FUNCTION NEWANIMATION% (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE, speed AS SINGLE, auto AS _BYTE)
SHARED animations() AS animation
DO
    FOR NEWANIMATION = 1 TO UBOUND(animations)
        IF animations(NEWANIMATION).inuse = 0 THEN EXIT DO
    NEXT NEWANIMATION
    NEWANIMATION = NEWANIMATION + 1
    REDIM _PRESERVE animations(NEWANIMATION) AS animation
    EXIT DO
LOOP
animations(NEWANIMATION).inuse = -1
animations(NEWANIMATION).status = 1
animations(NEWANIMATION).x1 = x1
animations(NEWANIMATION).y1 = y1
animations(NEWANIMATION).x2 = x2
animations(NEWANIMATION).y2 = y2
animations(NEWANIMATION).speed = speed
animations(NEWANIMATION).slope = (y2 - y1) / (x2 - x1)
animations(NEWANIMATION).x = x1
animations(NEWANIMATION).y = y1
animations(NEWANIMATION).time = 0
IF auto = ANIMATION_AUTOUPDATE_ON THEN
    animations(NEWANIMATION).timer = _FREETIMER
ELSE
    animations(NEWANIMATION).timer = 0
END IF
END FUNCTION

FUNCTION VALIDANIMATION` (handle AS INTEGER)
SHARED animations() AS animation
IF handle > UBOUND(animations) OR handle = 0 THEN EXIT FUNCTION
IF animations(handle).inuse = 0 THEN EXIT FUNCTION
VALIDANIMATION = -1
END FUNCTION