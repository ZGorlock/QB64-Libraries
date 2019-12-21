'******************************************************************************
'
'QB64 Animation Library by Gorlock
'v0.21
'
'******************************************************************************
'
'Version 0.1  - 2013-08-11
'Version 0.11 - 2013-10-04
'  - added tutorial and init include file
'Version 0.2  - 2013-11-27
'  - added autoterminate abilities
'Version 0.21 - 2013-12-01
'  - added bounce and equation behaviours
'
'******************************************************************************
'
'SUB       ANIMATIONFREE    (handle%)                                                                 Frees the handle of an animation
'SUB       STARTANIMATION   (handle%)                                                                 Starts or resumes the animation
'SUB       STOPANIMATION    (handle%)                                                                 Pauses the animation
'SUB       UPDATEANIMATION  (handle%)                                                                 Updates the display coordinations of the animated object
'FUNCTION  ANIMATIONSTATUS  (handle%)                                                                 Returns the status of an animation
'FUNCTION  ANIMATIONTIME    (handle%)                                                                 Returns the time the animation has been going on for
'FUNCTION  ANIMATIONX       (handle%)                                                                 Returns the x coordinate of the animated object
'FUNCTION  ANIMATIONY       (handle%)                                                                 Returns the y coordinate of the animated object
'FUNCTION  NEWANIMATION     (x1!, y1!, x2!, y2!, time!, equation$, behaviour%%, auto%%, terminate%%)  Creates a new animation
'FUNCTION  VALIDANIMATION   (handle%)                                                                 Checks whether a animation handle is valid or not
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/50u1t8CB
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'This library requires SMcNeill's Math Evalutor: https://db.tt/ZiLVqdP5
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/GB5001Ji
'
'******************************************************************************

SUB ANIMATIONFREE (handle AS INTEGER)
SHARED animations() AS ANIMATION
SELECT CASE handle
    CASE ANIMATION_ALLANIMATIONS
        FOR handle = UBOUND(animations) TO 1 STEP -1
            ANIMATIONFREE handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDANIMATION(handle) THEN EXIT SUB
        IF animations(handle).timer THEN TIMER(animations(handle).timer) FREE
        IF handle = UBOUND(animations) AND handle > 1 THEN
            REDIM _PRESERVE animations(1 TO handle - 1) AS ANIMATION
        ELSE
            animations(handle).inuse = FALSE
        END IF
END SELECT
END SUB

SUB STARTANIMATION (handle AS INTEGER)
SHARED animations() AS ANIMATION
SELECT CASE handle
    CASE ANIMATION_ALLANIMATIONS
        FOR handle = UBOUND(animations) TO 1 STEP -1
            STARTANIMATION handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDANIMATION(handle) THEN EXIT SUB
        animations(handle).status = ANIMATION_STARTED
        IF animations(handle).timer THEN
            ON TIMER(animations(handle).timer, .025) UPDATEANIMATION handle
            TIMER(animations(handle).timer) ON
        END IF
        animations(handle).oldTIMER = TIMER
        UPDATEANIMATION handle
END SELECT
END SUB

SUB STOPANIMATION (handle AS INTEGER)
SHARED animations() AS ANIMATION
SELECT CASE handle
    CASE ANIMATION_ALLANIMATIONS
        FOR handle = UBOUND(animations) TO 1 STEP -1
            STOPANIMATION handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDANIMATION(handle) THEN EXIT SUB
        animations(handle).status = ANIMATION_PAUSED
        TIMER(animations(handle).timer) OFF
END SELECT
END SUB

SUB UPDATEANIMATION (handle AS INTEGER)
DIM lx AS INTEGER
DIM ly AS INTEGER
DIM equ AS STRING
SHARED animations() AS ANIMATION
SELECT CASE handle
    CASE ANIMATION_ALLANIMATIONS
        FOR handle = UBOUND(animations) TO 1 STEP -1
            UPDATEANIMATION handle
        NEXT handle
    CASE ELSE
        IF NOT VALIDANIMATION(handle) THEN EXIT SUB
        IF animations(handle).time >= animations(handle).speed THEN
            STOPANIMATION handle
            SELECT CASE animations(handle).behaviour
                CASE ANIMATION_BASIC, ANIMATION_EQUATION
                    animations(handle).status = ANIMATION_FINISHED
                    IF animations(handle).terminate = ANIMATION_AUTOTERMINATE_ON THEN ANIMATIONFREE handle
                CASE ANIMATION_BOUNCE
                    SWAP animations(handle).x1, animations(handle).x2
                    SWAP animations(handle).y1, animations(handle).y2
                    animations(handle).slope = (animations(handle).y2 - animations(handle).y1) / (animations(handle).x2 - animations(handle).x1)
                    animations(handle).time = 0
                    STARTANIMATION handle
            END SELECT
            EXIT SUB
        END IF
        IF animations(handle).status = ANIMATION_PAUSED THEN EXIT SUB
        IF TIMER < animations(handle).oldTIMER THEN animations(handle).oldTIMER = 0
        animations(handle).time = animations(handle).time + (TIMER - animations(handle).oldTIMER)
        animations(handle).oldTIMER = TIMER
        IF animations(handle).time > animations(handle).speed THEN animations(handle).time = animations(handle).speed
        SELECT CASE animations(handle).behaviour
            CASE ANIMATION_BASIC, ANIMATION_BOUNCE
                animations(handle).x = animations(handle).x1 + animations(handle).time / animations(handle).speed * (animations(handle).x2 - animations(handle).x1)
                animations(handle).y = animations(handle).slope * (animations(handle).x - animations(handle).x1) + animations(handle).y1
            CASE ANIMATION_EQUATION
                SELECT CASE animations(handle).fof
                    CASE ANIMATION_FX
                        animations(handle).x = animations(handle).x1 + animations(handle).time / animations(handle).speed * (animations(handle).x2 - animations(handle).x1)
                        equ = animations(handle).equation
                        DO
                            lx = INSTR(equ, "X")
                            IF lx = 0 THEN EXIT DO
                            equ = LEFT$(equ, lx - 1) + LTRIM$(RTRIM$(STR$(animations(handle).x))) + MID$(equ, lx + 1)
                        LOOP
                        animations(handle).y = VAL(Evaluate_Expression(equ))
                    CASE ANIMATION_FY
                        animations(handle).y = animations(handle).slope * (animations(handle).x - animations(handle).x1) + animations(handle).y1
                        equ = animations(handle).equation
                        DO
                            ly = INSTR(equ, "Y")
                            IF ly = 0 THEN EXIT DO
                            equ = LEFT$(equ, ly - 1) + LTRIM$(RTRIM$(STR$(animations(handle).y))) + MID$(equ, ly + 1)
                        LOOP
                        animations(handle).x = VAL(Evaluate_Expression(equ))
                END SELECT
       END SELECT
END SELECT
END SUB

FUNCTION ANIMATIONSTATUS%% (handle AS INTEGER)
SHARED animations() AS ANIMATION
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONSTATUS = animations(handle).status
END FUNCTION

FUNCTION ANIMATIONTIME! (handle AS INTEGER)
SHARED animations() AS ANIMATION
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONTIME = animations(handle).time
END FUNCTION

FUNCTION ANIMATIONX! (handle AS INTEGER)
SHARED animations() AS ANIMATION
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONX = animations(handle).x
END FUNCTION

FUNCTION ANIMATIONY! (handle AS INTEGER)
SHARED animations() AS ANIMATION
IF NOT VALIDANIMATION(handle) THEN EXIT FUNCTION
ANIMATIONY = animations(handle).y
END FUNCTION

FUNCTION NEWANIMATION% (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE, speed AS SINGLE, equation AS STRING, behaviour AS _BYTE, auto AS _BYTE, terminate AS _BYTE)
SHARED animations() AS ANIMATION
IF behaviour < ANIMATION_BASIC THEN behaviour = ANIMATION_BASIC
IF behaviour > ANIMATION_EQUATION THEN behaviour = ANIMATION_EQUATION
IF auto < ANIMATION_AUTOUPDATE_ON THEN auto = ANIMATION_AUTOUPDATE_ON
IF auto > ANIMATION_AUTOUPDATE_OFF THEN auto = ANIMATION_AUTOUPDATE_OFF
IF terminate < ANIMATION_AUTOTERMINATE_ON THEN terminate = ANIMATION_AUTOTERMINATE_ON
IF terminate > ANIMATION_AUTOTERMINATE_OFF THEN terminate = ANIMATION_AUTOTERMINATE_OFF
equation = UCASE$(equation)
DO
    FOR NEWANIMATION = 1 TO UBOUND(animations)
        IF animations(NEWANIMATION).inuse = 0 THEN EXIT DO
    NEXT NEWANIMATION
    NEWANIMATION = NEWANIMATION + 1
    REDIM _PRESERVE animations(1 TO NEWANIMATION) AS ANIMATION
    EXIT DO
LOOP
animations(NEWANIMATION).inuse = TRUE
animations(NEWANIMATION).status = 1
animations(NEWANIMATION).x1 = x1
animations(NEWANIMATION).y1 = y1
animations(NEWANIMATION).x2 = x2
animations(NEWANIMATION).y2 = y2
animations(NEWANIMATION).speed = speed
animations(NEWANIMATION).equation = equation
IF INSTR(equation, "X") THEN
    animations(NEWANIMATION).fof = ANIMATION_FX
ELSE IF INSTR(equation, "Y") THEN
         animations(NEWANIMATION).fof = ANIMATION_FY
    END IF
END IF
animations(NEWANIMATION).behaviour = behaviour
animations(NEWANIMATION).slope = (y2 - y1) / (x2 - x1)
animations(NEWANIMATION).x = x1
animations(NEWANIMATION).y = y1
animations(NEWANIMATION).time = 0
animations(NEWANIMATION).terminate = terminate
IF auto = ANIMATION_AUTOUPDATE_ON THEN
    animations(NEWANIMATION).timer = _FREETIMER
ELSE
    animations(NEWANIMATION).timer = FALSE
END IF
END FUNCTION

FUNCTION VALIDANIMATION` (handle AS INTEGER)
SHARED animations() AS ANIMATION
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(animations) THEN EXIT FUNCTION
IF animations(handle).inuse = FALSE THEN EXIT FUNCTION
VALIDANIMATION = TRUE
END FUNCTION