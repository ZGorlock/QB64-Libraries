'******************************************************************************
'
'Terry Ritchie's QB64 Sprite Library
'Version 0.22
'*Modified by Gorlock*
'
'******************************************************************************
'
'Version A.1  - 07-18-2011
'Version A.11 - 08-01-2011
'  - minor bug fixes
'Version 0.2  - 10-27-2012
'  - updated error handling routines
'  - added SPRITEERROR function to handle errors reported by subs/functions
'  - removed SPRITEFILEEXISTS function and replaced with QB64 _FILEEXISTS command
'Version 0.21 - 10-19-2013
'  - updated SPRITEERROR
'  - added SPRITEANIMATIONCELL, SPRITEANIMATIONCELLSET, SPRITECELLS, and SHEETFREE
'Version 0.21 - 12-28-13
'  - added SPRITEPLACE
'
'******************************************************************************
'
'SUB       SPRITEANIMATESET        (handle%, startcell%, endcell%)                         Sets a sprite's animation sequence start and end sprite sheet cells.
'SUB       SPRITEANIMATION         (handle%, onoff%, behavior%)                            Turns on or off automatic sprite animation with specified behavior.
'SUB       SPRITEANIMATIONCELLSET  (handle%, cell%)                                        Sets the cell of a sprite's animation cycle
'SUB       SPRITECOLLIDETYPE       (handle%, behavior%)                                    Sets the type of collision detection used for a sprite.
'SUB       SPRITEDIRECTIONSET      (handle%, direction!)                                   Sets a sprite's automotion direction angle.
'SUB       SPRITEFLIP              (handle%, behavior%)                                    Flips a sprite horizontaly, verticaly, both or resets to no flipping.
'SUB       SPRITEFREE              (handle%)                                               Removes a sprite from memory, freeing its resources.
'SUB       SPRITEHIDE              (handle%)                                               Hides a sprite from view.
'SUB       SPRITEMOTION            (handle%, behavior%)                                    Enables or disables a sprite's automotion feature.
'SUB       SPRITENEXT              (handle%)                                               Go to next cell of sprite's animation sequence.
'SUB       SPRITEPLACE             (x!, y!, handle%)                                       Places a sprite on screen with center at INT(x!), INT(y!).
'SUB       SPRITEPREVIOUS          (handle%)                                               Go to previous cell of sprite's animation sequence.
'SUB       SPRITEPUT               (x!, y!, handle%)                                       Places a sprite on screen at coordinates INT(x!), INT(y!).
'SUB       SPRITEREVERSEX          (handle%)                                               Reverses the x vector automotion value of a sprite.
'SUB       SPRITEREVERSEY          (handle%)                                               Reverses the y vector automotion value of a sprite.
'SUB       SPRITEROTATE            (handle%, degrees!)                                     Rotates a sprite from 0 to 360 degrees.
'SUB       SPRITESCORESET          (handle%, value!)                                       Sets the score value of a sprite.
'SUB       SPRITESET               (handle%, cell%)                                        Sets a sprite's image to a new image number on sprite sheet.
'SUB       SPRITESHOW              (handle%)                                               Unhides a sprite from view.
'SUB       SPRITESPEEDSET          (handle%, speed!)                                       Sets a sprite's automotion speed in pixels.
'SUB       SPRITESPINSET           (handle%, spin!)                                        Sets a sprite's automotion spin direction.
'SUB       SPRITESTAMP             (x%, y%, handle%)                                       Places a sprite on the background as if using a sprite stamp pad.
'SUB       SPRITESTRETCH           (x!, y!, x2!, y2!, handle%)                             Places a sprite stretched between the parameter values
'SUB       SPRITETRAVEL            (handle%, direction!, speed!)                           Moves a sprite in the direction and speed indicated.
'SUB       SPRITEZOOM              (handle%, zoom%)                                        Change the size (zoom level) of a sprite.
'FUNCTION  SPRITEANGLE             (handle%, handle2%)                                     Retrieves the angle in degrees between two sprites.
'FUNCTION  SPRITEANIMATIONCELL     (handle%)                                               Returns the current cell of a sprite's animation cycle
'FUNCTION  SPRITEAX                (handle%)                                               Returns the actual x location of a sprite. (single) centered
'FUNCTION  SPRITEAY                (handle%)                                               Returns the actual y location of a sprite. (single) centered
'FUNCTION  SPRITECELLS             (handle%)                                               Returns the number of cells in a sprite's animation cycle
'FUNCTION  SPRITECOLLIDE           (handle%, handle2%)                                     Returns the status of collisions with other sprites.
'FUNCTION  SPRITECOLLIDEWITH       (handle%)                                               Returns the sprite number that collided with the specified sprite.
'FUNCTION  SPRITECOPY              (handle%)                                               Makes a copy of a sprite and returns the newly created sprite's handle.
'FUNCTION  SPRITECURRENTHEIGHT     (handle%)                                               Returns the current height of a sprite.
'FUNCTION  SPRITECURRENTWIDTH      (handle%)                                               Returns the current width of a sprite.
'FUNCTION  SPRITEDIRECTION         (handle%)                                               Returns the direction angle a sprite's automotion has been set to.
'FUNCTION  SPRITEFILEEXISTS        (filename$)                                             Checks for the existance of the file specified.
'FUNCTION  SPRITEMOUSE             (handle%)                                               Returns the status of the current sprite and mouse pointer interaction.
'FUNCTION  SPRITEMOUSEAX           (handle%)                                               Returns the x location of the mouse on the screen.
'FUNCTION  SPRITEMOUSEAY           (handle%)                                               Returns the y location of the mouse on the screen.
'FUNCTION  SPRITEMOUSEX            (handle%)                                               Returns the x location of the mouse on the sprite itself.
'FUNCTION  SPRITEMOUSEY            (handle%)                                               Returns the y location of the mouse on the sprite itself.
'FUNCTION  SPRITENEW               (sheet%, cell%, behavior%)                              Creates a new sprite given the sheet and images that make up the sprite.
'FUNCTION  SPRITEROTATION          (handle%)                                               Gets the current rotation angle of a sprite in degrees.
'FUNCTION  SPRITESCORE             (handle%)                                               Retrieves the score value from a sprite.
'FUNCTION  SPRITESHEETLOAD         (filename$, spritewidth%, spriteheight%, transparent&)  Loads a sprite sheet into the sprite sheet array and assigns an integer handle value pointing to the sheet.
'FUNCTION  SPRITESHOWING           (handle%)                                               Returns the status of a sprite being hidden or not.
'FUNCTION  SPRITEX                 (handle%)                                               Returns the screen x location of a sprite. (integer) centered
'FUNCTION  SPRITEX1                (handle%)                                               Returns the upper left x screen position of the sprite.
'FUNCTION  SPRITEX2                (handle%)                                               Returns the lower right x screen position of the sprite.
'FUNCTION  SPRITEY                 (handle%)                                               Returns the screen y location of a sprite. (integer) centered
'FUNCTION  SPRITEY1                (handle%)                                               Returns the upper left y screen position of the sprite.
'FUNCTION  SPRITEY2                (handle%)                                               Returns the lower right y screen position of the sprite.
'FUNCTION  SPRITEZOOMLEVEL         (handle%)                                               Returns a sprite's current zoom level.
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/Z0x3pBuW
'
'For a template for writing programs with this library see:
'https://db.tt/Q90uEiID
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'This library has been modified to use OPTION BASE 0 in an OPTION BASE 1
'  environment
'
'******************************************************************************
'
'For the original version of this library and a tutorial for it download:
'https://dl.dropbox.com/u/416997/AllRitchiesQB64Libraries.zip
'
'(terry.ritchie@gmail.com)
'
'******************************************************************************

'##################################################################################################################################

SUB SPRITESPINSET (handle%, spin!)

'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets a sprite's automotion spin direction.                                 |                     - NOTES -                     |
'|                                                                            | - Specifyng a sprite that does not exist will     |
'| Usage : SPRITESPINSET mysprite%, 11.25                                     |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to set automotion spin on.                    |                                                   |
'|         spin!   - the amount of spin to add, positive or negative value.   |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESPINSET", 100 : EXIT SUB '            is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESPINSET", 101 : EXIT SUB '           is this sprite handle in use?
sprites(handle%).spindir = spin! '                                              set the sprite's automotion spin direction

END SUB

'##################################################################################################################################

FUNCTION SPRITEZOOMLEVEL (handle%)

'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns a sprite's current zoom level.                                     |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : zoom% = SPRITEZOOMLEVEL(mysprite%)                                 |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the zoom level from.              |                                                   |
'|                                                                            |                                                   |
'| Output: an integer value representing the sprite's zoom level.             |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEZOOMLEVEL", 100 : EXIT FUNCTION '          is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEZOOMLEVEL", 101 : EXIT FUNCTION '         is this sprite handle in use?
SPRITEZOOMLEVEL = sprites(handle%).zoom '                                       return the sprite's current zoom value

END FUNCTION

'##################################################################################################################################

SUB SPRITEREVERSEY (handle%)

'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Reverses the y vector automotion value of a sprite.                        |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEREVERSEY mysprite%                                           |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to reverse the y vector direction of.         |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEREVERSEY", 100 : EXIT SUB '           is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEREVERSEY", 101 : EXIT SUB '          is this sprite handle in use?
sprites(handle%).ydir = -sprites(handle%).ydir '                                 reverse sprite's y vector automotion value

END SUB

'##################################################################################################################################

SUB SPRITEREVERSEX (handle%)

'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Reverses the x vector automotion value of a sprite.                        |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEREVERSEX mysprite%                                           |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to reverse the x vector direction of.         |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEREVERSEX", 100 : EXIT SUB '           is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEREVERSEX", 101 : EXIT SUB '          is this sprite handle in use?
sprites(handle%).xdir = -sprites(handle%).xdir '                                 reverse sprite's x vector automotion value

END SUB

'##################################################################################################################################

FUNCTION SPRITECURRENTHEIGHT (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the current height of a sprite.                                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : sheight% = SPRITECURRENTHEIGHT(mysprite%)                          |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the current height information.   | - This is an especially useful command for        |
'|                                                                            |   getting the height of sprites that have been    |
'| Output: an integer value representing the height of the sprite.            |   rotated.                                        |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECURRENTHEIGHT", 100 : EXIT FUNCTION '      is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECURRENTHEIGHT", 101 : EXIT FUNCTION '     is this sprite handle in use?
SPRITECURRENTHEIGHT = sprites(handle%).currentheight '                          return the current height of the sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITECURRENTWIDTH (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the current width of a sprite.                                     |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that doe not exist will     |
'| Usage : swidth% = SPRITECURRENTWIDTH(mysprite%)                            |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the current width information.    | - This is an especially useful command for        |
'|                                                                            |   getting the width of sprites that have been     |
'| Output: an integer value representing the width of the sprite.             |   rotated.                                        |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECURRENTWIDTH", 100 : EXIT FUNCTION '       is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECURRENTWIDTH", 101 : EXIT FUNCTION '      is this sprite handle in use?
SPRITECURRENTWIDTH = sprites(handle%).currentwidth '                            return the current width of the sprite

END FUNCTION

'##################################################################################################################################

SUB SPRITETRAVEL (handle%, direction!, speed!)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Moves a sprite in the direction and speed indicated.                       |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exists will   |
'| Usage : SPRITETRAVEL mysprite%, 90, 10                                     |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%    - the sprite to move.                                   | - The direction and speed given will not set the  |
'|         direction! - the direction to move in (0 - 359.99..)               |   the sprite's automotion speed and direction     |
'|         speed!     - the speed to move at.                                 |   angle or vectors.                               |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITETRAVEL", 100 : EXIT SUB '             is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITETRAVEL", 101 : EXIT SUB '            is this sprite handle in use?
sprites(handle%).actualx = sprites(handle%).actualx + SIN(direction! * PI / 180) * speed! ' calculate x vector and update position
sprites(handle%).actualy = sprites(handle%).actualy + -COS(direction! * PI / 180) * speed! 'calculate y vector and update position

END SUB

'##################################################################################################################################

FUNCTION SPRITEDIRECTION (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the direction angle a sprite's automotion has been set to.         |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : angle! = SPRITEDIRECTION(mysprite%)                                |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the direction angle from.         | - The sprite direction angle is independant of    |
'|                                                                            |   the sprite's rotation angle.                    |
'| Output: a single numeric value representing direction angle (0 - 359.99..) |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEDIRECTION", 100 : EXIT FUNCTION '          is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEDIRECTION", 101 : EXIT FUNCTION '         is this sprite handle in use?
SPRITEDIRECTION = sprites(handle%).direction '                                  return the automotion direction angle

END FUNCTION

'##################################################################################################################################

SUB SPRITEDIRECTIONSET (handle%, direction!)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets a sprite's automotion direction angle.                                |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEDIRECTIONSET mysprite%, 270                                  |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%    - the sprite to set the direction.                      | - The direction value is used by the automotion   |
'|         direction! - the angle of direction to set (0 - 359.99..)          |   portion of the sprite library. This value will  |
'|                                                                            |   not be used unless automotion has been enabled  |
'| Output: none                                                               |   for the sprite (SPRITEMOTION).                  |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEDIRECTIONSET", 100 : EXIT SUB '       is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEDIRECTIONSET", 101 : EXIT SUB '      is this sprite handle in use?
sprites(handle%).direction = direction! '                                       set the automotion direction angle
sprites(handle%).xdir = SIN(direction! * 3.1415926 / 180) * sprites(handle%).speed ' calculate the x direction vector
sprites(handle%).ydir = -COS(direction! * 3.1415926 / 180) * sprites(handle%).speed 'calculate the y direction vector

END SUB

'##################################################################################################################################

SUB SPRITESPEEDSET (handle%, speed!)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets a sprite's automotion speed in pixels.                                |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITESPEEDSET mysprite%, 5                                        |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to set the speed.                             | - The speed value is used by the automotion       |
'|         speed!  - the number of pixels or partial pixels to move.          |   portion of the sprite library. This value will  |
'|                                                                            |   not be used unless automotion has been enabled  |
'| Output: none                                                               |   for the sprite (SPRITEMOTION).                  |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESPEEDSET", 100 : EXIT SUB '           is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESPEEDSET", 101 : EXIT SUB '          is this sprite handle in use?
sprites(handle%).speed = speed! '                                               set the sprite's speed value

END SUB

'##################################################################################################################################

SUB SPRITEMOTION (handle%, behavior%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Enables or disables a sprite's automotion feature.                         |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEMOTION mysprite%, -1                                         |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%   - the sprite to turn on or off automotion.               | - This subroutine will enable the automotion      |
'|         behavior% - enable or disable the sprite's automotion.             |   portion of the sprite library which uses the    |
'|                 0 = disable automotion (constant DONTMOVE)                 |   sprite's direction (SPRITEDIRECTIONSET) and     |
'|                -1 = enable automotion  (constant MOVE)                     |   speed (SPRITESPEEDSET) values.                  |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITEMOTION handle%, behavior%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOTION", 100 : EXIT SUB '             is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOTION", 101 : EXIT SUB '            is this sprite handle in use?
        sprites(handle%).motion = behavior% '                                           set the automotion behavior
END SELECT

END SUB

'##################################################################################################################################

FUNCTION SPRITESCORE (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Retrieves the score value from a sprite.                                   |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : score% = SPRITESCORE(mysprite%)                                    |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite the score is to be retrieved from.            |                                                   |
'|                                                                            |                                                   |
'| Output: An single value representing the score value saved.                |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESCORE", 100 : EXIT FUNCTION '              is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESCORE", 101 : EXIT FUNCTION '             is this sprite handle in use?
SPRITESCORE = sprites(handle%).score '                                          return the score value of sprite

END FUNCTION

'##################################################################################################################################

SUB SPRITESCORESET (handle%, value!)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets the score value of a sprite.                                          |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITESCORESET mysprite%, 100                                      |   result in the subroutine reporting an error     |
'|                                                                            |   and halting program execution.                  |
'| Input : handle% - the sprite number of the score to set.                   | - This subroutine can also be used as a storage   |
'|         value!  - the single numeric value to store as the score.          |   container for any numeric data that needs to be |
'|                                                                            |   associated with the sprite or even as a flag    |
'| Output: none                                                               |   indicator.                                      |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESCORESET", 100 : EXIT SUB '           is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESCORESET", 101 : EXIT SUB '          is this sprite handle in use?
sprites(handle%).score = value! '                                               set the sprite's score value

END SUB

'##################################################################################################################################

FUNCTION SPRITESHOWING (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the status of a sprite being hidden or not.                        |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : visible% = SPRITESHOWING(mysprite%)                                |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite number of the visible status to retrieve.     |                                                   |
'|                                                                            |                                                   |
'| Output: an integer value of -1 (true) if showing or 0 (false) if not.      |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESHOWING", 100 : EXIT FUNCTION '            is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESHOWING", 101 : EXIT FUNCTION '           is this sprite handle in use?
SPRITESHOWING = sprites(handle%).visible '                                      return hidden status of sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEANGLE (handle%, handle2%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Retrieves the angle in degrees between two sprites.                        |                     - NOTES -                     |
'|                                                                            | - Specifying sprites that do not exist will       |
'| Usage : missiledir! = SPRITEANGLE(mysprite%, anothersprite%)               |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle%  - the sprite where the angle is originating from.         |                                                   |
'|         handle2% - the sprite the first sprite is pointing to.             |                                                   |
'|                                                                            |                                                   |
'| Output: a single value with the range of 0 to 359.99....                   |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: Most of this code was realized by using Galleon's getangle# function as found in the following forum topic:           |
'|          http://www.qb64.net/forum/index.php?topic=3934.0                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANGLE", 100 : EXIT FUNCTION '              is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEANGLE", 101 : EXIT FUNCTION '             is this sprite handle in use?
IF handle2% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANGLE", 100 : EXIT FUNCTION '             is this an invalid sprite handle?
IF NOT sprites(handle2%).inuse THEN SPRITEERROR "SPRITEANGLE", 101 : EXIT FUNCTION '            is this sprite handle in use?
IF sprites(handle2%).currenty = sprites(handle%).currenty THEN '                 are both sprites at same y location?
    IF sprites(handle%).currentx = sprites(handle2%).currentx THEN EXIT FUNCTION 'yes, if both at same x the leave, at same location
    IF sprites(handle2%).currentx > sprites(handle%).currentx THEN SPRITEANGLE = 90 ELSE SPRITEANGLE = 270 'sprite is either right or left
    EXIT FUNCTION
END IF
IF sprites(handle2%).currentx = sprites(handle%).currentx THEN '                 are both sprites at same x location?
    IF sprites(handle2%).currenty > sprites(handle%).currenty THEN SPRITEANGLE = 180 'yes, if its greater, 180, less, 0
    EXIT FUNCTION
END IF
IF sprites(handle2%).currenty < sprites(handle%).currenty THEN '                 is sprite2 y value less than sprite1?
    IF sprites(handle2%).currentx > sprites(handle%).currentx THEN '             yes, is sprite2 x value greater than sprite1?
        SPRITEANGLE = ATN((sprites(handle2%).currentx - sprites(handle%).currentx) / (sprites(handle2%).currenty - sprites(handle%).currenty)) * -57.2957795131
    ELSE
        SPRITEANGLE = ATN((sprites(handle2%).currentx - sprites(handle%).currentx) / (sprites(handle2%).currenty - sprites(handle%).currenty)) * -57.2957795131 + 360
    END IF
ELSE
    SPRITEANGLE = ATN((sprites(handle2%).currentx - sprites(handle%).currentx) / (sprites(handle2%).currenty - sprites(handle%).currenty)) * -57.2957795131 + 180
END IF

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITECOLLIDEWITH (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the sprite number that collided with the specified sprite.         |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : hit% = SPRITECOLLIDEWITH(mysprite%)                                |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the collision information from.   | - This function should be used after the function |
'|                                                                            |   SPRITECOLLIDE() was called and ALLSPRITES       |
'| Output: handle number of the sprite that collided with specified sprite.   |   specified to determine which sprite collided.   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECOLLIDEWITH", 100 : EXIT FUNCTION '        is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECOLLIDEWITH", 101 : EXIT FUNCTION '       is this sprite handle in use?
SPRITECOLLIDEWITH = sprites(handle%).collsprite '                               return the handle number of the colliding sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITECOLLIDE (handle%, handle2%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the status of collisions with other sprites.                       |                     - NOTES -                     |
'|                                                                            | - Specifying sprites that do not exist will       |
'| Usage : hit% = SPRITECOLLIDE(mysprite%, anothersprite%)                    |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle%  - the sprite to test for collisions.                      | - The constant ALLSPRITES was created to be used  |
'|         handle2% - the sprite(s) to test for a collision with.             |   with this function.                             |
'|               -1 = test all sprites for collision (constant ALLSPRITES).   | - THERE IS A KNOWN PROBLEM currently with this    |
'|              > 0 = the specific sprite to check for a collision with.      |   function. See "Known Issues" below.             |
'|                                                                            |                                                   |
'| Output: 0 if no collision, the sprite handle number colliding with.        |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Known Issues: If two identical sized sprites come at each other on the same horizontal or vertical position a collision will   |
'|               not be detected. The work-around for now is to make sure that exact size sprites be one pixel off from each      |
'|               other. This problem will be resolved soon.                                                                       |
'+--------------------------------------------------------------------------------------------------------------------------------+
'| Credits: Portions of this code were realized by examining UnseenMachine's (John Onyon) GDK code. His Game Development Kit(GDK) |
'|          can be downloaded from here: http://dl.dropbox.com/u/8822351/UnseenGDK.bm and a tutorial for the GDK can be           |
'|          downloaded from here: http://dl.dropbox.com/u/8822351/UnseenGDK_Tutorial.doc                                          |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM count% '                                                                   counter used to cycle through sprite array
DIM countfrom% '                                                               where to start count cycle
DIM countto% '                                                                 where to end count cycle
DIM x% '                                                                       pixel accurate collision box pixel x location
DIM y% '                                                                       pixel accurate collision box pixel y location
DIM w% '                                                                       pixel accurate collision box width
DIM h% '                                                                       pixel accurate collision box height
DIM p1& '                                                                      pixel accurate image 1 pixel color
DIM p2& '                                                                      pixel accurate image 2 pixel color

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECOLLIDE", 100 : EXIT FUNCTION '            is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECOLLIDE", 101 : EXIT FUNCTION '           is this sprite handle in use?
SPRITECOLLIDE = 0 '                                                            assume no collision and send this assumption back
IF sprites(handle%).detecttype = 0 THEN EXIT FUNCTION '                         the sprite does not have collision detection on
sprites(handle%).detect = 0 '                                                   assume no collision
IF handle2% = -1 THEN '                                                        should all sprites be checked for collisions?
    countfrom% = 1 '                                                           yes, start at beginning of sprite array
    countto% = UBOUND(sprites) '                                                end at the last element in sprite array
ELSE '                                                                         checking for a specific sprite collision
    IF handle2% > UBOUND(sprites) THEN SPRITEERROR "SPRITECOLLIDE", 100 : EXIT FUNCTION '       is this an invalid sprite handle?
    IF NOT sprites(handle2%).inuse THEN SPRITEERROR "SPRITECOLLIDE", 101 : EXIT FUNCTION '      is this sprite handle in use?
    countfrom% = handle2% '                                                    start at element of specific sprite in array
    countto% = handle2% '                                                      end at element of specific sprite in array
END IF
FOR count% = countfrom% TO countto% '                                          cycle through all known sprites or single sprite
    IF count% <> handle% THEN '                                                can't detect a collision with ourself!
        IF sprites(count%).visible THEN '                                       don't check hidden sprites
            IF sprites(count%).detecttype <> 0 THEN '                           ignore sprites with no detection enabled
                IF sprites(count%).layer = sprites(handle%).layer THEN '         sprites must be on same layer to collide
                    IF sprites(handle%).screenx1 <= sprites(count%).screenx1 + sprites(count%).currentwidth THEN '               is upper left x of sprite within current sprite?
                        IF sprites(handle%).screenx1 + sprites(handle%).currentwidth >= sprites(count%).screenx1 THEN '          is upper left x of current sprite within sprite?
                            IF sprites(handle%).screeny1 <= sprites(count%).screeny1 + sprites(count%).currentheight THEN '      is upper left y of sprite within current sprite?
                                IF sprites(handle%).screeny1 + sprites(handle%).currentheight >= sprites(count%).screeny1 THEN ' is upper left y of current sprite within sprite?
                                    sprites(handle%).detect = -1 '              all true, sprite has collision
                                    sprites(count%).detect = -1 '               current sprite also has collision
                                    sprites(handle%).collsprite = count% '      sprite colliding with current sprite
                                    sprites(count%).collsprite = handle% '      current sprite colliding with sprite
                                    EXIT FOR '                                 no need to check the rest of sprites
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF
NEXT count%
IF sprites(handle%).detect THEN '                                               a box collision was detected
    SPRITECOLLIDE = sprites(handle%).collsprite '                               return the sprite number the collision happened with
    IF sprites(handle%).detecttype = 2 AND sprites(sprites(handle%).collsprite).detecttype = 2 THEN ' we need to go to pixel accurate detection
        IF sprites(handle%).screenx1 < sprites(sprites(handle%).collsprite).screenx1 THEN ' get collision area and place coordinates in each sprite
            sprites(sprites(handle%).collsprite).collx1 = 0
            sprites(handle%).collx1 = sprites(sprites(handle%).collsprite).screenx1 - sprites(handle%).screenx1 - 1
            IF sprites(sprites(handle%).collsprite).currentwidth + sprites(handle%).collx1 < sprites(handle%).currentwidth THEN
                sprites(sprites(handle%).collsprite).collx2 = sprites(sprites(handle%).collsprite).currentwidth - 1
                sprites(handle%).collx2 = sprites(handle%).collx1 + sprites(sprites(handle%).collsprite).currentwidth - 1
            ELSE
                sprites(handle%).collx2 = sprites(handle%).currentwidth - 1
                sprites(sprites(handle%).collsprite).collx2 = (sprites(handle%).screenx1 + sprites(handle%).currentwidth) - sprites(sprites(handle%).collsprite).screenx1 - 1
            END IF
        ELSEIF sprites(handle%).screenx1 > sprites(sprites(handle%).collsprite).screenx1 THEN
            sprites(handle%).collx1 = 0
            sprites(sprites(handle%).collsprite).collx1 = sprites(handle%).screenx1 - sprites(sprites(handle%).collsprite).screenx1 - 1
            IF sprites(sprites(handle%).collsprite).currentwidth - sprites(sprites(handle%).collsprite).collx1 < sprites(handle%).currentwidth THEN
                sprites(handle%).collx2 = (sprites(handle%).collx1 + sprites(sprites(handle%).collsprite).currentwidth) - sprites(sprites(handle%).collsprite).collx1 - 1
                sprites(sprites(handle%).collsprite).collx2 = sprites(sprites(handle%).collsprite).currentwidth - 1
            ELSE
                sprites(handle%).collx2 = sprites(handle%).currentwidth - 1
                sprites(sprites(handle%).collsprite).collx2 = sprites(sprites(handle%).collsprite).collx1 + sprites(handle%).currentwidth - 1
            END IF
        END IF
        IF sprites(handle%).screeny1 < sprites(sprites(handle%).collsprite).screeny1 THEN
            sprites(sprites(handle%).collsprite).colly1 = 0
            sprites(handle%).colly1 = sprites(sprites(handle%).collsprite).screeny1 - sprites(handle%).screeny1 - 1
            IF sprites(sprites(handle%).collsprite).currentheight + sprites(handle%).colly1 < sprites(handle%).currentheight THEN
                sprites(sprites(handle%).collsprite).colly2 = sprites(sprites(handle%).collsprite).currentheight - 1
                sprites(handle%).colly2 = sprites(handle%).colly1 + sprites(sprites(handle%).collsprite).currentheight - 1
            ELSE
                sprites(handle%).colly2 = sprites(handle%).currentheight - 1
                sprites(sprites(handle%).collsprite).colly2 = (sprites(handle%).screeny1 + sprites(handle%).currentheight) - sprites(sprites(handle%).collsprite).screeny1 - 1
            END IF
        ELSEIF sprites(handle%).screeny1 > sprites(sprites(handle%).collsprite).screeny1 THEN
            sprites(handle%).colly1 = 0
            sprites(sprites(handle%).collsprite).colly1 = sprites(handle%).screeny1 - sprites(sprites(handle%).collsprite).screeny1 - 1
            IF sprites(sprites(handle%).collsprite).currentheight - sprites(sprites(handle%).collsprite).colly1 < sprites(handle%).currentheight THEN
                sprites(handle%).colly2 = (sprites(handle%).colly1 + sprites(sprites(handle%).collsprite).currentheight) - sprites(sprites(handle%).collsprite).colly1 - 1
                sprites(sprites(handle%).collsprite).colly2 = sprites(sprites(handle%).collsprite).currentheight - 1
            ELSE
                sprites(handle%).colly2 = sprites(handle%).currentheight - 1
                sprites(sprites(handle%).collsprite).colly2 = sprites(sprites(handle%).collsprite).colly1 + sprites(handle%).currentheight - 1
            END IF
        END IF '                                                               collision coordinates now in both sprites
        x% = 0 '                                                               start at upper left x of each collision box
        y% = 0 '                                                               start at upper left y of each collision box
        w% = sprites(handle%).collx2 - sprites(handle%).collx1 '- 1 '            get the width of the collision boxes
        h% = sprites(handle%).colly2 - sprites(handle%).colly1 '- 1 '            get the height of the collision boxes
        DO '                                                                   start looping through collision box pixels
            _SOURCE sprites(handle%).image '                                    set the first sprite as the image source
            p1~& = POINT(sprites(handle%).collx1 + x%, sprites(handle%).colly1 + y%) ' get the current pixel's color
            a1& = _ALPHA32(p1~&)
            _SOURCE sprites(sprites(handle%).collsprite).image '                 set the second sprite as the image source
            p2~& = POINT(sprites(sprites(handle%).collsprite).collx1 + x%, sprites(sprites(handle%).collsprite).colly1 + y%) ' get the current pixel's color
            a2& = _ALPHA32(p2~&)
            IF (a1& <> 0) AND (a2& <> 0) THEN EXIT DO '                        if both are not transparent then we have a collision
            x% = x% + 1 '                                                      move right one pixel
            IF x% > w% THEN '                                                  have we reached the edge of the collision box?
                x% = 0 '                                                       yes, move back to the left of the collision box
                y% = y% + 1 '                                                  move down one pixel
            END IF
        LOOP UNTIL y% > h% '                                                   stop when gone past the bottom of collision box
        IF y% > h% THEN SPRITECOLLIDE = 0 '                                    no collision if we checked entire collision box
    END IF
    _SOURCE 0
END IF

END FUNCTION

'##################################################################################################################################

SUB SPRITECOLLIDETYPE (handle%, behavior%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets the type of collision detection used for a sprite.                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITECOLLIDETYPE mysprite%, 2                                     |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%   - the number of the sprite to set the collision type.    | - The constants NODETECT, BOXDETECT and           |
'|         behavior% - the type of collision detection desired for sprite:    |   PIXELDETECT have been created to be used with   |
'|                 0 - no collision detection        (constant NODETECT)      |   this subroutine.                                |
'|                 1 - rectangular box detection     (constant BOXDETECT)     |                                                   |
'|                 2 - pixel accurate detection      (constant PIXELDETECT)   |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().detecttype to desired detection method.                   |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECOLLIDETYPE", 100 : EXIT SUB '        is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECOLLIDETYPE", 101 : EXIT SUB '       is this sprite handle in use?
IF behavior% = 2 THEN sprites(handle%).image = _NEWIMAGE(1, 1, 32) '            temp image for first time deletion in SPRITEPUT()
sprites(handle%).detecttype = behavior% '                                       set the collision detection method for this sprite

END SUB

'##################################################################################################################################

SUB SPRITEPUT (x!, y!, handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Places a sprite on screen at coordinates INT(x!), INT(y!).                 |                     - NOTES -                     |
'|                                                                            | - The x! and y! values can be sent in as integers |
'| Usage : SPRITEPUT 10, 10, mysprite%                                        |   or single precision to allow the programmer to  |
'|                                                                            |   use fine or pixel by pixel movements.           |
'| Input : x!      - row to display sprite at.                                |                                                   |
'|         y!      - column to display sprite at.                             |                                                   |
'|         handle% - the sprite to display on the screen.                     |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().onscreen to -1 if not previously on screen.               |                                                   |
'|         sprite().actualx to equal x! variable passed in.                   |                                                   |
'|         sprite().actualy to equal y! variable passed in.                   |                                                   |
'|         sprite().currentx to equal INT(x!) variable passed in.             |                                                   |
'|         sprite().currenty to equal INT(y!) variable passed in.             |                                                   |
'|         sprite().currentwidth to equal the screen width of sprite.         |                                                   |
'|         sprite().currentheight to equal the screen height of sprite.       |                                                   |
'|         sprite().background to hold the image of the background if needed. |                                                   |
'|         sprite().screenx1 to upper left x of sprite on screen. *           |                                                   |
'|         sprite().screeny1 to upper left y of sprite on screen. *           |                                                   |
'|         sprite().screenx2 to lower right x of sprite on screen.            |                                                   |
'|         sprite().screeny2 to lower right y of sprite on screen.            |                                                   |
'|         sprite().backx to upper left x of sprite on screen. *              |                                                   |
'|         sprite().backy to upper left y of sprite on screen. *              |                                                   |
'|         * = redundant?                                                     |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: Portions of this subroutine have code based off of Galleon's RotoZoom subroutine found in the QB64 documentation at   |
'|          http://qb64.net/wiki/index.php?title=MAPTRIANGLE                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sheets() AS SHEET '                                                      array defining sprite sheets
SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM tempsprite& '                                                              temporary holding image for sprite from sheet
DIM cellx% '                                                                   the upper left x location of sprite on sheet
DIM celly% '                                                                   the upper right y location of sprite on sheet
DIM px!(0 TO 3) '                                                                   polar x coordinates of maptriangle
DIM py!(0 TO 3) '                                                                   polar y coordinates of maptriangle
DIM sinr! '                                                                    the sine function used on rotation
DIM cosr! '                                                                    the cosine function used on rotation
DIM count% '                                                                   a generic counter used in subroutine
DIM x2& '                                                                      temp variable used when computing polar coordinates
DIM y2& '                                                                      temp variable used when computing polar coordinates
DIM swidth% '                                                                  the width of the sprite on the sprite sheet
DIM sheight% '                                                                 the height of the sprite on the sprite sheet
DIM bx1% '                                                                     upper left x location of background image
DIM by1% '                                                                     upper left y location of background image
DIM bx2% '                                                                     lower right x location of background image
DIM by2% '                                                                     lower right y location of background image
DIM cx% '                                                                      used to center pixel accurate collision image
DIM cy% '                                                                      used to center pixel accurate collision image

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEPUT", 100 : EXIT SUB '                is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEPUT", 101 : EXIT SUB '               is this sprite handle in use?

swidth% = sheets(sprites(handle%).sheet).spritewidth '                           width of sprite
sheight% = sheets(sprites(handle%).sheet).spriteheight '                         height of sprite

IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '                 is sprite on screen and should it restore background
    _PUTIMAGE (sprites(handle%).backx + (swidth% / 2), sprites(handle%).backy + (sheight% / 2)), sprites(handle%).background
    _FREEIMAGE sprites(handle%).background '                                    background image no longer needed
ELSEIF (NOT sprites(handle%).onscreen) AND sprites(handle%).visible THEN '       hAS SPRITE been made visible again or first time?
    sprites(handle%).onscreen = -1 '                                            sprite will be placed on the screen
END IF

IF sprites(handle%).motion THEN '                                               is sprite automotion enabled?
    sprites(handle%).actualx = sprites(handle%).actualx + sprites(handle%).xdir ' yes, update sprite x position
    sprites(handle%).actualy = sprites(handle%).actualy + sprites(handle%).ydir ' update sprite y position
    sprites(handle%).currentx = INT(sprites(handle%).actualx) '                  this is where the sprite will show on screen
    sprites(handle%).currenty = INT(sprites(handle%).actualy) '                  this is where the sprite will show on screen

ELSE
    sprites(handle%).actualx = x! '                                             allows user to use small increments if desired
    sprites(handle%).actualy = y! '                                             allows user to use small increments if desired
    sprites(handle%).currentx = INT(x!) '                                       this is where the sprite will show on screen
    sprites(handle%).currenty = INT(y!) '                                       this is where the sprite will show on screen
END IF

IF NOT sprites(handle%).visible THEN EXIT SUB '                                 if sprite hidden no need to do rest of subroutine
IF sprites(handle%).animation THEN SPRITENEXT handle% '                         perform autoanimation if enabled

IF sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns = 0 THEN ' is sprite in rightmost column?
    cellx% = swidth% * (sheets(sprites(handle%).sheet).columns - 1) '            yes, upper left x position of sprite on sprite sheet
    celly% = ((sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) - 1) * sheight% ' upper left y position on sheet
ELSE '                                                                         sprite is not in rightmost column
    cellx% = (sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns - 1) * swidth% ' upper left x position of sprite on sheet
    celly% = (sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) * sheight% '      upper left y position of sprite on sheet
END IF
IF sprites(handle%).zoom <> 100 THEN '                                          does the sprite need to be zoomed in or out?
    swidth% = swidth% * (sprites(handle%).zoom / 100) '                         yes, calculate new sprite width
    sheight% = sheight% * (sprites(handle%).zoom / 100) '                       calculate new sprite height
END IF
tempsprite& = _NEWIMAGE(swidth%, sheight%, 32) '                               create temporary image holder for sprite
SELECT CASE sprites(handle%).flip '                                             should the image be flipped while copied?
    CASE 0 '                                                                   no flip, copy original sprite orientation
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly%)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 1 '                                                                   flip sprite horizontally while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)-(cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 2 '                                                                   flip sprite vertically while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)
    CASE 3 '                                                                   flip sprite both horizontally and vertically
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx%, celly%)
END SELECT
px!(0) = -swidth% / 2 '                                                        upper left  x polar coordinate of sprite
py!(0) = -sheight% / 2 '                                                       upper left  y polar coordinate of sprite
px!(1) = px!(0) '                                                              lower left  x polar coordinate of sprite
py!(1) = sheight% / 2 '                                                        lower left  y polar coordinate of sprite
px!(2) = swidth% / 2 '                                                         lower right x polar coordinate of sprite
py!(2) = py!(1) '                                                              lower right y polar coordinate of sprite
px!(3) = px!(2) '                                                              upper right x polar coordinate of sprite
py!(3) = py!(0) '                                                              upper right y polar coordinate of sprite


IF sprites(handle%).motion AND sprites(handle%).spindir <> 0 THEN
    sprites(handle%).rotation = sprites(handle%).rotation + sprites(handle%).spindir
    IF sprites(handle%).rotation < 0 THEN sprites(handle%).rotation = sprites(handle%).rotation + 360
    IF sprites(handle%).rotation >= 360 THEN sprites(handle%).rotation = sprites(handle%).rotation - 360
END IF


IF sprites(handle%).rotation <> 0 THEN '                                        does the sprite need to be rotated?
    sinr! = SIN(-sprites(handle%).rotation / 57.2957795131) '                   yes, some magic math for rotation
    cosr! = COS(-sprites(handle%).rotation / 57.2957795131) '                   some more magic math for rotation
    bx1% = 0 '                                                                 upper left  x coordinate of background
    by1% = 0 '                                                                 upper left  y coordinate of background
    bx2% = 0 '                                                                 lower right x coordinate of background
    by2% = 0 '                                                                 lower right y coordinate of background
    FOR count% = 0 TO 3 '                                                      cycle through all four polar coordinates
        x2& = (px!(count%) * cosr! + sinr! * py!(count%)) '                    compute new polar coordinate location
        y2& = (py!(count%) * cosr! - px!(count%) * sinr!) '                    compute new polar coordinate location
        px!(count%) = x2& '                                                    save the new polar coordinate
        py!(count%) = y2& '                                                    save the new polar coordinate
        IF px!(count%) < bx1% THEN bx1% = px!(count%) '                        save lowest  x value seen \
        IF px!(count%) > bx2% THEN bx2% = px!(count%) '                        save highest x value seen  \ background image
        IF py!(count%) < by1% THEN by1% = py!(count%) '                        save lowest  y value seen  / rectangle coordinates
        IF py!(count%) > by2% THEN by2% = py!(count%) '                        save highest y value seen /
    NEXT count%
    IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '             should the sprite save the background?
        sprites(handle%).background = _NEWIMAGE((bx2% - bx1%) + 1, (by2% - by1%) + 1, 32) ' yes, compute the background image
        _PUTIMAGE , _DEST, sprites(handle%).background, (sprites(handle%).currentx + bx1% + (swidth% / 2), sprites(handle%).currenty + by1% + (sheight% / 2))-(sprites(handle%).currentx + bx2% + (swidth% / 2), sprites(handle%).currenty + by2% + (sheight% / 2))
        sprites(handle%).backx = sprites(handle%).currentx + bx1% ' save the background's x location
        sprites(handle%).backy = sprites(handle%).currenty + by1% ' save the background's y location
    END IF
    sprites(handle%).currentwidth = bx2% - bx1% + 1 '                           save width of sprite after zoom and rotation
    sprites(handle%).currentheight = by2% - by1% + 1 '                          save height of sprite after zoom and rotation
    sprites(handle%).screenx1 = sprites(handle%).currentx + bx1% '               save upper left x position of sprite on screen
    sprites(handle%).screeny1 = sprites(handle%).currenty + by1% '               save upper left y potition of sprite on screen
    sprites(handle%).screenx2 = sprites(handle%).currentx + bx2% '               save lower right x position of sprite on screen
    sprites(handle%).screeny2 = sprites(handle%).currenty + by2% '               save lower right y potition of sprite on screen
    _MAPTRIANGLE (0, 0)-(0, sheight% - 1)-(swidth% - 1, sheight% - 1), tempsprite& TO(sprites(handle%).currentx + px!(0), sprites(handle%).currenty + py!(0))-(sprites(handle%).currentx + px!(1), sprites(handle%).currenty + py!(1))-(sprites(handle%).currentx + px!(2), sprites(handle%).currenty + py!(2))
    _MAPTRIANGLE (0, 0)-(swidth% - 1, 0)-(swidth% - 1, sheight% - 1), tempsprite& TO(sprites(handle%).currentx + px!(0), sprites(handle%).currenty + py!(0))-(sprites(handle%).currentx + px!(3), sprites(handle%).currenty + py!(3))-(sprites(handle%).currentx + px!(2), sprites(handle%).currenty + py!(2))
    IF sprites(handle%).detecttype = 2 THEN '                                   does sprite use pixel accuracy collision detection?
        _FREEIMAGE sprites(handle%).image '                                     yes, get rid of the last image save
        sprites(handle%).image = _NEWIMAGE(sprites(handle%).currentwidth, sprites(handle%).currentheight, 32) ' create a new image holder and map triangles in
        cx% = sprites(handle%).currentwidth / 2
        cy% = sprites(handle%).currentheight / 2
        _MAPTRIANGLE (0, 0)-(0, sheight% - 1)-(swidth% - 1, sheight% - 1), tempsprite& TO(cx% + px!(0), cy% + py!(0))-(cx% + px!(1), cy% + py!(1))-(cx% + px!(2), cy% + py!(2)), sprites(handle%).image
        _MAPTRIANGLE (0, 0)-(swidth% - 1, 0)-(swidth% - 1, sheight% - 1), tempsprite& TO(cx% + px!(0), cy% + py!(0))-(cx% + px!(3), cy% + py!(3))-(cx% + px!(2), cy% + py!(2)), sprites(handle%).image
    END IF
ELSE '                                                                         no rotation was needed, just place image on screen
    IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '             should the sprite save the background?
        sprites(handle%).background = _NEWIMAGE(INT(px!(2)) - INT(px!(0)), INT(py!(2)) - INT(py!(0)), 32) ' yes, compute the background image
        _PUTIMAGE , _DEST, sprites(handle%).background, (sprites(handle%).currentx + INT(px!(0)) + (swidth% / 2), sprites(handle%).currenty + INT(py!(0)) + (sheight% / 2))-(sprites(handle%).currentx + INT(px!(2)) - 1 + (swidth% / 2), sprites(handle%).currenty + INT(py!(2)) - 1 + (sheight% / 2))
        sprites(handle%).backx = sprites(handle%).currentx + INT(px!(0)) '       save the background's x location
        sprites(handle%).backy = sprites(handle%).currenty + INT(py!(0)) '       save the background's y location
    END IF
    sprites(handle%).currentwidth = INT(px!(2)) - INT(px!(0)) '                 save width of sprite after zoom
    sprites(handle%).currentheight = INT(py!(2)) - INT(py!(0)) '                save height of sprite after zoom
    sprites(handle%).screenx1 = sprites(handle%).currentx + INT(px!(0)) '        save upper left x of sprite on screen
    sprites(handle%).screeny1 = sprites(handle%).currenty + INT(py!(0)) '        save upper left y of sprite on screen
    sprites(handle%).screenx2 = sprites(handle%).currentx + INT(px!(2)) - 1 '    save lower right x of sprite onscreen
    sprites(handle%).screeny2 = sprites(handle%).currenty + INT(py!(2)) - 1 '    save lower right y of sprite onscreen

    _PUTIMAGE (sprites(handle%).screenx1 + (swidth% / 2), sprites(handle%).screeny1 + (sheight% / 2)), tempsprite& ' copy temporary sprite image to the screen

    IF sprites(handle%).detecttype = 2 THEN '                                   does sprite use pixel accuracy collision detection?
        _FREEIMAGE sprites(handle%).image '                                     yes, get rid of the last image saved
        sprites(handle%).image = _COPYIMAGE(tempsprite&) '                      save a copy of the current sprite image
    END IF
END IF
_FREEIMAGE tempsprite& '                                                       temporary image holder no longer needed

END SUB

'##################################################################################################################################

SUB SPRITEPLACE (x!, y!, handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Places a sprite on screen at coordinates INT(x!), INT(y!).                 |                     - NOTES -                     |
'|                                                                            | - The x! and y! values can be sent in as integers |
'| Usage : SPRITEPUT 10, 10, mysprite%                                        |   or single precision to allow the programmer to  |
'|                                                                            |   use fine or pixel by pixel movements.           |
'| Input : x!      - row to display sprite at.                                |                                                   |
'|         y!      - column to display sprite at.                             |                                                   |
'|         handle% - the sprite to display on the screen.                     |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().onscreen to -1 if not previously on screen.               |                                                   |
'|         sprite().actualx to equal x! variable passed in.                   |                                                   |
'|         sprite().actualy to equal y! variable passed in.                   |                                                   |
'|         sprite().currentx to equal INT(x!) variable passed in.             |                                                   |
'|         sprite().currenty to equal INT(y!) variable passed in.             |                                                   |
'|         sprite().currentwidth to equal the screen width of sprite.         |                                                   |
'|         sprite().currentheight to equal the screen height of sprite.       |                                                   |
'|         sprite().background to hold the image of the background if needed. |                                                   |
'|         sprite().screenx1 to upper left x of sprite on screen. *           |                                                   |
'|         sprite().screeny1 to upper left y of sprite on screen. *           |                                                   |
'|         sprite().screenx2 to lower right x of sprite on screen.            |                                                   |
'|         sprite().screeny2 to lower right y of sprite on screen.            |                                                   |
'|         sprite().backx to upper left x of sprite on screen. *              |                                                   |
'|         sprite().backy to upper left y of sprite on screen. *              |                                                   |
'|         * = redundant?                                                     |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: Portions of this subroutine have code based off of Galleon's RotoZoom subroutine found in the QB64 documentation at   |
'|          http://qb64.net/wiki/index.php?title=MAPTRIANGLE                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sheets() AS SHEET '                                                      array defining sprite sheets
SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM tempsprite& '                                                              temporary holding image for sprite from sheet
DIM cellx% '                                                                   the upper left x location of sprite on sheet
DIM celly% '                                                                   the upper right y location of sprite on sheet
DIM px!(0 TO 3) '                                                                   polar x coordinates of maptriangle
DIM py!(0 TO 3) '                                                                   polar y coordinates of maptriangle
DIM sinr! '                                                                    the sine function used on rotation
DIM cosr! '                                                                    the cosine function used on rotation
DIM count% '                                                                   a generic counter used in subroutine
DIM x2& '                                                                      temp variable used when computing polar coordinates
DIM y2& '                                                                      temp variable used when computing polar coordinates
DIM swidth% '                                                                  the width of the sprite on the sprite sheet
DIM sheight% '                                                                 the height of the sprite on the sprite sheet
DIM bx1% '                                                                     upper left x location of background image
DIM by1% '                                                                     upper left y location of background image
DIM bx2% '                                                                     lower right x location of background image
DIM by2% '                                                                     lower right y location of background image
DIM cx% '                                                                      used to center pixel accurate collision image
DIM cy% '                                                                      used to center pixel accurate collision image

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEPUT", 100 : EXIT SUB '                is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEPUT", 101 : EXIT SUB '               is this sprite handle in use?

swidth% = sheets(sprites(handle%).sheet).spritewidth '                           width of sprite
sheight% = sheets(sprites(handle%).sheet).spriteheight '                         height of sprite

IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '                 is sprite on screen and should it restore background
    _PUTIMAGE (sprites(handle%).backx + (swidth% / 2), sprites(handle%).backy + (sheight% / 2)), sprites(handle%).background
    _FREEIMAGE sprites(handle%).background '                                    background image no longer needed
ELSEIF (NOT sprites(handle%).onscreen) AND sprites(handle%).visible THEN '       hAS SPRITE been made visible again or first time?
    sprites(handle%).onscreen = -1 '                                            sprite will be placed on the screen
END IF

IF sprites(handle%).motion THEN '                                               is sprite automotion enabled?
    sprites(handle%).actualx = sprites(handle%).actualx + sprites(handle%).xdir ' yes, update sprite x position
    sprites(handle%).actualy = sprites(handle%).actualy + sprites(handle%).ydir ' update sprite y position
    sprites(handle%).currentx = INT(sprites(handle%).actualx) '                  this is where the sprite will show on screen
    sprites(handle%).currenty = INT(sprites(handle%).actualy) '                  this is where the sprite will show on screen

ELSE
    sprites(handle%).actualx = x! '                                             allows user to use small increments if desired
    sprites(handle%).actualy = y! '                                             allows user to use small increments if desired
    sprites(handle%).currentx = INT(x!) '                                       this is where the sprite will show on screen
    sprites(handle%).currenty = INT(y!) '                                       this is where the sprite will show on screen
END IF

IF NOT sprites(handle%).visible THEN EXIT SUB '                                 if sprite hidden no need to do rest of subroutine
IF sprites(handle%).animation THEN SPRITENEXT handle% '                         perform autoanimation if enabled

IF sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns = 0 THEN ' is sprite in rightmost column?
    cellx% = swidth% * (sheets(sprites(handle%).sheet).columns - 1) '            yes, upper left x position of sprite on sprite sheet
    celly% = ((sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) - 1) * sheight% ' upper left y position on sheet
ELSE '                                                                         sprite is not in rightmost column
    cellx% = (sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns - 1) * swidth% ' upper left x position of sprite on sheet
    celly% = (sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) * sheight% '      upper left y position of sprite on sheet
END IF
IF sprites(handle%).zoom <> 100 THEN '                                          does the sprite need to be zoomed in or out?
    swidth% = swidth% * (sprites(handle%).zoom / 100) '                         yes, calculate new sprite width
    sheight% = sheight% * (sprites(handle%).zoom / 100) '                       calculate new sprite height
END IF
tempsprite& = _NEWIMAGE(swidth%, sheight%, 32) '                               create temporary image holder for sprite
SELECT CASE sprites(handle%).flip '                                             should the image be flipped while copied?
    CASE 0 '                                                                   no flip, copy original sprite orientation
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly%)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 1 '                                                                   flip sprite horizontally while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)-(cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 2 '                                                                   flip sprite vertically while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)
    CASE 3 '                                                                   flip sprite both horizontally and vertically
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx%, celly%)
END SELECT
px!(0) = -swidth% / 2 '                                                        upper left  x polar coordinate of sprite
py!(0) = -sheight% / 2 '                                                       upper left  y polar coordinate of sprite
px!(1) = px!(0) '                                                              lower left  x polar coordinate of sprite
py!(1) = sheight% / 2 '                                                        lower left  y polar coordinate of sprite
px!(2) = swidth% / 2 '                                                         lower right x polar coordinate of sprite
py!(2) = py!(1) '                                                              lower right y polar coordinate of sprite
px!(3) = px!(2) '                                                              upper right x polar coordinate of sprite
py!(3) = py!(0) '                                                              upper right y polar coordinate of sprite


IF sprites(handle%).motion AND sprites(handle%).spindir <> 0 THEN
    sprites(handle%).rotation = sprites(handle%).rotation + sprites(handle%).spindir
    IF sprites(handle%).rotation < 0 THEN sprites(handle%).rotation = sprites(handle%).rotation + 360
    IF sprites(handle%).rotation >= 360 THEN sprites(handle%).rotation = sprites(handle%).rotation - 360
END IF


IF sprites(handle%).rotation <> 0 THEN '                                        does the sprite need to be rotated?
    sinr! = SIN(-sprites(handle%).rotation / 57.2957795131) '                   yes, some magic math for rotation
    cosr! = COS(-sprites(handle%).rotation / 57.2957795131) '                   some more magic math for rotation
    bx1% = 0 '                                                                 upper left  x coordinate of background
    by1% = 0 '                                                                 upper left  y coordinate of background
    bx2% = 0 '                                                                 lower right x coordinate of background
    by2% = 0 '                                                                 lower right y coordinate of background
    FOR count% = 0 TO 3 '                                                      cycle through all four polar coordinates
        x2& = (px!(count%) * cosr! + sinr! * py!(count%)) '                    compute new polar coordinate location
        y2& = (py!(count%) * cosr! - px!(count%) * sinr!) '                    compute new polar coordinate location
        px!(count%) = x2& '                                                    save the new polar coordinate
        py!(count%) = y2& '                                                    save the new polar coordinate
        IF px!(count%) < bx1% THEN bx1% = px!(count%) '                        save lowest  x value seen \
        IF px!(count%) > bx2% THEN bx2% = px!(count%) '                        save highest x value seen  \ background image
        IF py!(count%) < by1% THEN by1% = py!(count%) '                        save lowest  y value seen  / rectangle coordinates
        IF py!(count%) > by2% THEN by2% = py!(count%) '                        save highest y value seen /
    NEXT count%
    IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '             should the sprite save the background?
        sprites(handle%).background = _NEWIMAGE((bx2% - bx1%) + 1, (by2% - by1%) + 1, 32) ' yes, compute the background image
        _PUTIMAGE , _DEST, sprites(handle%).background, (sprites(handle%).currentx + bx1% + (swidth% / 2), sprites(handle%).currenty + by1% + (sheight% / 2))-(sprites(handle%).currentx + bx2% + (swidth% / 2), sprites(handle%).currenty + by2% + (sheight% / 2))
        sprites(handle%).backx = sprites(handle%).currentx + bx1% ' save the background's x location
        sprites(handle%).backy = sprites(handle%).currenty + by1% ' save the background's y location
    END IF
    sprites(handle%).currentwidth = bx2% - bx1% + 1 '                           save width of sprite after zoom and rotation
    sprites(handle%).currentheight = by2% - by1% + 1 '                          save height of sprite after zoom and rotation
    sprites(handle%).screenx1 = sprites(handle%).currentx + bx1% '               save upper left x position of sprite on screen
    sprites(handle%).screeny1 = sprites(handle%).currenty + by1% '               save upper left y potition of sprite on screen
    sprites(handle%).screenx2 = sprites(handle%).currentx + bx2% '               save lower right x position of sprite on screen
    sprites(handle%).screeny2 = sprites(handle%).currenty + by2% '               save lower right y potition of sprite on screen
    _MAPTRIANGLE (0, 0)-(0, sheight% - 1)-(swidth% - 1, sheight% - 1), tempsprite& TO(sprites(handle%).currentx + px!(0), sprites(handle%).currenty + py!(0))-(sprites(handle%).currentx + px!(1), sprites(handle%).currenty + py!(1))-(sprites(handle%).currentx + px!(2), sprites(handle%).currenty + py!(2))
    _MAPTRIANGLE (0, 0)-(swidth% - 1, 0)-(swidth% - 1, sheight% - 1), tempsprite& TO(sprites(handle%).currentx + px!(0), sprites(handle%).currenty + py!(0))-(sprites(handle%).currentx + px!(3), sprites(handle%).currenty + py!(3))-(sprites(handle%).currentx + px!(2), sprites(handle%).currenty + py!(2))
    IF sprites(handle%).detecttype = 2 THEN '                                   does sprite use pixel accuracy collision detection?
        _FREEIMAGE sprites(handle%).image '                                     yes, get rid of the last image save
        sprites(handle%).image = _NEWIMAGE(sprites(handle%).currentwidth, sprites(handle%).currentheight, 32) ' create a new image holder and map triangles in
        cx% = sprites(handle%).currentwidth / 2
        cy% = sprites(handle%).currentheight / 2
        _MAPTRIANGLE (0, 0)-(0, sheight% - 1)-(swidth% - 1, sheight% - 1), tempsprite& TO(cx% + px!(0), cy% + py!(0))-(cx% + px!(1), cy% + py!(1))-(cx% + px!(2), cy% + py!(2)), sprites(handle%).image
        _MAPTRIANGLE (0, 0)-(swidth% - 1, 0)-(swidth% - 1, sheight% - 1), tempsprite& TO(cx% + px!(0), cy% + py!(0))-(cx% + px!(3), cy% + py!(3))-(cx% + px!(2), cy% + py!(2)), sprites(handle%).image
    END IF
ELSE '                                                                         no rotation was needed, just place image on screen
    IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '             should the sprite save the background?
        sprites(handle%).background = _NEWIMAGE(INT(px!(2)) - INT(px!(0)), INT(py!(2)) - INT(py!(0)), 32) ' yes, compute the background image
        _PUTIMAGE , _DEST, sprites(handle%).background, (sprites(handle%).currentx + INT(px!(0)) + (swidth% / 2), sprites(handle%).currenty + INT(py!(0)) + (sheight% / 2))-(sprites(handle%).currentx + INT(px!(2)) - 1 + (swidth% / 2), sprites(handle%).currenty + INT(py!(2)) - 1 + (sheight% / 2))
        sprites(handle%).backx = sprites(handle%).currentx + INT(px!(0)) '       save the background's x location
        sprites(handle%).backy = sprites(handle%).currenty + INT(py!(0)) '       save the background's y location
    END IF
    sprites(handle%).currentwidth = INT(px!(2)) - INT(px!(0)) '                 save width of sprite after zoom
    sprites(handle%).currentheight = INT(py!(2)) - INT(py!(0)) '                save height of sprite after zoom
    sprites(handle%).screenx1 = sprites(handle%).currentx + INT(px!(0)) '        save upper left x of sprite on screen
    sprites(handle%).screeny1 = sprites(handle%).currenty + INT(py!(0)) '        save upper left y of sprite on screen
    sprites(handle%).screenx2 = sprites(handle%).currentx + INT(px!(2)) - 1 '    save lower right x of sprite onscreen
    sprites(handle%).screeny2 = sprites(handle%).currenty + INT(py!(2)) - 1 '    save lower right y of sprite onscreen

    _PUTIMAGE (sprites(handle%).screenx1, sprites(handle%).screeny1), tempsprite& ' copy temporary sprite image to the screen

    IF sprites(handle%).detecttype = 2 THEN '                                   does sprite use pixel accuracy collision detection?
        _FREEIMAGE sprites(handle%).image '                                     yes, get rid of the last image saved
        sprites(handle%).image = _COPYIMAGE(tempsprite&) '                      save a copy of the current sprite image
    END IF
END IF
_FREEIMAGE tempsprite& '                                                       temporary image holder no longer needed

END SUB

'##################################################################################################################################

SUB SPRITESTRETCH (x!, y!, x2!, y2!, handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Places a sprite on screen at coordinates INT(x!), INT(y!).                 |                     - NOTES -                     |
'|                                                                            | - The x! and y! values can be sent in as integers |
'| Usage : SPRITEPUT 10, 10, mysprite%                                        |   or single precision to allow the programmer to  |
'|                                                                            |   use fine or pixel by pixel movements.           |
'| Input : x!      - row to display sprite at.                                |                                                   |
'|         y!      - column to display sprite at.                             |                                                   |
'|         handle% - the sprite to display on the screen.                     |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().onscreen to -1 if not previously on screen.               |                                                   |
'|         sprite().actualx to equal x! variable passed in.                   |                                                   |
'|         sprite().actualy to equal y! variable passed in.                   |                                                   |
'|         sprite().currentx to equal INT(x!) variable passed in.             |                                                   |
'|         sprite().currenty to equal INT(y!) variable passed in.             |                                                   |
'|         sprite().currentwidth to equal the screen width of sprite.         |                                                   |
'|         sprite().currentheight to equal the screen height of sprite.       |                                                   |
'|         sprite().background to hold the image of the background if needed. |                                                   |
'|         sprite().screenx1 to upper left x of sprite on screen. *           |                                                   |
'|         sprite().screeny1 to upper left y of sprite on screen. *           |                                                   |
'|         sprite().screenx2 to lower right x of sprite on screen.            |                                                   |
'|         sprite().screeny2 to lower right y of sprite on screen.            |                                                   |
'|         sprite().backx to upper left x of sprite on screen. *              |                                                   |
'|         sprite().backy to upper left y of sprite on screen. *              |                                                   |
'|         * = redundant?                                                     |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: Portions of this subroutine have code based off of Galleon's RotoZoom subroutine found in the QB64 documentation at   |
'|          http://qb64.net/wiki/index.php?title=MAPTRIANGLE                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sheets() AS SHEET '                                                      array defining sprite sheets
SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM tempsprite& '                                                              temporary holding image for sprite from sheet
DIM cellx% '                                                                   the upper left x location of sprite on sheet
DIM celly% '                                                                   the upper right y location of sprite on sheet
DIM px!(0 TO 3) '                                                                   polar x coordinates of maptriangle
DIM py!(0 TO 3) '                                                                   polar y coordinates of maptriangle
DIM sinr! '                                                                    the sine function used on rotation
DIM cosr! '                                                                    the cosine function used on rotation
DIM count% '                                                                   a generic counter used in subroutine
DIM x2& '                                                                      temp variable used when computing polar coordinates
DIM y2& '                                                                      temp variable used when computing polar coordinates
DIM swidth% '                                                                  the width of the sprite on the sprite sheet
DIM sheight% '                                                                 the height of the sprite on the sprite sheet
DIM bx1% '                                                                     upper left x location of background image
DIM by1% '                                                                     upper left y location of background image
DIM bx2% '                                                                     lower right x location of background image
DIM by2% '                                                                     lower right y location of background image
DIM cx% '                                                                      used to center pixel accurate collision image
DIM cy% '                                                                      used to center pixel accurate collision image

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEPUT", 100 : EXIT SUB '                is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEPUT", 101 : EXIT SUB '               is this sprite handle in use?
IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '                 is sprite on screen and should it restore background
    _PUTIMAGE (sprites(handle%).backx, sprites(handle%).backy), sprites(handle%).background
    _FREEIMAGE sprites(handle%).background '                                    background image no longer needed
ELSEIF (NOT sprites(handle%).onscreen) AND sprites(handle%).visible THEN '       hAS SPRITE been made visible again or first time?
    sprites(handle%).onscreen = -1 '                                            sprite will be placed on the screen
END IF

IF sprites(handle%).motion THEN '                                               is sprite automotion enabled?
    sprites(handle%).actualx = sprites(handle%).actualx + sprites(handle%).xdir ' yes, update sprite x position
    sprites(handle%).actualy = sprites(handle%).actualy + sprites(handle%).ydir ' update sprite y position
    sprites(handle%).currentx = INT(sprites(handle%).actualx) '                  this is where the sprite will show on screen
    sprites(handle%).currenty = INT(sprites(handle%).actualy) '                  this is where the sprite will show on screen
ELSE
    sprites(handle%).actualx = x! '                                             allows user to use small increments if desired
    sprites(handle%).actualy = y! '                                             allows user to use small increments if desired
    sprites(handle%).currentx = INT(x!) '                                       this is where the sprite will show on screen
    sprites(handle%).currenty = INT(y!) '                                       this is where the sprite will show on screen
END IF

IF NOT sprites(handle%).visible THEN EXIT SUB '                                 if sprite hidden no need to do rest of subroutine
IF sprites(handle%).animation THEN SPRITENEXT handle% '                         perform autoanimation if enabled
swidth% = sheets(sprites(handle%).sheet).spritewidth '                           width of sprite
sheight% = sheets(sprites(handle%).sheet).spriteheight '                         height of sprite

IF x2! = 0 THEN x2! = x! + swidth%
IF y2! = 0 THEN y2! = y! + sheight%

IF sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns = 0 THEN ' is sprite in rightmost column?
    cellx% = swidth% * (sheets(sprites(handle%).sheet).columns - 1) '            yes, upper left x position of sprite on sprite sheet
    celly% = ((sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) - 1) * sheight% ' upper left y position on sheet
ELSE '                                                                         sprite is not in rightmost column
    cellx% = (sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns - 1) * swidth% ' upper left x position of sprite on sheet
    celly% = (sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) * sheight% '      upper left y position of sprite on sheet
END IF
IF sprites(handle%).zoom <> 100 THEN '                                          does the sprite need to be zoomed in or out?
    swidth% = swidth% * (sprites(handle%).zoom / 100) '                         yes, calculate new sprite width
    sheight% = sheight% * (sprites(handle%).zoom / 100) '                       calculate new sprite height
END IF
tempsprite& = _NEWIMAGE(swidth%, sheight%, 32) '                               create temporary image holder for sprite
SELECT CASE sprites(handle%).flip '                                             should the image be flipped while copied?
    CASE 0 '                                                                   no flip, copy original sprite orientation
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly%)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 1 '                                                                   flip sprite horizontally while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)-(cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 2 '                                                                   flip sprite vertically while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)
    CASE 3 '                                                                   flip sprite both horizontally and vertically
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx%, celly%)
END SELECT
px!(0) = -swidth% / 2 '                                                        upper left  x polar coordinate of sprite
py!(0) = -sheight% / 2 '                                                       upper left  y polar coordinate of sprite
px!(1) = px!(0) '                                                              lower left  x polar coordinate of sprite
py!(1) = sheight% / 2 '                                                        lower left  y polar coordinate of sprite
px!(2) = swidth% / 2 '                                                         lower right x polar coordinate of sprite
py!(2) = py!(1) '                                                              lower right y polar coordinate of sprite
px!(3) = px!(2) '                                                              upper right x polar coordinate of sprite
py!(3) = py!(0) '                                                              upper right y polar coordinate of sprite


IF sprites(handle%).motion AND sprites(handle%).spindir <> 0 THEN
    sprites(handle%).rotation = sprites(handle%).rotation + sprites(handle%).spindir
    IF sprites(handle%).rotation < 0 THEN sprites(handle%).rotation = sprites(handle%).rotation + 360
    IF sprites(handle%).rotation >= 360 THEN sprites(handle%).rotation = sprites(handle%).rotation - 360
END IF


IF sprites(handle%).rotation <> 0 THEN EXIT SUB'                                        does the sprite need to be rotated?
IF sprites(handle%).onscreen AND sprites(handle%).restore THEN '             should the sprite save the background?
    sprites(handle%).background = _NEWIMAGE(INT(px!(2)) - INT(px!(0)), INT(py!(2)) - INT(py!(0)), 32) ' yes, compute the background image
    _PUTIMAGE , _DEST, sprites(handle%).background, (sprites(handle%).currentx + INT(px!(0)), sprites(handle%).currenty + INT(py!(0)))-(sprites(handle%).currentx + INT(px!(2)) - 1, sprites(handle%).currenty + INT(py!(2)) - 1)
    sprites(handle%).backx = sprites(handle%).currentx + INT(px!(0)) '       save the background's x location
    sprites(handle%).backy = sprites(handle%).currenty + INT(py!(0)) '       save the background's y location
END IF
sprites(handle%).currentwidth = INT(px!(2)) - INT(px!(0)) '                 save width of sprite after zoom
sprites(handle%).currentheight = INT(py!(2)) - INT(py!(0)) '                save height of sprite after zoom
sprites(handle%).screenx1 = sprites(handle%).currentx + INT(px!(0)) '        save upper left x of sprite on screen
sprites(handle%).screeny1 = sprites(handle%).currenty + INT(py!(0)) '        save upper left y of sprite on screen
sprites(handle%).screenx2 = sprites(handle%).currentx + INT(px!(2)) - 1 '    save lower right x of sprite onscreen
sprites(handle%).screeny2 = sprites(handle%).currenty + INT(py!(2)) - 1 '    save lower right y of sprite onscreen

_PUTIMAGE (x!, y!)-(x2!, y2!), tempsprite& ' copy temporary sprite image to the screen
IF sprites(handle%).detecttype = 2 THEN '                                   does sprite use pixel accuracy collision detection?
    _FREEIMAGE sprites(handle%).image '                                     yes, get rid of the last image saved
    sprites(handle%).image = _COPYIMAGE(tempsprite&) '                      save a copy of the current sprite image
END IF
_FREEIMAGE tempsprite& '                                                       temporary image holder no longer needed

END SUB

'##################################################################################################################################

FUNCTION SPRITEY2 (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the lower right y screen position of the sprite.                   |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : y2% = SPRITEY2(mysprite%)                                          |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to get the lower right y screen coordinate.   | - The y value passed back is the lower right y    |
'|                                                                            |   screen position of the bounding box of the      |
'| Output: an integer value containing the lower right y screen coordinate.   |   sprite.                                         |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEY2", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEY2", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEY2 = sprites(handle%).screeny2 '                                          return the lower right y coordinate of sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEX2 (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the lower right x screen position of the sprite.                   |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : x2% = SPRITEX2(mysprite%)                                          |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to get the lower right x screen coordinate.   | - The x value passed back is the lower right x    |
'|                                                                            |   screen position of the bounding box of the      |
'| Output: an integer value containing the lower right x screen coordinate.   |   sprite.                                         |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEX2", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEX2", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEX2 = sprites(handle%).screenx2 '                                          return the lower right x coordinate of sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEY1 (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the upper left y screen position of the sprite.                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : y1% = SPRITEY1(mysprite%)                                          |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to get the upper left y screen coordinate.    | - The y value passed back is the upper left y     |
'|                                                                            |   screen position of the bounding box of the      |
'| Output: an integer value containing the upper left y screen coordinate.    |   sprite.                                         |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEY1", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEY1", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEY1 = sprites(handle%).screeny1 '                                          return the upper left y coordinate of sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEX1 (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the upper left x screen position of the sprite.                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : x1% = SPRITEX1(mysprite%)                                          |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to get the upper left x screen coordinate.    | - The x value passed back is the upper left x     |
'|                                                                            |   screen position of the bounding box of the      |
'| Output: an integer value containing the upper left x screen coordinate.    |   sprite.                                         |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEX1", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEX1", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEX1 = sprites(handle%).screenx1 '                                          return the upper left x coordinate of sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEMOUSEAY (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the y location of the mouse on the screen.                         |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : yonscreen% = SPRITEMOUSEAY(mysprite%)                              |   result in the function reporting an error and   |
'|                                                                            |   halting program execution                       |
'| Input : handle% - the sprite to get the mouse's y screen position from.    | - The y location returned is the coordinate on    |
'|                                                                            |   on the screen itself.                           |
'| Output: an integer value containing the mouse's y screen position.         |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOUSEAY", 100 : EXIT FUNCTION '            is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOUSEAY", 101 : EXIT FUNCTION '           is this sprite handle in use?
SPRITEMOUSEAY = sprites(handle%).mouseay '                                      report the y location of pointer on screen

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEMOUSEAX (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the x location of the mouse on the screen.                         |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : xonscreen% = SPRITEMOUSEAX(mysprite%)                              |   result in the function reporting an error and   |
'|                                                                            |   halting program execution                       |
'| Input : handle% - the sprite to get the mouse's x screen position from.    | - The x location returned is the coordinate on    |
'|                                                                            |   on the screen itself.                           |
'| Output: an integer value containing the mouse's x screen position.         |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOUSEAX", 100 : EXIT FUNCTION '            is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOUSEAX", 101 : EXIT FUNCTION '           is this sprite handle in use?
SPRITEMOUSEAX = sprites(handle%).mouseax '                                      report the x location of pointer on screen

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEMOUSEY (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the y location of the mouse on the sprite itself.                  |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : yonsprite% = SPRITEMOUSEY(mysprite%)                               |   result in the function reporting an error and   |
'|                                                                            |   halting program execution                       |
'| Input : handle% - the sprite to get the mouse's y position from.           | - The y location returned is the coordinate on    |
'|                                                                            |   on the sprite itself. A 50x50 sprite would      |
'| Output: an integer value containing the mouse's y poisition on the sprite. |   return a y value of 0 through 49.               |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOUSEY", 100 : EXIT FUNCTION '             is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOUSEY", 101 : EXIT FUNCTION '            is this sprite handle in use?
SPRITEMOUSEY = sprites(handle%).mousecy '                                       report the y location of pointer on sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEMOUSEX (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the x location of the mouse on the sprite itself.                  |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : xonsprite% = SPRITEMOUSEX(mysprite%)                               |   result in the function reporting an error and   |
'|                                                                            |   halting program execution                       |
'| Input : handle% - the sprite to get the mouse's x position from.           | - The x location returned is the coordinate on    |
'|                                                                            |   on the sprite itself. A 50x50 sprite would      |
'| Output: an integer value containing the mouse's x position on the sprite.  |   return an x value of 0 through 49.              |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOUSEX", 100 : EXIT FUNCTION '             is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOUSEX", 101 : EXIT FUNCTION '            is this sprite handle in use?
SPRITEMOUSEX = sprites(handle%).mousecx '                                       report the x location of pointer on sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEMOUSE (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the status of the current sprite and mouse pointer interaction.    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : interaction% = SPRITEMOUSE(mysprite%)                              |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to check for mouse interaction with.          | - If a mouse event is detected the event is also  |
'|                                                                            |   recorded in sprite().pointer and the            |
'| Output: integer value between 0 and 3 indicating the type of interaction.  |   actual mouse x,y screen location, as well as    |
'|         0 = no mouse interaction.       (constant NOMOUSE)                 |   the mouse x,y sprite location, is recorded.     |
'|         1 = left mouse button clicked.  (constant MOUSELEFT)               | - The constants NOMOUSE, MOUSELEFT, MOUSERIGHT    |
'|         2 = right mouse button clicked. (constant MOUSERIGHT)              |   and MOUSEHOVER have been created to be used     |
'|         3 = mouse pointer is hovering.  (constant MOUSEHOVER)              |   with this function.                             |
'|                                                                            | - The global constant NOVALUE can be used to      |
'| Sets  : sprite().pointer  = event number (0-3).                            |   check for valid mouse x,y actual screen         |
'|         sprite().mouseax  = actual x location of mouse on screen.          |   locations and valid mouse x,y sprite locations. |
'|                          -32767 = no interaction (constant NOVALUE)        |                                                   |
'|         sprite().mouseay  = actual y location of mouse on screen.          |                                                   |
'|                          -32767 = no interaction (constant NOVALUE)        |                                                   |
'|         sprite().mousecx = x location of mouse on sprite.                  |                                                   |
'|                          -32767 = no interaction (constant NOVALUE)        |                                                   |
'|         sprite().mousecy = y location of mouse on sprite.                  |                                                   |
'|                          =32767 = no interaction (constant NOVALUE)        |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM event% '                                                                   event currently interacting between mouse and sprite

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEMOUSE", 100 : EXIT FUNCTION '              is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEMOUSE", 101 : EXIT FUNCTION '             is this sprite handle in use?
DO: LOOP WHILE _MOUSEINPUT '                                                   get the most up to date mouse event
SPRITEMOUSE = 0 '                                                              report no sprite and mouse interaction by default
event% = 3 '                                                                   assume mouse in currenly hovering over sprite
IF _MOUSEBUTTON(1) THEN event% = 1 '                                           if left button pressed then set the event
IF _MOUSEBUTTON(2) THEN event% = 2 '                                           if the right button pressed then set the event
IF sprites(handle%).onscreen THEN '                                             is the sprite currently on the screen?
    IF (_MOUSEX >= sprites(handle%).screenx1) AND (_MOUSEX <= sprites(handle%).screenx2) AND (_MOUSEY >= sprites(handle%).screeny1) AND (_MOUSEY <= sprites(handle%).screeny2) THEN
        sprites(handle%).pointer = event% '                                     mouse pointer is over sprite, set the event
        SPRITEMOUSE = event% '                                                 report the event number
    ELSE '                                                                     mouse pointer is not currently over sprite
        sprites(handle%).pointer = 0 '                                          set event as no mouse interaction
        SPRITEMOUSE = 0 '                                                      report the event number
        event% = 0 '                                                           set the event as no interaction
    END IF
    IF event% <> 0 THEN '                                                      was there mouse interaction with this sprite?
        sprites(handle%).mouseax = _MOUSEX '                                    yes, save the actual screen x location of pointer
        sprites(handle%).mouseay = _MOUSEY '                                    save the actual screen y location of pointer
        sprites(handle%).mousecx = _MOUSEX - sprites(handle%).screenx1 '         save the pointer x location on sprite itself
        sprites(handle%).mousecy = _MOUSEY - sprites(handle%).screeny1 '         save the pointer y location on sprite itself
    ELSE '                                                                     there is no mouse interaction with sprite
        sprites(handle%).mouseax = -32767 '                                     set the mouse x value as having no interaction
        sprites(handle%).mouseay = -32767 '                                     set the mouse y value as having no interaction
        sprites(handle%).mousecx = -32767 '                                     set the mouse x location on sprite as no interaction
        sprites(handle%).mousecy = -32767 '                                     set the mouse y location on sprite as no interaction
    END IF
END IF

END FUNCTION

'##################################################################################################################################

SUB SPRITEFREE (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Removes a sprite from memory, freeing its resources.                       |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEFREE mysprite%                                               |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite number to be freed from memory.               | - If the sprite is on the screen when freed the   |
'|                                                                            |   background image will be restored before the    |
'| Output: none                                                               |   sprite is freed from memory.                    |
'|                                                                            |                                                   |
'| Sets  : sprite().inuse to equal 0 if the element is not last in array.     |                                                   |
'|         sprite().background image freed if element contained one.          |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITEFREE handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEFREE", 100 : EXIT SUB '               is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEFREE", 101 : EXIT SUB '              is this sprite handle in use?
        IF sprites(handle%).restore THEN '                                              has this sprite been saving the background?
            IF sprites(handle%).onscreen THEN '                                         yes, is the sprite onscreen?
                _PUTIMAGE (sprites(handle%).backx, sprites(handle%).backy), sprites(handle%).background ' restore background
            END IF
            _FREEIMAGE sprites(handle%).background '                                    free the sprite's background image
        END IF
        IF handle% = UBOUND(sprites) AND handle% <> 1 THEN '                            is this the last element in the array?
            REDIM _PRESERVE sprites(0 TO handle% - 1) AS SPRITE '                            yes, resize the array, removing the element
        ELSE '                                                                         this is not the last element in the array
            sprites(handle%).inuse = 0 '                                                mark the array entry as not in use
        END IF
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITESTAMP (x%, y%, handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Places a sprite on the background as if using a sprite stamp pad.          |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITESTAMP 100, 100, mysprite%                                    |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : x%      - the x location to place stamp.                           | - The sprite will be stamped onto the background  |
'|         y%      - the y location to place stamp.                           |   with all current aspects (orientation and zoom) |
'|         handle% - the sprite image to use as a stamp.                      |   that have been set for the sprite being stamped.|
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: Portions of this subroutine have code based off of Galleon's RotoZoom subroutine found in the QB64 documentation at   |
'|          http://qb64.net/wiki/index.php?title=MAPTRIANGLE                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites
SHARED sheets() AS SHEET '                                                      array defining sprite sheets

DIM tempsprite& '                                                              temporary holding image for sprite from sheet
DIM cellx% '                                                                   the upper left x location of sprite on sheet
DIM celly% '                                                                   the upper right y location of sprite on sheet
DIM px!(0 TO 3) '                                                                   polar x coordinates of maptriangle
DIM py!(0 TO 3) '                                                                   polar y coordinates of maptriangle
DIM sinr! '                                                                    the sine function used on rotation
DIM cosr! '                                                                    the cosine function used on rotation
DIM count% '                                                                   a generic counter used in subroutine
DIM x2& '                                                                      temp variable used when computing polar coordinates
DIM y2& '                                                                      temp variable used when computing polar coordinates
DIM swidth% '                                                                  the width of the stamp on the sprite sheet
DIM sheight% '                                                                 the height of the stamp on the sprite sheet

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESTAMP", 100 : EXIT SUB '              is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESTAMP", 101 : EXIT SUB '             is this sprite handle in use?
swidth% = sheets(sprites(handle%).sheet).spritewidth '                           width of stamp
sheight% = sheets(sprites(handle%).sheet).spriteheight '                         height of stamp
IF sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns = 0 THEN ' is stamp in rightmost column?
    cellx% = swidth% * (sheets(sprites(handle%).sheet).columns - 1) '            yes, upper left x position of stamp on sprite sheet
    celly% = ((sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) - 1) * sheight% ' upper left y position on sheet
ELSE '                                                                         stamp is not in rightmost column
    cellx% = (sprites(handle%).currentcell MOD sheets(sprites(handle%).sheet).columns - 1) * swidth% ' upper left x position of stamp on sheet
    celly% = (sprites(handle%).currentcell \ sheets(sprites(handle%).sheet).columns) * sheight% '      upper left y position of stamp on sheet
END IF
IF sprites(handle%).zoom <> 100 THEN '                                          does the stamp need to be zoomed in or out?
    swidth% = swidth% * (sprites(handle%).zoom / 100) '                         yes, calculate new stamp width
    sheight% = sheight% * (sprites(handle%).zoom / 100) '                       calculate new stamp height
END IF
tempsprite& = _NEWIMAGE(swidth%, sheight%, 32) '                               create temporary image holder for the stamp
SELECT CASE sprites(handle%).flip '                                             should the image be flipped while copied?
    CASE 0 '                                                                   no flip, copy original stamp orientation
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly%)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 1 '                                                                   flip stamp horizontally while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)-(cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)
    CASE 2 '                                                                   flip stamp vertically while copying it
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx%, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly%)
    CASE 3 '                                                                   flip stamp both horizontally and vertically
        _PUTIMAGE , sheets(sprites(handle%).sheet).sheetimage, tempsprite&, (cellx% + sheets(sprites(handle%).sheet).spritewidth - 1, celly% + sheets(sprites(handle%).sheet).spriteheight - 1)-(cellx%, celly%)
END SELECT
px!(0) = -swidth% / 2 '                                                        upper left  x polar coordinate of stamp
py!(0) = -sheight% / 2 '                                                       upper left  y polar coordinate of stamp
px!(1) = px!(0) '                                                              lower left  x polar coordinate of stamp
py!(1) = sheight% / 2 '                                                        lower left  y polar coordinate of stamp
px!(2) = swidth% / 2 '                                                         lower right x polar coordinate of stamp
py!(2) = py!(1) '                                                              lower right y polar coordinate of stamp
px!(3) = px!(2) '                                                              upper right x polar coordinate of stamp
py!(3) = py!(0) '                                                              upper right y polar coordinate of stamp
IF sprites(handle%).rotation <> 0 THEN '                                        does the stamp need to be rotated?
    sinr! = SIN(-sprites(handle%).rotation / 57.2957795131) '                   yes, some magic math for rotation
    cosr! = COS(-sprites(handle%).rotation / 57.2957795131) '                   some more magic math for rotation
    FOR count% = 0 TO 3 '                                                      cycle through all four polar coordinates
        x2& = (px!(count%) * cosr! + sinr! * py!(count%)) '                    compute new polar coordinate location
        y2& = (py!(count%) * cosr! - px!(count%) * sinr!) '                    compute new polar coordinate location
        px!(count%) = x2& '                                                    save the new polar coordinate
        py!(count%) = y2& '                                                    save the new polar coordinate
    NEXT count%
    _MAPTRIANGLE (0, 0)-(0, sheight% - 1)-(swidth% - 1, sheight% - 1), tempsprite& TO(x% + px!(0), y% + py!(0))-(x% + px!(1), y% + py!(1))-(x% + px!(2), y% + py!(2))
    _MAPTRIANGLE (0, 0)-(swidth% - 1, 0)-(swidth% - 1, sheight% - 1), tempsprite& TO(x% + px!(0), y% + py!(0))-(x% + px!(3), y% + py!(3))-(x% + px!(2), y% + py!(2))
ELSE '                                                                         no rotation was needed, just place stamp on screen
'IF absolute THEN
    _PUTIMAGE (x% + INT(px!(0)) + (swidth% / 2), y% + INT(py!(0)) + (sheight% / 2)), tempsprite& '              stamp temporary sprite image to the screen
'ELSE
'    _PUTIMAGE (x% + INT(px!(0)), y% + INT(py!(0))), tempsprite& '              stamp temporary sprite image to the screen
'END IF
END IF
_FREEIMAGE tempsprite& '                                                       temporary image holder no longer needed

END SUB

'##################################################################################################################################

FUNCTION SPRITECOPY (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Makes a copy of a sprite and returns the newly created sprite's handle.    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : newsprite% = SPRITECOPY(mysprite%)                                 |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite being copied.                                 | - All aspects of the sprite being copied (auto-   |
'|                                                                            |   motion, autoanimation, location, orientation,   |
'| Output: an integer greater than 0 indicating the new sprite's handle.      |   etc.. will be retained by the new sprite.       |
'|                                                                            |                                                   |
'| Sets  : sprite().* all variables of new sprite by virtue of copying values |                                                   |
'|         from original sprite().*                                           |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM newhandle% '                                                               the handle number fo the newly created sprite

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECOPY", 100 : EXIT FUNCTION '               is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECOPY", 101 : EXIT FUNCTION '              is this sprite handle in use?
newhandle% = 0 '                                                               initialize handle value
DO '                                                                           look for the next available handle
    newhandle% = newhandle% + 1 '                                              increment the handle value
LOOP UNTIL (NOT sprites(newhandle%).inuse) OR newhandle% = UBOUND(sprites) '     stop looking when valid handle value found
IF sprites(newhandle%).inuse THEN '                                             is the last array element in use?
    newhandle% = newhandle% + 1 '                                              yes, increment the handle value
    REDIM _PRESERVE sprites(0 TO newhandle%) AS SPRITE '                             increase the size of sprite array
END IF
sprites(newhandle%) = sprites(handle%) '                                         copy the sprite
SPRITECOPY = newhandle% '                                                      report back with the new sprite handle

END FUNCTION

'##################################################################################################################################

SUB SPRITEPREVIOUS (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Go to previous cell of sprite's animation sequence.                        |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEPREVIOUS mysprite%                                           |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the number of the sprite to advance animation cell.      |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().currentcell to the previous animation cell based on the   |                                                   |
'|         saved sprite().animtype.                                           |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITEPREVIOUS handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEPREVIOUS", 100 : EXIT SUB '           is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEPREVIOUS", 101 : EXIT SUB '          is this sprite handle in use?
        SELECT CASE sprites(handle%).animtype '                                         which type of animation behavior should be used?
            CASE 0 '                                                                   forward looping animation
                sprites(handle%).currentcell = sprites(handle%).currentcell - 1 '        select previous sprite sheet cell
                IF sprites(handle%).currentcell < sprites(handle%).animstart THEN '      does cell go beyond the minimum cell allowed?
                    sprites(handle%).currentcell = sprites(handle%).animend '            yes, reset the cell back to end of animation
                END IF
            CASE 1 '                                                                   backward looping animation
                sprites(handle%).currentcell = sprites(handle%).currentcell + 1 '        select next sprite sheet cell
                IF sprites(handle%).currentcell > sprites(handle%).animend THEN '        does the cell go beyond the maximum cell allowed?
                    sprites(handle%).currentcell = sprites(handle%).animstart '          yes, reset the cell back to beginning of animation
                END IF
            CASE 2 '                                                                   forward/backward looping animation
                sprites(handle%).animdir = -sprites(handle%).animdir '                   temporarily set opposite looping direction
                IF (sprites(handle%).currentcell + sprites(handle%).animdir < sprites(handle%).animstart) OR (sprites(handle%).currentcell + sprites(handle%).animdir > sprites(handle%).animend) THEN
                    sprites(handle%).animdir = -sprites(handle%).animdir '               minimum/maximum cell was reached, change direction
                END IF
                sprites(handle%).currentcell = sprites(handle%).currentcell + sprites(handle%).animdir ' select next/previous sheet cell
                sprites(handle%).animdir = -sprites(handle%).animdir '                   set looping direction back to what it was
        END SELECT
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITENEXT (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Go to next cell of sprite's animation sequence.                            |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITENEXT mysprite%                                               |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the number of the sprite to advance animation cell.      |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().currentcell to the next animation cell based on the saved |                                                   |
'|         sprite().animtype.                                                 |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITENEXT handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITENEXT", 100 : EXIT SUB '               is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITENEXT", 101 : EXIT SUB '              is this sprite handle in use?
        SELECT CASE sprites(handle%).animtype '                                         which type of animation behavior should be used?
            CASE 0 '                                                                   forward looping animation
                sprites(handle%).currentcell = sprites(handle%).currentcell + 1 '        select next sprite sheet cell
                IF sprites(handle%).currentcell > sprites(handle%).animend THEN '        does cell go beyond the maximum cell allowed?
                    sprites(handle%).currentcell = sprites(handle%).animstart '          yes, reset the cell back to beginning of animation
                END IF
            CASE 1 '                                                                   backward looping animation
                sprites(handle%).currentcell = sprites(handle%).currentcell - 1 '        select previous sprite sheet cell
                IF sprites(handle%).currentcell < sprites(handle%).animstart THEN '      does the cell go beyond the minimum cell allowed?
                    sprites(handle%).currentcell = sprites(handle%).animend '            yes, reset the cell back to end of animation
                END IF
            CASE 2 '                                                                   forward/backward looping animation
                IF (sprites(handle%).currentcell + sprites(handle%).animdir < sprites(handle%).animstart) OR (sprites(handle%).currentcell + sprites(handle%).animdir > sprites(handle%).animend) THEN
                    sprites(handle%).animdir = -sprites(handle%).animdir '               minimum/maximum cell was reached, change direction
                END IF
                sprites(handle%).currentcell = sprites(handle%).currentcell + sprites(handle%).animdir ' select next/previous sheet cell
        END SELECT
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITEANIMATION (handle%, onoff%, behavior%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Turns on or off automatic sprite animation with specified behavior.        |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEANIMATION mysprite%, -1, 0                                   |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%   - the number of the sprite to turn animation on or off.  | - The constants ANIMATE, NOANIMATE, FORWARDLOOP,  |
'|         onoff%    - enable or disable automatic sprite animation:          |   BACKWARDLOOP and BACKFORTHLOOP have been        |
'|                     0 = disable automatic animation (constant NOANIMATE)   |   created to be used with this subroutine.        |
'|                    -1 = enable automatic animation  (cosntant ANIMATE)     |                                                   |
'|         behavior% - the type of animation sequence desired:                |                                                   |
'|                     0 = forward loop               (constant FORWARDLOOP)  |                                                   |
'|                     1 = backward loop              (constant BACKWARDLOOP) |                                                   |
'|                     2 = forward then backward loop (constant BACKFORTHLOOP)|                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().animation to 0 or -1 based on variable onoff% passed in.  |                                                   |
'|         sprite().animtype to 0, 1 or 2 bassed on behavior% passed in.      |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITEANIMATION handle%, onoff%, behavior%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANIMATION", 100 : EXIT SUB '          is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEANIMATION", 101 : EXIT SUB '         is this sprite handle in use?
        sprites(handle%).animation = onoff% '                                           enable or disable automatic sprite animation
        sprites(handle%).animtype = behavior% '                                         set animation looping type behavior
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITEANIMATESET (handle%, startcell%, endcell%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets a sprite's animation sequence start and end sprite sheet cells.       |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEANIMATESET mysprite%, 10, 14                                 |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%    - the sprite number to set the animation sequence for.  |                                                   |
'|         startcell% - the sprite sheet cell number to start at.             |                                                   |
'|         endcell%   - the sprite sheet cell number to end at.               |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().animstart to equal the startcell number passed in.        |                                                   |
'|         sprite().animend to equal the endcell number passed in.            |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANIMATESET", 100 : EXIT SUB '         is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEANIMATESET", 101 : EXIT SUB '        is this sprite handle in use?
sprites(handle%).animstart = startcell% '                                       set sprite's starting animation cell
sprites(handle%).animend = endcell% '                                           set sprite's ending animation cell

END SUB

'##################################################################################################################################

SUB SPRITESET (handle%, cell%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Sets a sprite's image to a new image number on sprite sheet.               |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITESET mysprite%, 20                                            |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite number to change the image of.                |                                                   |
'|         cell%   - the cell number on the animation sheet to use.           |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().currentcell to equal the cell number passed in.           |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESET", 100 : EXIT SUB '                is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESET", 101 : EXIT SUB '               is this sprite handle in use?
sprites(handle%).currentcell = cell% '                                          set sprite's image to new sheet image number

END SUB

'##################################################################################################################################

FUNCTION SPRITEAY (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the actual y location of a sprite.                                 |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : y! = SPRITEAY(mysprite%)                                           |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the actual y location from.       |                                                   |
'|                                                                            |                                                   |
'| Output: a single value indicating the actual y position of the sprite.     |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEAY", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEAY", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEAY = sprites(handle%).actualy '                                           report back with sprite's actual y location

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEAX (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the actual x location of a sprite.                                 |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : x! = SPRITEAX(mysprite%)                                           |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the actual x location from.       |                                                   |
'|                                                                            |                                                   |
'| Output: a single value indicating the actual x position of the sprite.     |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEAX", 100 : EXIT FUNCTION '                 is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEAX", 101 : EXIT FUNCTION '                is this sprite handle in use?
SPRITEAX = sprites(handle%).actualx '                                           report back with sprite's screen x location

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEY (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the screen y location of a sprite.                                 |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : y% = SPRITEY(mysprite%)                                            |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the screen y location from.       |                                                   |
'|                                                                            |                                                   |
'| Output: an integer value indicating the y screen position of the sprite.   |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEY", 100 : EXIT FUNCTION '                  is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEY", 101 : EXIT FUNCTION '                 is this sprite handle in use?
SPRITEY = sprites(handle%).currenty '                                           report back with sprite's screen y location

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEX (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Returns the screen x location of a sprite.                                 |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : x% = SPRITEX(mysprite%)                                            |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to retrieve the screen x location from.       |                                                   |
'|                                                                            |                                                   |
'| Output: an integer value indicating the x screen position of the sprite.   |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEX", 100 : EXIT FUNCTION '                  is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEX", 101 : EXIT FUNCTION '                 is this sprite handle in use?
SPRITEX = sprites(handle%).currentx '                                           report back with sprite's screen x location

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITEROTATION (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Gets the current rotation angle of a sprite in degrees.                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : degrees! = SPRITEROTATION(mysprite%)                               |   result in the function reporting an error and   |
'|                                                                            |   halting program execution                       |
'| Input : handle% - the sprite to retrieve the rotation angle from.          |                                                   |
'|                                                                            |                                                   |
'| Output: a single value between 0 and 359.99..                              |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEROTATION", 100 : EXIT FUNCTION '           is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEROTATION", 101 : EXIT FUNCTION '          is this sprite handle in use?
SPRITEROTATION = sprites(handle%).rotation '                                    report back with sprite's rotation angle

END FUNCTION

'##################################################################################################################################

SUB SPRITESHOW (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Unhides a sprite from view.                                                |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITESHOW mysprite%                                               |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to make visible.                              |                                                   |
'|                                                                            |                                                   |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().visible to equal -1.                                      |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse THEN SPRITESHOW handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITESHOW", 100 : EXIT SUB '               is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITESHOW", 101 : EXIT SUB '              is this sprite handle in use?
        IF NOT sprites(handle%).visible THEN '                                          is sprite currently hidden?
            sprites(handle%).visible = -1 '                                             yes, set sprite as being visible
            'SPRITEPUT sprites(handle%).currentx, sprites(handle%).currenty, handle% '    put sprite back on screen
        END IF
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITEHIDE (handle%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Hides a sprite from view.                                                  |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEHIDE mysprite%                                               |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to make invisible.                            | - Only a sprite that saves its background can be  |
'|                                                                            |   hidden. Specifying a sprite that is not saving  |
'| Output: none                                                               |   the background will result in the subroutine    |
'|                                                                            |   reporting an error and halting program          |
'| Sets  : sprite().visible to equal 0. *                                     |   execution.                                      |
'|         sprite().onscreen equal to 0 if it was showing on screen. *        |                                                   |
'|          * redundant?                                                      |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites
SHARED sheets() AS SHEET '                                                    array defining sprites

DIM swidth% '                                                                  the width of the sprite on the sprite sheet
DIM sheight% '                                                                 the height of the sprite on the sprite sheet

SELECT CASE handle%
    CASE SPRITE_ALLSPRITES
        FOR handle% = 1 TO UBOUND(sprites)
            IF NOT sprites(handle%).inuse AND NOT sprites(handle%).restore THEN SPRITEHIDE handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEHIDE", 100 : EXIT SUB '               is this an invalid sprite handle?
        IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEHIDE", 101 : EXIT SUB '              is this sprite handle in use?
        IF NOT sprites(handle%).restore THEN SPRITEERROR "SPRITEHIDE", 102 : EXIT SUB '            can the sprite be hidden?
        swidth% = sheets(sprites(handle%).sheet).spritewidth '                           width of sprite
        sheight% = sheets(sprites(handle%).sheet).spriteheight '                         height of sprite
        'IF sprites(handle%).onscreen THEN '                                             yes, is the sprite onscreen?
        '    _PUTIMAGE (sprites(handle%).backx + (swidth% / 2), sprites(handle%).backy + (sheight% / 2)), sprites(handle%).background ' restore background
        '    _FREEIMAGE sprites(handle%).background '                                    free the sprite's background image
            sprites(handle%).onscreen = 0 '                                             sprite is no longer on the screen
        'END IF
        sprites(handle%).visible = 0 '                                                  set sprite as being hidden
END SELECT

END SUB

'##################################################################################################################################

SUB SPRITEZOOM (handle%, zoom%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Change the size (zoom level) of a sprite.                                  |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEZOOM mysprite%, 150                                          |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle% - the sprite to change the size of.                        | - Specifying a zoom level of 0 or less will       |
'|         zoom%   - 1 to x (100 = 100%)                                      |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Output: none                                                               |                                                   |
'|                                                                            |                                                   |
'| Sets  : sprite().zoom to equal the zoom value passed in.                   |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEZOOM", 100 : EXIT SUB '               is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEZOOM", 101 : EXIT SUB '              is this sprite handle in use?
IF zoom% < 1 THEN SPRITEERROR "SPRITEZOOM", 103 '                              is zoom value valid?
sprites(handle%).zoom = zoom% '                                                 set the sprite's zoom level

END SUB

'##################################################################################################################################

SUB SPRITEROTATE (handle%, degrees!)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Rotates a sprite from 0 to 360 degrees.                                    |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEROTATE mysprite%, 22.5                                       |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%  - the sprite to rotate.                                   | - Specifying a rotation angle of less than 0 or   |
'|         degrees! - the angle to rotate the sprite to.                      |   or not less than 360 will result in the sub-    |
'|                                                                            |   routine reporting an error and halting program  |
'| Output: none                                                               |   execution.                                      |
'|                                                                            |                                                   |
'| Sets  : sprite().rotation to equal the rotation degrees passed in.         |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEROTATE", 100 : EXIT SUB '             is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEROTATE", 101 : EXIT SUB '            is this sprite handle in use?
IF (degrees! < 0) OR (degrees! > 360) THEN SPRITEERROR "SPRITEROTATE", 104 : EXIT SUB '   is angle within valid range?
sprites(handle%).rotation = degrees! '                                          set sprite degree angle

END SUB

'##################################################################################################################################

SUB SPRITEFLIP (handle%, behavior%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Flips a sprite horizontaly, verticaly, both or resets to no flipping.      |                     - NOTES -                     |
'|                                                                            | - Specifying a sprite that does not exist will    |
'| Usage : SPRITEFLIP mysprite%, 2                                            |   result in the subroutine reporting an error and |
'|                                                                            |   halting program execution.                      |
'| Input : handle%   - the sprite to flip.                                    | - Specifying an invalid behavior type will result |
'|         behavior% - the type of flipping desired:                          |   in the subroutine reporting an error and        |
'|                     0 = no flipping     (constant NONE)                    |   halting program execution.                      |
'|                     1 = flip horizontal (constant HORIZONTAL)              | - The constants NONE, HORIZONTAL, VERTICAL and    |
'|                     2 = flip vertical   (constant VERTICAL)                |   BOTH have been created to be used with this     |
'|                     3 = flip both       (constant BOTH)                    |   subroutine.                                     |
'|                                                                            | - Once a flip behavior has been set it will       |
'| Output: none                                                               |   remain in effect until the behavior is changed. |
'|                                                                            |                                                   |
'| Sets  : sprite().flip to equal the bevaior number passed in.               |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEFLIP", 100 : EXIT SUB '               is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEFLIP", 101 : EXIT SUB '              is this sprite handle in use?
IF behavior% < 0 OR behavior% > 3 THEN SPRITEERROR "SPRITEFLIP", 105 : EXIT SUB '         is this a valid behavior?
sprites(handle%).flip = behavior% '                                             save the new flipping behavior to the sprite

END SUB

'##################################################################################################################################

FUNCTION SPRITENEW (sheet%, cell%, behavior%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Creates a new sprite given the sheet and images that make up the sprite.   |                     - NOTES -                     |
'|                                                                            | - Specifying a sheet that does not exist will     |
'| Usage : mysprite% = SPRITENEW(mysheet%, 1, -1)                             |   result in the function reporting an error and   |
'|                                                                            |   halting program execution.                      |
'| Input : sheet%     - the sprite sheet the images reside on.                | - The constants SAVE and DONTSAVE have been       |
'|         cell%      - the first image in the animation set.                 |   created to be used with this function.          |
'|                                                                            |                                                   |
'|         behavior%  - the desired background behavior of sprite:            |                                                   |
'|                      0 = don't restore background (constant DONTSAVE)      |                                                   |
'|                     -1 = restore background       (constant SAVE)          |                                                   |
'|                                                                            |                                                   |
'| Output: integer value greater than 0 pointing to the newly created sprite. |                                                   |
'|                                                                            |                                                   |
'| Sets  : all variables associated with sprite().*                           |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+

SHARED sheets() AS SHEET '                                                      array defining sprite sheets
SHARED sprites() AS SPRITE '                                                    array defining sprites

DIM handle% '                                                                  handle number of new sprite

IF sheet% > UBOUND(sheets) THEN SPRITEERROR "SPRITENEW", 106 : EXIT FUNCTION '                  is this an invalid sheet handle?
IF NOT sheets(sheet%).inuse THEN SPRITEERROR "SPRITENEW", 107 : EXIT FUNCTION '                 is this sheet handle in use?
handle% = 0 '                                                                  initialize handle value
DO '                                                                           look for the next available handle
    handle% = handle% + 1 '                                                    increment the handle value
LOOP UNTIL (NOT sprites(handle%).inuse) OR handle% = UBOUND(sprites) '           stop looking when valid handle value found
IF sprites(handle%).inuse THEN '                                                is the last array element in use?
    handle% = handle% + 1 '                                                    yes, increment the handle value
    REDIM _PRESERVE sprites(0 TO handle%) AS SPRITE '                                increase the size of sprite array
END IF
sprites(handle%).inuse = TRUE '                                                   mark this element as being used
sprites(handle%).sheet = sheet% '                                               sprite sheet graphics can be found on
sprites(handle%).currentcell = cell% '                                          reset first image as default
sprites(handle%).animation = 0 '                                                reset automatic animation to off
sprites(handle%).animtype = 0 '                                                 default to forward animation looping
sprites(handle%).animdir = 1 '                                                  forward/backward animation looping starts forward
sprites(handle%).animstart = 1 '                                                reset animation sequence start cell
sprites(handle%).animend = 1 '                                                  reset amimation sequence end cell
sprites(handle%).flip = 0 '                                                     sprite is not flipped horizontaly or verticaly
sprites(handle%).onscreen = 0 '                                                 sprite is not on screen yet
sprites(handle%).visible = -1 '                                                 sprite has not been hidden
sprites(handle%).actualx = 0 '                                                  reset sprite's actual x location
sprites(handle%).actualy = 0 '                                                  reset sprite's actual y location
sprites(handle%).currentx = 0 '                                                 reset sprite's current screen x location
sprites(handle%).currenty = 0 '                                                 reset sprite's current screen y location
sprites(handle%).backx = 0 '                                                    reset background x location
sprites(handle%).backy = 0 '                                                    reset background y location
sprites(handle%).currentwidth = 0 '                                             reset sprite screen width
sprites(handle%).currentheight = 0 '                                            reset sprite screen height
sprites(handle%).screenx1 = 0 '                                                 reset upper left x position
sprites(handle%).screeny1 = 0 '                                                 reset upper left y position
sprites(handle%).screenx2 = 0 '                                                 reset lower right x position
sprites(handle%).screeny2 = 0 '                                                 reset lower right y position
sprites(handle%).layer = 1 '                                                    reset sprite's layer
sprites(handle%).restore = behavior% '                                          set sprite's background restoration behavior
sprites(handle%).transparent = sheets(sheet%).transparent '               get transparent color of sprite
sprites(handle%).zoom = 100 '                                                   reset sprite's zoom level
sprites(handle%).rotation = 0 '                                                 reset sprite's degree of rotation
sprites(handle%).detect = 0 '                                                   reset sprite's collision autodetection
sprites(handle%).detecttype = 0 '                                               reset to no collision detection desired
sprites(handle%).collx1 = 0 '                                                   reset collision rectangular area
sprites(handle%).colly1 = 0 '                                                   reset collision rectangular area
sprites(handle%).collx2 = 0 '                                                   reset collision rectangular area
sprites(handle%).colly2 = 0 '                                                   reset collision rectangular area
sprites(handle%).collsprite = 0 '                                               sprite is not currently colliding with sprite
sprites(handle%).motion = 0 '                                                   reset sprite's automotion
sprites(handle%).speed = 0 '                                                    reset sprite's automotion speed
sprites(handle%).direction = 0 '                                                reset sprite's automotion direction
sprites(handle%).pointer = 0 '                                                  mouse not currently interacting
sprites(handle%).mouseax = -32767 '                                             set as no interaction with mouse
sprites(handle%).mouseay = -32767 '                                             set as no interaction with mouse
sprites(handle%).mousecx = -32767 '                                             set as no interaction with mouse
sprites(handle%).mousecy = -32767 '                                             set as no interaction with mouse
sprites(handle%).score = 0 '                                                    reset sprite's score value
sprites(handle%).motion = 0
sprites(handle%).speed = 0
sprites(handle%).direction = 0
sprites(handle%).xdir = 0
sprites(handle%).ydir = 0
sprites(handle%).spindir = 0
SPRITENEW = handle% '                                                          return the handle number pointing to this sprite

END FUNCTION

'##################################################################################################################################

FUNCTION SPRITESHEETLOAD (filename$, spritewidth%, spriteheight%, transparent&)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Loads a sprite sheet into the sprite sheet array and assigns an integer    |                     - NOTES -                     |
'| handle value pointing to the sheet.                                        | - Specifying a transparent color less than 0 will |
'|                                                                            |   indicate to the function that no transparency   |
'| Usage : mysheet% = SPRITESHEETLOAD("sprites.png", 64, 96, _RGB(0, 1, 0))   |   exists in the sprite sheet.                     |
'|                                                                            | - Specifying a file name of a file that does not  |
'| Input : filename$     - the name of the sprite sheet image.                |   exist will result in the function reporting an  |
'|         spritewidth%  - the width of each sprite on the sheet.             |   error and halting program execution.            |
'|         spriteheight% - the height of each sprite on the sheet.            | - When the width and height of sprites are added  |
'|         transparent&  - the transparent color layer in sheet (if any).     |   they should match the sheet width and height.   |
'|                          -1 = no transparency (constant NOTRANSPARENCY).   |   For example, 10 sprites of 20x20 pixels         |
'|                          -2 = auto discover   (constant AUTOTRANSPARENCY)  |   arranged in two rows should make a 100x40 sheet.|
'|                         >-1 = transparency color.                          |                                                   |
'|                                                                            | - The constants NOTRANSPARENCY and                |
'| Output: integer value greater than 0 pointing to the newly loaded sheet.   |   AUTOTRANSPARENCY have been created to be used   |
'|                                                                            |   with this function.                             |
'|                                                                            | - THERE IS A KNOWN ISSUE with this function. See  |
'| Sets  :                                                                    |   "Known Issues" below.                           |
'|                                                                            |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Known Issues: Currently, specifying a transparency value (transparent&) has no effect on any of the other subroutines and      |
'|               functions in this library. For now, simply supply a value of 0 (or an actual color if you prefer). This feature  |
'|               is going to be added in a later revision of the library. Use transparent PNG files for now for transparency.     |
'+--------------------------------------------------------------------------------------------------------------------------------+

SHARED sheets() AS SHEET '                                                      array defining sprite sheets

DIM handle% '                                                                  handle number of new sprite sheet
DIM x%
DIM y%

IF NOT _FILEEXISTS(filename$) THEN SPRITEERROR "SPRITESHEETLOAD", 106 : EXIT FUNCTION '        does the sprite sheet exist?
handle% = 0 '                                                                  initialize handle value
DO '                                                                           look for the next available handle
    handle% = handle% + 1 '                                                    increment the handle value
LOOP UNTIL (NOT sheets(handle%).inuse) OR handle% = UBOUND(sheets) '             stop looking when valid handle value found
IF sheets(handle%).inuse THEN '                                                 is the last array element in use?
    handle% = handle% + 1 '                                                    yes, increment the handle value
    REDIM _PRESERVE sheets(0 TO handle%) AS SHEET '                                  increase the size of sprite sheet array
END IF
sheets(handle%).sheetimage = _LOADIMAGE(filename$, 32) '                        assign the image to the array
sheets(handle%).inuse = -1 '                                                    mark this element as being used
sheets(handle%).sheetwidth = _WIDTH(sheets(handle%).sheetimage) '                save the width of the sprite sheet
sheets(handle%).sheetheight = _HEIGHT(sheets(handle%).sheetimage) '              save the height of the sprite sheet
sheets(handle%).spritewidth = spritewidth% '                                    save the width of each sprite
sheets(handle%).spriteheight = spriteheight% '                                  save the height of each sprite
sheets(handle%).columns = sheets(handle%).sheetwidth / spritewidth% '            number of sprite columns on sheet
SELECT CASE transparent& '                                                     which type of transparency selected?
    CASE -2 '                                     (constant AUTOTRANSPARENCY)  auto discover the transparency color
        x% = 0 '                                                               start at upper left x of sheet
        y% = 0 '                                                               start at upper left y of sheet
        _SOURCE sheets(handle%).sheetimage '                                    set the sprite sheet image as the source image
        DO '                                                                   start looping through the sheet's pixels
            pixel& = POINT(x%, y%) '                                           get the pixel's color attributes
            alpha& = _ALPHA32(pixel&) '                                        get the alpha level (0 - 255)
            IF alpha& = 0 THEN EXIT DO '                                       if it is transparent then leave the loop
            x% = x% + 1 '                                                      move right one pixel
            IF x% > sheets(handle%).sheetwidth THEN '                           have we gone off the sheet?
                x% = 0 '                                                       yes, reset back to the left beginning
                y% = y% + 1 '                                                  move down one pixel
            END IF
        LOOP UNTIL y% > sheets(handle%).sheetheight '                           don't stop until the entire sheet has been checked
        IF alpha& = 0 THEN '                                                   did we find a transparent pixel?
            sheets(handle%).transparent = pixel& '                              yes, set the sheet's transparency to this color
        ELSE '                                                                 there was no transparent pixel found
            sheets(handle%).transparent = -1 '                                  set as having no transparency layer
        END IF
        _SOURCE 0 '                                                            set the source back to the screen
    CASE -1 '                                     (constant NOTRANSPARENCY)    sheet has no transparency layer
        sheets(handle%).transparent = -1 '                                      set as having no transparency layer
    CASE ELSE '                                                                transparency layer specified by programmer
        sheets(handle%).transparent = transparent& '                            set transparency layer color
END SELECT
SPRITESHEETLOAD = handle% '                                                    return the handle number pointing to this sheet

END FUNCTION

'##################################################################################################################################

SUB SPRITEERROR (routine$, errno%)
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Reports a Sprite Library error to the programmer. Used internally only.    |                     - NOTES -                     |
'|                                                                            | This routine will report the error to the         |
'| Input : errno%                                                             | programmer and end the program.                   |
'|         100 - sprite does not exist                                        |                                                   |
'|         101 - sprite is not in use                                         |                                                   |
'|         102 - sprite can't be hidden                                       |                                                   |
'|         103 - invalid zoom value                                           |                                                   |
'|         104 - invalid rotation angle                                       |                                                   |
'|         105 - invalid flipping behavior                                    |                                                   |
'|         106 - sheet does not exist                                         |                                                   |
'|         107 - sheet is not in use                                          |                                                   |
'|                                                                            |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits: This routine was created in response to a request from QB64 member pitt                                               |
'|          http://www.qb64.net/forum/index.php?topic=7281.0                                                                      |
'+--------------------------------------------------------------------------------------------------------------------------------+

DIM printerror AS _BIT
DIM checkerror AS INTEGER
DIM routineno AS INTEGER

SELECT CASE routine$
    CASE "SPRITEANIMATESET"
        routineno = 1
    CASE "SPRITEANIMATION"
        routineno = 2
    CASE "SPRITEANIMATIONCELLSET"
        routineno = 3
    CASE "SPRITECOLLIDETYPE"
        routineno = 4
    CASE "SPRITEDIRECTIONSET"
        routineno = 5
    CASE "SPRITEFLIP"
        routineno = 6
    CASE "SPRITEFREE"
        routineno = 7
    CASE "SPRITEHIDE"
        routineno = 8
    CASE "SPRITEMOTION"
        routineno = 9
    CASE "SPRITENEXT"
        routineno = 10
    CASE "SPRITEPREVIOUS"
        routineno = 11
    CASE "SPRITEPUT"
        routineno = 12
    CASE "SPRITESTRETCH"
        routineno = 13
    CASE "SPRITEREVERSEX"
        routineno = 14
    CASE "SPRITEREVERSEY"
        routineno = 15
    CASE "SPRITEROTATE"
        routineno = 16
    CASE "SPRITESCORESET"
        routineno = 17
    CASE "SPRITESET"
        routineno = 18
    CASE "SPRITESHOW"
        routineno = 19
    CASE "SPRITESPEEDSET"
        routineno = 20
    CASE "SPRITESPINSET"
        routineno = 21
    CASE "SPRITESTAMP"
        routineno = 22
    CASE "SPRITETRAVEL"
        routineno = 23
    CASE "SPRITETRAVEL"
        routineno = 24
    CASE "SPRITEZOOM"
        routineno = 25
    CASE "SPRITECELLS"
        routineno = 26
    CASE "SPRITEANGLE"
        routineno = 27
    CASE "SPRITEANIMATIONCELL"
        routineno = 28
    CASE "SPRITEAX"
        routineno = 29
    CASE "SPRITEAY"
        routineno = 30
    CASE "SPRITECOLLIDE"
        routineno = 31
    CASE "SPRITECOLLIDEWITH"
        routineno = 32
    CASE "SPRITECOPY"
        routineno = 33
    CASE "SPRITECURRENTHEIGHT"
        routineno = 34
    CASE "SPRITECURRENTWIDTH"
        routineno = 35
    CASE "SPRITEDIRECTION"
        routineno = 36
    CASE "SPRITEFILEEXISTS"
        routineno = 37
    CASE "SPRITEMOUSE"
        routineno = 38
    CASE "SPRITEMOUSEAX"
        routineno = 39
    CASE "SPRITEMOUSEAY"
        routineno = 40
    CASE "SPRITEMOUSEX"
        routineno = 41
    CASE "SPRITEMOUSEY"
        routineno = 42
    CASE "SPRITENEW"
        routineno = 43
    CASE "SPRITEROTATION"
        routineno = 44
    CASE "SPRITESCORE"
        routineno = 45
    CASE "SPRITESHEETLOAD"
        routineno = 46
    CASE "SPRITESHOWING"
        routineno = 47
    CASE "SPRITEX"
        routineno = 48
    CASE "SPRITEX1"
        routineno = 49
    CASE "SPRITEX2"
        routineno = 50
    CASE "SPRITEY"
        routineno = 51
    CASE "SPRITEY1"
        routineno = 52
    CASE "SPRITEY2"
        routineno = 53
    CASE "SPRITEZOOMLEVEL"
        routineno = 54
END SELECT

printerror = TRUE
FOR checkerror = 1 TO UBOUND(errors)
    IF errors(checkerror).err = errno% AND errors(checkerror).line = routineno THEN
        errors(checkerror).count = errors(checkerror).count + 1
        printerror = FALSE
        EXIT FOR
    END IF
NEXT checkerror

IF printerror THEN
PRINT #32766, Timestamp$; " - "; "Sprite Library:"; routine$; " - "; LTRIM$(RTRIM$(STR$(errno%)));
SELECT CASE errno% '                                                           which error number is being reported?
    CASE 100
        PRINT #32766, " - the specified sprite does not exist"
    CASE 101
        PRINT #32766, " - the specified sprite is not in use"
    CASE 102
        PRINT #32766, " - the specified sprite can't be hidden"
    CASE 103
        PRINT #32766, " - the zoom value specified must be greater than zero"
    CASE 104
        PRINT #32766, " - the angle specified must be 0 to 360"
    CASE 105
        PRINT #32766, " - invalid flipping behavior specified"
    CASE 106
        PRINT #32766, " - the specified sprite sheet does not exist"
    CASE 107
        PRINT #32766, " - the specified sprite sheet is not in use"
END SELECT
REDIM _PRESERVE errors(1 TO UBOUND(errors) + 1) AS errorhandle
errors(UBOUND(errors)).err = errno%
errors(UBOUND(errors)).line = routineno
errors(UBOUND(errors)).count = 1
END IF

END SUB


'##################################################################################################################################
 
FUNCTION SPRITEANIMATIONCELL (handle%)

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANIMATIONCELL", 100 : EXIT FUNCTION '          is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEANIMATIONCELL", 101 : EXIT FUNCTION '         is this sprite handle in use?
SPRITEANIMATIONCELL = sprites(handle%).currentcell

END FUNCTION

'##################################################################################################################################

SUB SPRITEANIMATIONCELLSET (handle%, cell%)

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITEANIMATIONCELLSET", 100 : EXIT SUB '       is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITEANIMATIONCELLSET", 101 : EXIT SUB '      is this sprite handle in use?
sprites(handle%).currentcell = cell%

END SUB

'##################################################################################################################################

FUNCTION SPRITECELLS (handle%)

SHARED sprites() AS SPRITE '                                                    array defining sprites

IF handle% > UBOUND(sprites) THEN SPRITEERROR "SPRITECELLSN", 100 : EXIT FUNCTION '          is this an invalid sprite handle?
IF NOT sprites(handle%).inuse THEN SPRITEERROR "SPRITECELLS", 101 : EXIT FUNCTION '         is this sprite handle in use?
SPRITECELLS = sprites(handle%).animend

END FUNCTION

'##################################################################################################################################

SUB SHEETFREE (handle%) 'frees a sheet handle and all the sprites derived from that sheet

SHARED sheets() AS SHEET '                                                    array defining sprites
SHARED sprites() AS SPRITE '                                                    array defining sprites

SELECT CASE handle%
    CASE SPRITE_ALLSHEETS
        FOR handle% = 1 TO UBOUND(sheets)
            IF NOT sheets(handle%).inuse THEN SHEETFREE handle%
        NEXT handle%
    CASE ELSE
        IF handle% > UBOUND(sheets) THEN SPRITEERROR "SHEETFREE", 106 : EXIT SUB '                  is this an invalid sheet handle?
        IF NOT sheets(handle%).inuse THEN SPRITEERROR "SHEETFREE", 107 : EXIT SUB '                 is this sheet handle in use?
        FOR checksprites% = 1 TO UBOUND(sprites)
        IF sprites(checksprites%).inuse AND sprites(checksprites%).sheet = handle% THEN SPRITEFREE checksprites%
        NEXT checksprites%
        IF sheets(handle%).sheetimage THEN _FREEIMAGE sheets(handle%).sheetimage
        IF handle% = UBOUND(sheets) AND handle% <> 1 THEN '                            is this the last element in the array?
            REDIM _PRESERVE sheets(0 TO handle% - 1) AS SHEET '                            yes, resize the array, removing the element
        ELSE '                                                                         this is not the last element in the array
            sheets(handle%).inuse = 0 '                                                mark the array entry as not in use
        END IF
END SELECT

END SUB

'##################################################################################################################################


'+----------------------------------------------------------------------------+---------------------------------------------------+
'| <blank>                                                                    |                     - NOTES -                     |
'|                                                                            | -                                                 |
'| Usage :                                                                    |                                                   |
'|                                                                            |                                                   |
'| Input :                                                                    |                                                   |
'|                                                                            |                                                   |
'| Output:                                                                    |                                                   |
'+----------------------------------------------------------------------------+---------------------------------------------------+
'| Credits:                                                                                                                       |
'|                                                                                                                                |
'+--------------------------------------------------------------------------------------------------------------------------------+

