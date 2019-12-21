'******************************************************************************
'
'Gorlock's QB64 Time Library
'v0.13
'
'******************************************************************************
'
'Version 0.1  - 2013-10-19
'Version 0.11 - 2013-10-20
'  - added additional lunar commands
'Version 0.12 - 2013-10-24
'  - added time name and holiday commands
'Version 0.13 - 2013-12-13
'  - fixed minor errors
'
'******************************************************************************
'
'
'SUB       CLEARDATE                       (t AS SYSTEMTIME)                                                  Clears the wYear, wMonth, and wDay elements of a SYSTEMTIME
'SUB       CLEARTIME                       (t AS SYSTEMTIME)                                                  Clears the wHour, wMinute, wSecond, and wMilliseconds elements of a SYSTEMTIME
'SUB       CURRENTTIME                     (t AS SYSTEMTIME)                                                  Saves the current system time
'SUB       LUNAR_LAST_FULL                 (t AS SYSTEMTIME)                                                  Saves the date of the last full moon
'SUB       LUNAR_LAST_FULL_X               (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Saves the date of the last full moon from a SYSTEMTIME
'SUB       LUNAR_LAST_NEW                  (t AS SYSTEMTIME)                                                  Saves the date of the last new moon
'SUB       LUNAR_LAST_NEW_X                (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Saves the date of the last new moon from a SYSTEMTIME
'SUB       MAKETIME                        (t AS SYSTEMTIME, y%, m%%, d%%, h%%, n%%, s%%, f%)                 Creates a SYSTEMTIME
'SUB       LUNAR_NEXT_FULL                 (t AS SYSTEMTIME)                                                  Saves the date of the next full moon
'SUB       LUNAR_NEXT_FULL_X               (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Saves the date of the next full moon from a SYSTEMTIME
'SUB       LUNAR_NEXT_NEW                  (t AS SYSTEMTIME)                                                  Saves the date of the next new moon
'SUB       LUNAR_NEXT_NEW_X                (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Saves the date of the next new moon from a SYSTEMTIME
'SUB       REPAIRTIME                      (t AS SYSTEMTIME)                                                  Checks a SYSTEMTIME for errors and fixes them
'SUB       TIMEAFTER                       (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, y%, m%%, d%%, h%%, n%%, s%%)  Saves a SYSTEMTIME specifying a certain time after a SYSTEMTIME
'SUB       TIMEBEFORE                      (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, y%, m%%, d%%, h%%, n%%, s%%)  Saves a SYSTEMTIME specifying a certain time before a SYSTEMTIME
'SUB       TIMEBETWEEN                     (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, t3 AS SYSTEMTIME)             Saves a SYSTEMTIME specifying the time between two SYSTEMTIMEs
'FUNCTION  AMPM`                           ()                                                                 Returns TRUE if it is PM, FALSE if it is AM
'FUNCTION  AMPM_X`                         (t AS SYSTEMTIME)                                                  Returns TRUE if it is PM, FALSE if it is AM for a SYSTEMTIME
'FUNCTION  ASTROLOGICALAGE$                ()                                                                 Returns the name of the current astrological age
'FUNCTION  ASTROLOGICALAGE_CYCLE%          ()                                                                 Returns the number of years since the start of the current astrological age
'FUNCTION  ASTROLOGICALAGE_CYCLE_X%        (t AS SYSTEMTIME)                                                  Returns the number of years since the start of the respective astrological age of a SYSTEMTIME
'FUNCTION  ASTROLOGICALAGE_END&            ()                                                                 Returns the year that marks the end of the current astrological age
'FUNCTION  ASTROLOGICALAGE_END_X&          (t AS SYSTEMTIME)                                                  Returns the year that marks the end of the respective astrological age of a SYSTEMTIME
'FUNCTION  ASTROLOGICALAGE_LEFT%           ()                                                                 Returns the number of years remaining in the current astrological age
'FUNCTION  ASTROLOGICALAGE_LEFT_X%         (t AS SYSTEMTIME)                                                  Returns the number of years remaining in the respective astrological age of a SYSTEMTIME
'FUNCTION  ASTROLOGICALAGE_START&          ()                                                                 Returns the year that marked the start of the current astrological age
'FUNCTION  ASTROLOGICALAGE_START_X&        (t AS SYSTEMTIME)                                                  Returns the year that marked the start of the respective astrological age of a SYSTEMTIME
'FUNCTION  ASTROLOGICALAGE_X$              (t AS SYSTEMTIME)                                                  Returns the name of the respective astrological age of a SYSTEMTIME
'FUNCTION  BIRTHSTONE$                     ()                                                                 Returns the birthstone of the current month
'FUNCTION  BIRTHSTONE_X$                   (t AS SYSTEMTIME)                                                  Returns the birthstone of the month specified in a SYSTEMTIME
'FUNCTION  DATETIME$                       ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YYYY HH:MM:SS"
'FUNCTION  DATETIME_12$                    ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YYYY HH:MM:SS"
'FUNCTION  DATETIME_12_SHORTYEAR$          ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YY HH:MM:SS"
'FUNCTION  DATETIME_12_SHORTYEAR_X$        (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM:SS"
'FUNCTION  DATETIME_12_X$                  (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM:SS"
'FUNCTION  DATETIME_LONG$                  ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YYYY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_12$               ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YYYY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_12_SHORTYEAR$     ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_12_SHORTYEAR_X$   (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_12_X$             (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_SHORTYEAR$        ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_SHORTYEAR_X$      (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM:SS:mm"
'FUNCTION  DATETIME_LONG_X$                (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM:SS:mm"
'FUNCTION  DATETIME_SHORT$                 ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YYYY HH:MM"
'FUNCTION  DATETIME_SHORTYEAR$             ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YY HH:MM:SS"
'FUNCTION  DATETIME_SHORTYEAR_X$           (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM:SS"
'FUNCTION  DATETIME_SHORT_12$              ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YYYY HH:MM"
'FUNCTION  DATETIME_SHORT_12_SHORTYEAR$    ()                                                                 Returns the 12-hour datetime expressed as a string in the form "MM-DD-YY HH:MM"
'FUNCTION  DATETIME_SHORT_12_SHORTYEAR_X$  (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM"
'FUNCTION  DATETIME_SHORT_12_X$            (t AS SYSTEMTIME)                                                  Returns the 12-hour datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM"
'FUNCTION  DATETIME_SHORT_SHORTYEAR$       ()                                                                 Returns the datetime expressed as a string in the form "MM-DD-YY HH:MM"
'FUNCTION  DATETIME_SHORT_SHORTYEAR_X$     (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YY HH:MM"
'FUNCTION  DATETIME_SHORT_X$               (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM"
'FUNCTION  DATETIME_X$                     (t AS SYSTEMTIME)                                                  Returns the datetime specified in a SYSTEMTIME expressed as a string in the form "MM-DD-YYYY HH:MM:SS"
'FUNCTION  DATE_SHORT$                     ()                                                                 Returns the date expressed in the form "MM-DD-YY"
'FUNCTION  DATE_SHORT_X$                   (t AS SYSTEMTIME)                                                  Returns the date of a SYSTEMTIME expressed in the form "MM-DD-YY"
'FUNCTION  DATE_X$                         (t AS SYSTEMTIME)                                                  Returns the date of a SYSTEMTIME expressed in the form "MM-DD-YYYY"
'FUNCTION  DAY%%                           ()                                                                 Returns the day value of the date
'FUNCTION  DAY_X%%                         (t AS SYSTEMTIME)                                                  Returns the day value of a date specified in a SYSTEMTIME
'FUNCTION  DAYNAME$                        ()                                                                 Returns the weekday name of the date
'FUNCTION  DAYNAME_X$                      (t AS SYSTEMTIME)                                                  Returns the weekday name of the date specified in a SYSTEMTIME
'FUNCTION  DAYOFWEEK%%                     ()                                                                 Returns the count of days since the last Sunday
'FUNCTION  DAYOFWEEK_X%%                   (t AS SYSTEMTIME)                                                  Returns the count of days since the last Sunday of a specific SYSTEMTIME
'FUNCTION  HOLIDAY$                        ()                                                                 Returns the holidays that are today
'FUNCTION  HOLIDAY_X$                      (t AS SYSTEMTIME)                                                  Returns the holidays that are on the date specified in a SYSTEMTIME
'FUNCTION  HOUR%%                          ()                                                                 Returns the hour value of the time
'FUNCTION  HOUR_12%%                       ()                                                                 Returns the hour value of the time in 12 Hour Time
'FUNCTION  HOUR_12_X%%                     (t AS SYSTEMTIME)                                                  Returns the hour value of the time specified in a SYSTEMTIME in 12 Hour Time
'FUNCTION  HOUR_X%%                        (t AS SYSTEMTIME)                                                  Returns the hour value of the time specified in a SYSTEMTIME
'FUNCTION  JULIANDAY&                      ()                                                                 Returns the Julian date
'FUNCTION  JULIANDAY_LONG#                 ()                                                                 Returns the Julian date including partial date
'FUNCTION  JULIANDAY_LONG_X#               (t AS SYSTEMTIME)                                                  Returns the Julian date including partial date of the date specified in a SYSTEMTIME
'FUNCTION  JULIANDAY_X&                    (t AS SYSTEMTIME)                                                  Returns the Julian date of the date specified in a SYSTEMTIME
'FUNCTION  LEAPYEAR`                       ()                                                                 Returns TRUE if this year is a leap year
'FUNCTION  LEAPYEAR_X`                     (t AS SYSTEMTIME)                                                  Returns TRUE if the year specified in a SYSTEMTIME is a leap year
'FUNCTION  LONGCOUNT$                      ()                                                                 Returns the Mayan Long Count expressed as a string
'FUNCTION  LONGCOUNT_BAKTUN%%              ()                                                                 Returns the B'ak'tun value of the Long Count
'FUNCTION  LONGCOUNT_BAKTUN_X%%            (t AS SYSTEMTIME)                                                  Returns the B'ak'tun value of the Long Count of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_CALENDARROUND$        ()                                                                 Returns the Mayan Calendar Round expressed as a string
'FUNCTION  LONGCOUNT_CALENDARROUND_X$      (t AS SYSTEMTIME)                                                  Returns the Mayan Calendar Round of a date specified by a SYSTEMTIME expressed as a string
'FUNCTION  LONGCOUNT_DAYS&                 ()                                                                 Returns the number of days since the last 13th Bak'tun
'FUNCTION  LONGCOUNT_DAYS_LONG&&           ()                                                                 Returns the number of days since the last 13th Bak'tun including partial days
'FUNCTION  LONGCOUNT_DAYS_LONG_X&&         (t AS SYSTEMTIME)                                                  Returns the number of days since the last 13th Bak'tun of a date specified by a SYSTEMTIME including partial days
'FUNCTION  LONGCOUNT_DAYS_X&               (t AS SYSTEMTIME)                                                  Returns the number of days since the last 13th Bak'tun of a date specified by a SYSTEMTIME
'FUNCTION  LONGCOUNT_HAAB$                 ()                                                                 Returns the Mayan Haab' expressed as a string
'FUNCTION  LONGCOUNT_HAAB_DATE%%           ()                                                                 Returns the day value of the Haab' month
'FUNCTION  LONGCOUNT_HAAB_DATE_X%%         (t AS SYSTEMTIME)                                                  Returns the day value of the Haab' month of a date specified by a SYSTEMTIME
'FUNCTION  LONGCOUNT_HAAB_DAY%             ()                                                                 Returns the day value of the Haab' cycle
'FUNCTION  LONGCOUNT_HAAB_DAY_X%           (t AS SYSTEMTIME)                                                  Returns the day value of the Haab' cycle of a date specified by a SYSTEMTIME
'FUNCTION  LONGCOUNT_HAAB_MONTH$           ()                                                                 Returns the Haab' month name
'FUNCTION  LONGCOUNT_HAAB_MONTH_NUM%%      ()                                                                 Returns the Haab' month value
'FUNCTION  LONGCOUNT_HAAB_MONTH_NUM_X%%    (t AS SYSTEMTIME)                                                  Returns the Haab' month value of a date specified by a SYSTEMTIME
'FUNCTION  LONGCOUNT_HAAB_MONTH_X$         (t AS SYSTEMTIME)                                                  Returns the Haab' month name of a date specified by a SYSTEMTIME
'FUNCTION  LONGCOUNT_HAAB_X$               (t AS SYSTEMTIME)                                                  Returns the Mayan Haab' of a date specified by a SYSTEMTIME expressed as a string
'FUNCTION  LONGCOUNT_KATUN%%               ()                                                                 Returns the K'atun value of the Long Count
'FUNCTION  LONGCOUNT_KATUN_X%%             (t AS SYSTEMTIME)                                                  Returns the K'atun value of the Long Count of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_KIN%%                 ()                                                                 Returns the K'in value of the Long Count
'FUNCTION  LONGCOUNT_KIN_X%%               (t AS SYSTEMTIME)                                                  Returns the K'in value of the Long Count of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_LONG$                 ()                                                                 Returns the Long Count including the Tzolk'in and Haab' dates expressed as a string
'FUNCTION  LONGCOUNT_LONG_X$               (t AS SYSTEMTIME)                                                  Returns the Long Count including the Tzolk'in and Haab' dates expressed as a string of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_TUN%%                 ()                                                                 Returns the Tun value of the Long Count
'FUNCTION  LONGCOUNT_TUN_X%%               (t AS SYSTEMTIME)                                                  Returns the Tun value of the Long Count of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_TZOLKIN$              ()                                                                 Returns the Mayan Tzolk'in expressed as a string
'FUNCTION  LONGCOUNT_TZOLKIN_DATE%%        ()                                                                 Returns the day value of the Tzolk'in week
'FUNCTION  LONGCOUNT_TZOLKIN_DATE_X%%      (t AS SYSTEMTIME)                                                  Returns the day value of the Tzolk'in week of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_TZOLKIN_DAY$          ()                                                                 Returns the Tzolk'in week name
'FUNCTION  LONGCOUNT_TZOLKIN_DAY_NUM%%     ()                                                                 Returns the Tzolk'in week value
'FUNCTION  LONGCOUNT_TZOLKIN_DAY_NUM_X%%   (t AS SYSTEMTIME)                                                  Returns the Tzolk'in week value of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_TZOLKIN_DAY_X$        (t AS SYSTEMTIME)                                                  Returns the Tzolk'in week name of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_TZOLKIN_X$            (t AS SYSTEMTIME)                                                  Returns the Mayan Tzolk'in of a date specified in a SYSTEMTIME expressed as a string
'FUNCTION  LONGCOUNT_WINAL%%               ()                                                                 Returns the Winal value of the Long Count
'FUNCTION  LONGCOUNT_WINAL_X%%             (t AS SYSTEMTIME)                                                  Returns the Winal value of the Long Count of a date specified in a SYSTEMTIME
'FUNCTION  LONGCOUNT_X$                    (t AS SYSTEMTIME)                                                  Returns the Mayan Long Count of a date specified in a SYSTEMTIME expressed as a string
'FUNCTION  LUNAR_AGE%%                     ()                                                                 Returns the age of the moon
'FUNCTION  LUNAR_AGE_LONG!                 ()                                                                 Returns the age of the moon including partial days
'FUNCTION  LUNAR_AGE_LONG_X!               (t AS SYSTEMTIME)                                                  Returns the age of the moon of the date specified in a SYSTEMTIME including patial days
'FUNCTION  LUNAR_AGE_X%%                   (t AS SYSTEMTIME)                                                  Returns the age of the moon of the date specified in a SYSTEMTIME
'FUNCTION  LUNAR_ANOMALISTIC!              ()                                                                 Returns the anomalistic phase of the moon
'FUNCTION  LUNAR_ANOMALISTIC_RAD!          ()                                                                 Returns the anomalistic phase of the moon in radians
'FUNCTION  LUNAR_ANOMALISTIC_RAD_X!        (t AS SYSTEMTIME)                                                  Returns the anomalistic phase of the moon of the date specified in a SYSTEMTIME in radians
'FUNCTION  LUNAR_ANOMALISTIC_X!            (t AS SYSTEMTIME)                                                  Returns the anomalistic phase of the moon of the date specified in a SYSTEMTIME
'FUNCTION  LUNAR_DISTANCE!                 ()                                                                 Returns the distance of the moon in Earth radii
'FUNCTION  LUNAR_DISTANCE_X!               (t AS SYSTEMTIME)                                                  Returns the distance of the moon of the date specified in a SYSTEMTIME in Earth radii
'FUNCTION  LUNAR_DRAW&                     (r!)                                                               Returns the handle of an image whereupon the moon in its current phase is drawn
'FUNCTION  LUNAR_DRAW_X&                   (t AS SYSTEMTIME, r!)                                              Returns the handle of an image whereupon the moon phase of the date specified in a SYSTEMTIME is drawn
'FUNCTION  LUNAR_ECLIPTIC_LATITUDE!        ()                                                                 Returns the current ecliptic latitude of the moon
'FUNCTION  LUNAR_ECLIPTIC_LATITUDE_X!      (t AS SYSTEMTIME)                                                  Returns the ecliptic latitude of the moon of a date specified by a SYSTEMTIME
'FUNCTION  LUNAR_ECLIPTIC_LONGITUDE!       ()                                                                 Returns the current ecliptic longitude of the moon
'FUNCTION  LUNAR_ECLIPTIC_LONGITUDE_X!     (t AS SYSTEMTIME)                                                  Returns the ecliptic longitude of the moon of a date specified by a SYSTEMTIME
'FUNCTION  LUNAR_PHASE$                    ()                                                                 Returns the current phase of the moon expressed as a string
'FUNCTION  LUNAR_PHASE_X$                  (t AS SYSTEMTIME)                                                  Returns the phase of the moon of the date specified in a SYSTEMTIME expressed as a string
'FUNCTION  LUNAR_SYNODIC!                  ()                                                                 Returns the synodic phase of the moon
'FUNCTION  LUNAR_SYNODIC_RAD!              ()                                                                 Returns the synodic phase of the moon in radians
'FUNCTION  LUNAR_SYNODIC_RAD_X!            (t AS SYSTEMTIME)                                                  Returns the synodic phase of the moon of the date specified in a SYSTEMTIME
'FUNCTION  LUNAR_SYNODIC_X!                (t AS SYSTEMTIME)                                                  Returns the synodic phase of the moon of the date specified in a SYSTEMTIME in radians
'FUNCTION  MILISECOND%                     ()                                                                 Returns the millisecond value of the current time
'FUNCTION  MILISECOND_STR$                 ()                                                                 Returns the millisecond value of the current time expressed as a string in the form "MM"
'FUNCTION  MILISECOND_STR_X$               (t AS SYSTEMTIME)                                                  Returns the millisecond value of the respective time of a SYSTEMTIME expressed as a string in the form "MM"
'FUNCTION  MILISECOND_X%                   (t AS SYSTEMTIME)                                                  Returns the millisecond value of the respective time of a SYSTEMTIME
'FUNCTION  MINUTE%%                        ()                                                                 Returns the minute value of the time
'FUNCTION  MINUTE_X%%                      (t AS SYSTEMTIME)                                                  Returns the minute value of the time specified in a SYSTEMTIME
'FUNCTION  MONTH%%                         ()                                                                 Returns the month value of the date
'FUNCTION  MONTH_X%%                       (t AS SYSTEMTIME)                                                  Returns the month value of the dat specified in a SYSTEMTIME
'FUNCTION  MONTHNAME$                      ()                                                                 Returns the name of the month
'FUNCTION  MONTHNAME_X$                    (t AS SYSTEMTIME)                                                  Returns the name of the month specified in a SYSTEMTIME
'FUNCTION  MONTH_DAYS%%                    (m%%)                                                              Returns the number of days in a month
'FUNCTION  PLATONICYEAR_CYCLE%             ()                                                                 Returns the number of years since the start of the current Platonic year
'FUNCTION  PLATONICYEAR_CYCLE_X%           (t AS SYSTEMTIME)                                                  Returns the number of years since the start of the respective Platonic year of a SYSTEMTIME
'FUNCTION  PLATONICYEAR_END&               ()                                                                 Returns the year that marks the end of the current Platonic year
'FUNCTION  PLATONICYEAR_END_X&             (t AS SYSTEMTIME)                                                  Returns the year that marks the end of the repspective Platonic year of a SYSTEMTIME
'FUNCTION  PLATONICYEAR_LEFT%              ()                                                                 Returns the number of years remaining before the end of the current Platonic year
'FUNCTION  PLATONICYEAR_LEFT_X%            (t AS SYSTEMTIME)                                                  Returns the number of years remaining before the end of the respective Platonic year of a SYSTEMTIME
'FUNCTION  PLATONICYEAR_START&             ()                                                                 Returns the year that marked the beginning of the current Platonic year
'FUNCTION  PLATONICYEAR_START_X&           (t AS SYSTEMTIME)                                                  Returns the year that marked the beginning of the respective Platonic year of a SYSTEMTIME
'FUNCTION  SECOND%%                        ()                                                                 Returns the second value of the time
'FUNCTION  SECOND_X%%                      (t AS SYSTEMTIME)                                                  Returns the second value of the time specified in a SYSTEMTIME
'FUNCTION  TICKCOUNT~&                     ()                                                                 Returns the number of milliseconds that have elapsed since the system was last started, up to 49.7 days
'FUNCTION  TIMEAFTER&&                     (t AS SYSTEMTIME, y%, m%%, d%%, h%%, n%%, s%%)                     Returns the 64bit filetime of a time starting at t and progressing through time per the parameters
'FUNCTION  TIMEBEFORE&&                    (t AS SYSTEMTIME, y%, m%%, d%%, h%%, n%%, s%%)                     Returns the 64bit filetime of a time starting at t and backtracking through time per the parameters
'FUNCTION  TIMEBETWEEN&&                   (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the 64bit filetime of a time in between time t1 and time t2
'FUNCTION  TIMEBETWEEN_DAY&&               (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in days
'FUNCTION  TIMEBETWEEN_HOUR&&              (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in hours
'FUNCTION  TIMEBETWEEN_MILLISECOND&&       (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in milliseconds
'FUNCTION  TIMEBETWEEN_MINUTE&&            (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in minutes
'FUNCTION  TIMEBETWEEN_MONTH&&             (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in months
'FUNCTION  TIMEBETWEEN_SECOND&&            (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in seconds
'FUNCTION  TIMEBETWEEN_STR$                (t AS SYSTEMTIME)                                                  Returns a SYSTEMTIME expressed as a string
'FUNCTION  TIMEBETWEEN_STR_LONG$           (t AS SYSTEMTIME)                                                  Returns a SYSTEMTIME expressed as a string including the millisecond value
'FUNCTION  TIMEBETWEEN_WEEK&&              (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in weeks
'FUNCTION  TIMEBETWEEN_YEAR&&              (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the amount of time between 2 SYSTEMTIMEs in years
'FUNCTION  TIMECOMPARE%%                   (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns the compares value of two times against one another
'FUNCTION  TIMEISAFTER`                    (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns TRUE if t1 is after t2
'FUNCTION  TIMEISBEFORE`                   (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns TRUE if t1 is before t2
'FUNCTION  TIMEISEQUAL`                    (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)                               Returns TRUE if t1 is equal to t2
'FUNCTION  TIMENAME$                       ()                                                                 Returns the name group of the time
'FUNCTION  TIMENAME_X$                     (t AS SYSTEMTIME)                                                  Returns the name group of the time specified in a SYSTEMTIME
'FUNCTION  TIMESTAMP$                      ()                                                                 Returns the timestamp expressed as a string in the form "YYYYMMDDHHMMSS"
'FUNCTION  TIMESTAMP_LONG$                 ()                                                                 Returns the timestamp expressed as a string in the form "YYYYMMDDHHMMSSmm"
'FUNCTION  TIMESTAMP_LONG_X$               (t AS SYSTEMTIME)                                                  Returns the timestamp expressed as a string in the form "YYYYMMDDHHMMSSmm"
'FUNCTION  TIMESTAMP_LONG_SHORTYEAR$       ()                                                                 Returns the timestamp expressed as a string in the form "YYMMDDHHMMSSmm"
'FUNCTION  TIMESTAMP_LONG_SHORTYEAR_X$     (t AS SYSTEMTIME)                                                  Returns the timestamp specified in a SYSTEMTIME expressed as a string in the form "YYMMDDHHMMSSmm"
'FUNCTION  TIMESTAMP_SHORT$                ()                                                                 Returns the timestamp expressed as a string in the form "YYYYMMDDHHMM"
'FUNCTION  TIMESTAMP_SHORT_X$              (t AS SYSTEMTIME)                                                  Returns the timestamp specified in a SYSTEMTIME expressed as a string in the form "YYYYMMDDHHMM"
'FUNCTION  TIMESTAMP_SHORTYEAR$            ()                                                                 Returns the timestamp expressed as a string in the form "YYMMDDHHMMSS"
'FUNCTION  TIMESTAMP_SHORTYEAR_X$          (t AS SYSTEMTIME)                                                  Returns the timestamp specified in a SYSTEMTIME expressed as a string in the form "YYMMDDHHMMSS"
'FUNCTION  TIMESTAMP_SHORT_SHORTYEAR$      ()                                                                 Returns the timestamp expressed as a string in the form "YYMMDDHHMM"
'FUNCTION  TIMESTAMP_SHORT_SHORTYEAR_X$    (t AS SYSTEMTIME)                                                  Returns the timestamp specified in a SYSTEMTIME expressed as a string in the form "YYMMDDHHMM"
'FUNCTION  TIMESTAMP_X$                    (t AS SYSTEMTIME)                                                  Returns the timestamp specified in a SYSTEMTIME expressed as a string in the form "YYYYMMDDHHMMSS"
'FUNCTION  TIMEZONE$                       ()                                                                 Returns the name of the timezone
'FUNCTION  TIMEZONE_DST%%                  ()                                                                 Returns whether it is daylight savings time or not
'FUNCTION  TIMEZONE_OFFSET!                ()                                                                 Returns the offset of the timezone from UTC in hours
'FUNCTION  TIME_12$                        ()                                                                 Returns the 12-hour time expressed as a string in the form "HH:MM:SS"
'FUNCTION  TIME_12_X$                      (t AS SYSTEMTIME)                                                  Returns the 12-hour time specified in a SYSMTEMTIME expressed as a string in the form "HH:MM:SS"
'FUNCTION  TIME_LONG$                      ()                                                                 Returns the time expressed as a string in the form "HH:MM:SS:mm"
'FUNCTION  TIME_LONG_12$                   ()                                                                 Returns the 12-hour time expressed as a string in the form "HH:MM:SS:mm"
'FUNCTION  TIME_LONG_12_X$                 (t AS SYSTEMTIME)                                                  Returns the 12-hour time specified in a SYSTEMTIME expressed as a string in the form "HH:MM:SS:mm"
'FUNCTION  TIME_LONG_X$                    (t AS SYSTEMTIME)                                                  Returns the time specified in a SYSTEMTIME expressed as a string in the form "HH:MM:SS:mm"
'FUNCTION  TIME_SHORT$                     ()                                                                 Returns the time expressed as a string in the form "HH:MM"
'FUNCTION  TIME_SHORT_12$                  ()                                                                 Returns the 12-hour time expressed as a string in the form "HH:MM"
'FUNCTION  TIME_SHORT_12_X$                (t AS SYSTEMTIME)                                                  Returns the 12-hour time specified in a SYSTEMTIME expressed as a string in the form "HH:MM"
'FUNCTION  TIME_SHORT_X$                   (t AS SYSTEMTIME)                                                  Returns the time specified in a SYSTEMTIME expressed as a string in the form "HH:MM"
'FUNCTION  TIME_X$                         (t AS SYSTEMTIME)                                                  Returns the time specified in a SYSTEMTIME expressed as a string in the form "HH:MM:SS"
'FUNCTION  UNIXTIME&                       ()                                                                 Returns the number of seconds since 01-01-1970
'FUNCTION  UNIXTIME_LONG&&                 ()                                                                 Returns the number of 10 microseconds since 01-01-1970
'FUNCTION  UNIXTIME_LONG_X&&               (t AS SYSTEMTIME)                                                  Returns the number of 10 microseconds since 01-01-1970 of a time specified in a SYSTEMTIME
'FUNCTION  UNIXTIME_X&                     (t AS SYSTEMTIME)                                                  Returns the number of seconds since 01-01-1970 of a time specified in a SYSTEMTIME
'FUNCTION  WEEK%%                          ()                                                                 Returns the week number of the year
'FUNCTION  WEEKDAYINMONTH%%                ()                                                                 Returns the number of days in the month so far that have the same name as the current day
'FUNCTION  WEEKDAYINMONTH_X%%              (t AS SYSTEMTIME)                                                  Returns the number of days in the month so far of the date specified in a SYSTEMTIME that have the same name as the current day
'FUNCTION  WEEK_X%%                        (t AS SYSTEMTIME)                                                  Returns the week number of the year of a date specified in a SYSTEMTIME
'FUNCTION  YEAR%                           ()                                                                 Returns the year value of the date
'FUNCTION  YEAR_CYCLE%                     ()                                                                 Returns the number of days since the beginning of the year
'FUNCTION  YEAR_CYCLE_X%                   (t AS SYSTEMTIME)                                                  Returns the number of days since the beginning of the year of a SYSTEMTIME
'FUNCTION  YEAR_SHORT%%                    ()                                                                 Returns the two digit year value of the date
'FUNCTION  YEAR_SHORT_X%%                  ()                                                                 Returns the two digit year value of the date specified on a SYSTEMTIME
'FUNCTION  YEAR_X%                         ()                                                                 Returns the year value of the date specified on a SYSTEMTIME
'FUNCTION  ZODIAC$                         ()                                                                 Returns the Zodiac sign for the current day of the year
'FUNCTION  ZODIAC_X$                       (t AS SYSTEMTIME)                                                  Returns the Zodiac sign for the respective day of the year of a SYSTEMTIME
'
'******************************************************************************
'
'Paste this code into your program as described in the tutorial:
'https://db.tt/e5g4mhnx
'
'Include this file at the beginning of your program:
'https://db.tt/0jJcZPLi
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/KrokTCWa
'
'******************************************************************************

SUB CLEARDATE (t AS SYSTEMTIME)
t.wYear = 1601
t.wMonth = 1
t.wDay = 1
END SUB

SUB CLEARTIME (t AS SYSTEMTIME)
t.wHour = 0
t.wMinute = 0
t.wSecond = 0
t.wMilliseconds = 0
END SUB

SUB CURRENTTIME (t AS SYSTEMTIME)
GetSystemTime _OFFSET(t)
END SUB

SUB LUNAR_LAST_FULL (t AS SYSTEMTIME)
DIM x AS _BYTE
CURRENTTIME t
CLEARTIME t
FOR x = 1 TO 30
    t.wDay = t.wDay - 1
    IF t.wDay = 0 THEN
        t.wMonth = t.wMonth - 1
        IF t.wMonth = 0 THEN
            t.wYear = t.wYear - 1
            t.wMonth = 12
        END IF
        t.wDay = MONTH_DAYS(t.wMonth)
    END IF
    IF LUNAR_PHASE_X(t) = "Full" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_LAST_FULL_X (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM x AS _BYTE
t2 = t1
CLEARTIME t2
FOR x = 1 TO 30
    t2.wDay = t2.wDay - 1
    IF t2.wDay = 0 THEN
        t2.wMonth = t2.wMonth - 1
        IF t2.wMonth = 0 THEN
            t2.wYear = t2.wYear - 1
            t2.wMonth = 12
        END IF
        t2.wDay = MONTH_DAYS(t2.wMonth)
    END IF
    IF LUNAR_PHASE_X(t2) = "Full" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_LAST_NEW (t AS SYSTEMTIME)
DIM x AS _BYTE
CURRENTTIME t
CLEARTIME t
FOR x = 1 TO 30
    t.wDay = t.wDay - 1
    IF t.wDay = 0 THEN
        t.wMonth = t.wMonth - 1
        IF t.wMonth = 0 THEN
            t.wYear = t.wYear - 1
            t.wMonth = 12
        END IF
        t.wDay = MONTH_DAYS(t.wMonth)
    END IF
    IF LUNAR_PHASE_X(t) = "New" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_LAST_NEW_X (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM x AS _BYTE
t2 = t1
CLEARTIME t2
FOR x = 1 TO 30
    t2.wDay = t2.wDay - 1
    IF t2.wDay = 0 THEN
        t2.wMonth = t2.wMonth - 1
        IF t2.wMonth = 0 THEN
            t2.wYear = t2.wYear - 1
            t2.wMonth = 12
        END IF
        t2.wDay = MONTH_DAYS(t2.wMonth)
    END IF
    IF LUNAR_PHASE_X(t2) = "New" THEN EXIT FOR
NEXT x
END FUNCTION

SUB MAKETIME (t AS SYSTEMTIME, y AS INTEGER, m AS _BYTE, d AS _BYTE, h AS _BYTE, n AS _BYTE, s AS _BYTE, f AS INTEGER)
t.wYear = y
t.wMonth = m
t.wDay = d
t.wHour = h
t.wMinute = n
t.wSecond = s
t.wMilliseconds = f
END SUB

SUB LUNAR_NEXT_FULL (t AS SYSTEMTIME)
DIM x AS _BYTE
CURRENTTIME t
CLEARTIME t
FOR x = 1 TO 30
    t.wDay = t.wDay + 1
    IF t.wDay > MONTH_DAYS(t.wMonth) THEN
        t.wMonth = t.wMonth + 1
        IF t.wMonth = 13 THEN
            t.wYear = t.wYear + 1
            t.wMonth = 1
        END IF
        t.wDay = 1
    END IF
    IF LUNAR_PHASE_X(t) = "Full" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_NEXT_FULL_X (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM x AS _BYTE
t2 = t1
CLEARTIME t2
FOR x = 1 TO 30
    t2.wDay = t2.wDay + 1
    IF t2.wDay > MONTH_DAYS(t2.wMonth) THEN
        t2.wMonth = t2.wMonth + 1
        IF t2.wMonth = 13 THEN
            t2.wYear = t2.wYear + 1
            t2.wMonth = 1
        END IF
        t2.wDay = 1
    END IF
    IF LUNAR_PHASE_X(t2) = "Full" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_NEXT_NEW (t AS SYSTEMTIME)
DIM x AS _BYTE
CURRENTTIME t
CLEARTIME t
FOR x = 1 TO DaysInMonth
    t.wDay = t.wDay + 1
    IF t.wDay > MONTH_DAYS(t.wMonth) THEN
        t.wMonth = t.wMonth + 1
        IF t.wMonth = 13 THEN
            t.wYear = t.wYear + 1
            t.wMonth = 1
        END IF
        t.wDay = 1
    END IF
    IF LUNAR_PHASE_X(t) = "New" THEN EXIT FOR
NEXT x
END FUNCTION

SUB LUNAR_NEXT_NEW_X (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM x AS _BYTE
t2 = t1
CLEARTIME t2
FOR x = 1 TO DaysInMonth
    t2.wDay = t2.wDay + 1
    IF t2.wDay > MONTH_DAYS(t2.wMonth) THEN
        t2.wMonth = t2.wMonth + 1
        IF t2.wMonth = 13 THEN
            t2.wYear = t2.wYear + 1
            t2.wMonth = 1
        END IF
        t2.wDay = 1
    END IF
    IF LUNAR_PHASE_X(t2) = "New" THEN EXIT FOR
NEXT x
END FUNCTION

SUB REPAIRTIME (t AS SYSTEMTIME)
t.wSecond = t.wSecond + t.wMilliseconds \ MillisecondsInSecond
t.wMilliseconds = t.wMilliseconds MOD MillisecondsInSecond
t.wMinute = t.wMinute + t.wSecond \ SecondsInMinute
t.wSecond = t.wSecond MOD SecondsInMinute
t.wHour = t.wHour + t.wMinute \ MinutesInHour
t.wMinute = t.wMinute MOD MinutesInHour
t.wDay = t.wDay + t.wHour \ HoursInDay
t.wHour = t.wHour MOD HoursInDay
t.wYear = t.wYear + (t.wMonth - 1) \ MonthsInYear
t.wMonth = t.wMonth MOD 12
IF t.wMonth = 0 THEN t.wMonth = 1
IF t.wDay > MONTH_DAYS(t.wMonth) THEN
    t.wMonth = t.wMonth + 1
    t.wDay = 1
END IF
t.wYear = t.wYear + (t.wMonth - 1) \ MonthsInYear
t.wMonth = t.wMonth MOD MonthsInYear
IF t.wMonth = 0 THEN t.wMonth = 1
END SUB

SUB TIMEAFTER (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, y AS INTEGER, m AS _BYTE, d AS _BYTE, h AS _BYTE, n AS _BYTE, s AS _BYTE)
DIM cft AS _INTEGER64
DIM ft AS FILETIME
cft = TIMEAFTER(t1, y, m, d, h, n, s)
ft.dwHighDateTime = INT(cft / 4294967295)
ft.dwLowDateTime = cft MOD 4294967295
FileTimeToSystemTime _OFFSET(ft), _OFFSET(t2)
IF t2.wMilliseconds >= MillisecondsInSecond / 2 THEN t2.wSecond = t2.wSecond + 1
t2.wMilliseconds = 0
REPAIRTIME t2
END SUB

SUB TIMEBEFORE (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, y AS INTEGER, m AS _BYTE, d AS _BYTE, h AS _BYTE, n AS _BYTE, s AS _BYTE)
DIM cft AS _INTEGER64
DIM ft AS FILETIME
cft = TIMEBEFORE(t1, y, m, d, h, n, s)
ft.dwHighDateTime = INT(cft / 4294967295)
ft.dwLowDateTime = cft MOD 4294967295
FileTimeToSystemTime _OFFSET(ft), _OFFSET(t2)
IF t2.wMilliseconds >= MillisecondsInSecond / 2 THEN t2.wSecond = t2.wSecond + 1
t2.wMilliseconds = 0
REPAIRTIME t2
END SUB

SUB TIMEBETWEEN (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME, t3 AS SYSTEMTIME)
DIM cft AS _INTEGER64
DIM ft3 AS FILETIME
cft = TIMEBETWEEN(t1, t2)
ft3.dwHighDateTime = INT(cft / 4294967295)
ft3.dwLowDateTime = cft MOD 4294967295
FileTimeToSystemTime _OFFSET(ft3), _OFFSET(t3)
IF t3.wMilliseconds >= MillisecondsInSecond / 2 THEN t3.wSecond = t3.wSecond + 1
t3.wMilliseconds = 0
REPAIRTIME t3
t3.wYear = t3.wYear - 1601
t3.wMonth = t3.wMonth - 1
t3.wDay = t3.wDay - 2
END SUB

FUNCTION AMPM` ()
AMPM = HOUR >= 12
END FUNCTION

FUNCTION AMPM_X` (t AS SYSTEMTIME)
AMPM_X = HOUR_X(t) >= 12
END FUNCTION

FUNCTION ASTROLOGICALAGE$ ()
DIM c AS _BYTE
c = 1 - INT((YEAR - ASTROLOGICALAGE_LENGTH) / ASTROLOGICALAGE_LENGTH)
IF c < 0 THEN c = c + 12
c = c MOD 12
SELECT CASE c
    CASE 0
        ASTROLOGICALAGE = "Capricorn"
    CASE 1
        ASTROLOGICALAGE = "Aquarius"
    CASE 2
        ASTROLOGICALAGE = "Pisces"
    CASE 3
        ASTROLOGICALAGE = "Aries"
    CASE 4
        ASTROLOGICALAGE = "Taurus"
    CASE 5
        ASTROLOGICALAGE = "Gemini"
    CASE 6
        ASTROLOGICALAGE = "Cancer"
    CASE 7
        ASTROLOGICALAGE = "Leo"
    CASE 8
        ASTROLOGICALAGE = "Virgo"
    CASE 9
        ASTROLOGICALAGE = "Libra"
    CASE 10
        ASTROLOGICALAGE = "Scorpio"
    CASE 11
        ASTROLOGICALAGE = "Sagittarius"
END SELECT
END FUNCTION

FUNCTION ASTROLOGICALAGE_CYCLE% ()
ASTROLOGICALAGE_CYCLE = YEAR - ASTROLOGICALAGE_START
END FUNCTION

FUNCTION ASTROLOGICALAGE_CYCLE_X% (t AS SYSTEMTIME)
ASTROLOGICALAGE_CYCLE_X = YEAR_X(t) - ASTROLOGICALAGE_START_X(t)
END FUNCTION

FUNCTION ASTROLOGICALAGE_END& ()
ASTROLOGICALAGE_END = ASTROLOGICALAGE_START + ASTROLOGICALAGE_LENGTH
END FUNCTION

FUNCTION ASTROLOGICALAGE_END_X& (t AS SYSTEMTIME)
ASTROLOGICALAGE_END_X = ASTROLOGICALAGE_START_X(t) + ASTROLOGICALAGE_LENGTH
END FUNCTION

FUNCTION ASTROLOGICALAGE_LEFT% ()
ASTROLOGICALAGE_LEFT = ASTROLOGICALAGE_END - YEAR
END FUNCTION

FUNCTION ASTROLOGICALAGE_LEFT_X% (t AS SYSTEMTIME)
ASTROLOGICALAGE_LEFT_X = ASTROLOGICALAGE_END_X(t) - YEAR_X(t)
END FUNCTION

FUNCTION ASTROLOGICALAGE_START& ()
ASTROLOGICALAGE_START = (INT((YEAR - ASTROLOGICALAGE_LENGTH) / ASTROLOGICALAGE_LENGTH) - (YEAR > ASTROLOGICALAGE_LENGTH) * ASTROLOGICALAGE_LENGTH) + 1
END FUNCTION

FUNCTION ASTROLOGICALAGE_START_X& (t AS SYSTEMTIME)
ASTROLOGICALAGE_START_X = (INT((YEAR_X(t) - ASTROLOGICALAGE_LENGTH) / ASTROLOGICALAGE_LENGTH) - (YEAR_X(t) > ASTROLOGICALAGE_LENGTH) * ASTROLOGICALAGE_LENGTH) + 1
END FUNCTION

FUNCTION ASTROLOGICALAGE_X$ (t AS SYSTEMTIME)
DIM c AS _BYTE
c = 1 - INT((YEAR_X(t) - ASTROLOGICALAGE_LENGTH) / ASTROLOGICALAGE_LENGTH)
IF c < 0 THEN c = c + 12
c = c MOD 12
SELECT CASE c
    CASE 0
        ASTROLOGICALAGE_X = "Capricorn"
    CASE 1
        ASTROLOGICALAGE_X = "Aquarius"
    CASE 2
        ASTROLOGICALAGE_X = "Pisces"
    CASE 3
        ASTROLOGICALAGE_X = "Aries"
    CASE 4
        ASTROLOGICALAGE_X = "Taurus"
    CASE 5
        ASTROLOGICALAGE_X = "Gemini"
    CASE 6
        ASTROLOGICALAGE_X = "Cancer"
    CASE 7
        ASTROLOGICALAGE_X = "Leo"
    CASE 8
        ASTROLOGICALAGE_X = "Virgo"
    CASE 9
        ASTROLOGICALAGE_X = "Libra"
    CASE 10
        ASTROLOGICALAGE_X = "Scorpio"
    CASE 11
        ASTROLOGICALAGE_X = "Sagittarius"
END SELECT
END FUNCTION

FUNCTION BIRTHSTONE$ () 'http://www.americangemsociety.org/birthstones
SELECT CASE MONTH
    CASE JANUARY
        BIRTHSTONE = "Garnet"
    CASE FEBRUARY
        BIRTHSTONE = "Amethyst"
    CASE MARCH
        BIRTHSTONE = "Aquamarine"
    CASE APRIL
        BIRTHSTONE = "Diamond"
    CASE MAY
        BIRTHSTONE = "Emerald"
    CASE JUNE
        BIRTHSTONE = "Pearl"
    CASE JULY
        BIRTHSTONE = "Ruby"
    CASE AUGUST
        BIRTHSTONE = "Peridot"
    CASE SEPTEMBER
        BIRTHSTONE = "Sapphire"
    CASE OCTOBER
        BIRTHSTONE = "Opal"
    CASE NOVEMBER
        BIRTHSTONE = "Topaz"
    CASE DECEMBER
        BIRTHSTONE = "Turqoise"
END SELECT
END FUNCTION

FUNCTION BIRTHSTONE_X$ (t AS SYSTEMTIME)
SELECT CASE MONTH_X(t)
    CASE JANUARY
        BIRTHSTONE_X = "Garnet"
    CASE FEBRUARY
        BIRTHSTONE_X = "Amethyst"
    CASE MARCH
        BIRTHSTONE_X = "Aquamarine"
    CASE APRIL
        BIRTHSTONE_X = "Diamond"
    CASE MAY
        BIRTHSTONE_X = "Emerald"
    CASE JUNE
        BIRTHSTONE_X = "Pearl"
    CASE JULY
        BIRTHSTONE_X = "Ruby"
    CASE AUGUST
        BIRTHSTONE_X = "Peridot"
    CASE SEPTEMBER
        BIRTHSTONE_X = "Sapphire"
    CASE OCTOBER
        BIRTHSTONE_X = "Opal"
    CASE NOVEMBER
        BIRTHSTONE_X = "Topaz"
    CASE DECEMBER
        BIRTHSTONE_X = "Turqoise"
END SELECT
END FUNCTION

FUNCTION DATETIME$ ()
DATETIME = DATE$ + " " + TIME$
END FUNCTION

FUNCTION DATETIME_12$ ()
DATETIME_12 = DATE$ + " " + TIME_12
END FUNCTION

FUNCTION DATETIME_12_SHORTYEAR$ ()
DATETIME_12_SHORTYEAR = DATE_SHORT + " " + TIME_12
END FUNCTION

FUNCTION DATETIME_12_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_12_SHORTYEAR_X = DATE_SHORT_X(t) + " " + TIME_12_X(t)
END FUNCTION

FUNCTION DATETIME_12_X$ (t AS SYSTEMTIME)
DATETIME_12_X = DATE_X(t) + " " + TIME_12_X(t)
END FUNCTION

FUNCTION DATETIME_LONG$ ()
DATETIME_LONG = DATETIME + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION DATETIME_LONG_12$ ()
DATETIME_LONG_12 = DATETIME_12 + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION DATETIME_LONG_12_SHORTYEAR$ ()
DATETIME_LONG_12_SHORTYEAR = DATETIME_12_SHORTYEAR + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION DATETIME_LONG_12_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_LONG_12_SHORTYEAR_X = DATETIME_12_SHORTYEAR_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION DATETIME_LONG_12_X$ (t AS SYSTEMTIME)
DATETIME_LONG_12_X = DATETIME_12_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION DATETIME_LONG_SHORTYEAR$ ()
DATETIME_LONG_SHORTYEAR = DATETIME_SHORTYEAR + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION DATETIME_LONG_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_LONG_SHORTYEAR_X = DATETIME_SHORTYEAR_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION DATETIME_LONG_X$ (t AS SYSTEMTIME)
DATETIME_LONG_X = DATETIME_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION DATETIME_SHORT$ ()
DATETIME_SHORT = LEFT$(DATETIME, 16)
END FUNCTION

FUNCTION DATETIME_SHORTYEAR$ ()
DATETIME_SHORTYEAR = DATE_SHORT + " " + TIME$
END FUNCTION

FUNCTION DATETIME_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_SHORTYEAR_X = DATE_SHORT_X(t) + " " + TIME_X(t)
END FUNCTION

FUNCTION DATETIME_SHORT_12$ ()
DATETIME_SHORT_12 = LEFT$(DATETIME_12, 16)
END FUNCTION

FUNCTION DATETIME_SHORT_12_SHORTYEAR$ ()
DATETIME_SHORT_12_SHORTYEAR = LEFT$(DATETIME_12_SHORTYEAR, 14)
END FUNCTION

FUNCTION DATETIME_SHORT_12_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_SHORT_12_SHORTYEAR_X = LEFT$(DATETIME_12_SHORTYEAR_X(t), 14)
END FUNCTION

FUNCTION DATETIME_SHORT_12_X$ (t AS SYSTEMTIME)
DATETIME_SHORT_12_X = LEFT$(DATETIME_12_X(t), 16)
END FUNCTION

FUNCTION DATETIME_SHORT_SHORTYEAR$ ()
DATETIME_SHORT_SHORTYEAR = LEFT$(DATETIME_SHORTYEAR, 14)
END FUNCTION

FUNCTION DATETIME_SHORT_SHORTYEAR_X$ (t AS SYSTEMTIME)
DATETIME_SHORT_SHORTYEAR_X = LEFT$(DATETIME_SHORTYEAR_X(t), 14)
END FUNCTION

FUNCTION DATETIME_SHORT_X$ (t AS SYSTEMTIME)
DATETIME_SHORT_X = LEFT$(DATETIME_X(t), 16)
END FUNCTION

FUNCTION DATETIME_X$ (t AS SYSTEMTIME)
DATETIME_X = DATE_X(t) + " " + TIME_X(t)
END FUNCTION

FUNCTION DATE_SHORT$ ()
DATE_SHORT = LEFT$(DATE$, 6) + zeroset$(YEAR_SHORT, 2)
END FUNCTION

FUNCTION DATE_SHORT_X$ (t AS SYSTEMTIME)
DATE_SHORT_X = LEFT$(DATE_X(t), 6) + zeroset$(YEAR_SHORT_X(t), 2)
END FUNCTION

FUNCTION DATE_X$ (t AS SYSTEMTIME)
DATE_X = zeroset$(MONTH_X(t), 2) + "-" + zeroset$(DAY_X(t), 2) + "-" + TRIMnum$(YEAR_X(t))
END FUNCTION

FUNCTION DAY%% ()
DAY = VAL(MID$(DATE$, 4, 2))
END FUNCTION

FUNCTION DAY_X%% (t AS SYSTEMTIME)
DAY_X = t.wDay
END FUNCTION

FUNCTION DAYNAME$ ()
SELECT CASE DAYOFWEEK
    CASE SUNDAY
        DAYNAME = "Sunday"
    CASE MONDAY
        DAYNAME = "Monday"
    CASE TUESDAY
        DAYNAME = "Tuesday"
    CASE WEDNESDAY
        DAYNAME = "Wednesday"
    CASE THURSDAY
        DAYNAME = "Thursday"
    CASE FRIDAY
        DAYNAME = "Friday"
    CASE SATURDAY
        DAYNAME = "Saturday"
END SELECT
END FUNCTION

FUNCTION DAYNAME_X$ (t AS SYSTEMTIME)
SELECT CASE DAYOFWEEK_X(t)
    CASE SUNDAY
        DAYNAME_X = "Sunday"
    CASE MONDAY
        DAYNAME_X = "Monday"
    CASE TUESDAY
        DAYNAME_X = "Tuesday"
    CASE WEDNESDAY
        DAYNAME_X = "Wednesday"
    CASE THURSDAY
        DAYNAME_X = "Thursday"
    CASE FRIDAY
        DAYNAME_X = "Friday"
    CASE SATURDAY
        DAYNAME_X = "Saturday"
END SELECT
END FUNCTION

FUNCTION DAYOFWEEK%% () 'Ted Weissgerber
DIM c AS _BYTE
DIM d AS _BYTE
DIM m AS _BYTE
DIM y AS INTEGER
d = DAY
m = MONTH
y = YEAR
IF m < 3 THEN
    m = m + 12
    y = y - 1
END IF
c = y \ YearsInCentury
y = y MOD 100
DAYOFWEEK = (((c \ 4) - (2 * c) - 1) + ((5 * y) \ 4) + (26 * (m + 1) \ 10) + d) MOD DaysInWeek
IF DAYOFWEEK < 0 THEN DAYOFWEEK = DAYOFWEEK + DaysInWeek
END FUNCTION

FUNCTION DAYOFWEEK_X%% (t AS SYSTEMTIME)
DIM c AS _BYTE
DIM d AS _BYTE
DIM m AS _BYTE
DIM y AS INTEGER
d = DAY_X(t)
m = MONTH_X(t)
y = YEAR_X(t)
IF m < 3 THEN
    m = m + 12
    y = y - 1
END IF
c = y \ YearsInCentury
y = y MOD 100
DAYOFWEEK_X = (((c \ 4) - (2 * c) - 1) + ((5 * y) \ 4) + (26 * (m + 1) \ 10) + d) MOD DaysInWeek
IF DAYOFWEEK_X < 0 THEN DAYOFWEEK_X = DAYOFWEEK_X + DaysInWeek
END FUNCTION

FUNCTION HOLIDAY$ () 'http://www.smart.net/~mmontes/ushols.html
SELECT CASE MONTH
    CASE JANUARY
        SELECT CASE DAY
            CASE 1
                HOLIDAY = HOLIDAY + ", New Year's Day"
            CASE 20
                IF YEAR > 1936 AND (YEAR - 1937) MOD 4 = 0 THEN HOLIDAY = HOLIDAY + ", Inauguration Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE MONDAY
                IF WEEKDAYINMONTH = 3 THEN HOLIDAY = HOLIDAY + ", Martin Luther King Jr. Day"
        END SELECT
    CASE FEBRUARY
        SELECT CASE DAY
            CASE 2
                HOLIDAY = HOLIDAY + ", Groundhog Day"
            CASE 12
                HOLIDAY = HOLIDAY + ", Lincoln's Birthday"
            CASE 14
                HOLIDAY = HOLIDAY + ", Valentine's Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE MONDAY
                IF WEEKDAYINMONTH = 3 THEN HOLIDAY = HOLIDAY + ", Washington's Birthday"
        END SELECT
    CASE MARCH
        SELECT CASE DAY
            CASE 14
                HOLIDAY = HOLIDAY + ", Pi Day"
            CASE 17
                HOLIDAY = HOLIDAY + ", St. Patrick's Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE SUNDAY
                IF WEEKDAYINMONTH = 2 THEN HOLIDAY = HOLIDAY + ", Start of Daylight Savings Time"
        END SELECT
    CASE APRIL
        SELECT CASE DAY
            CASE 1
                HOLIDAY = HOLIDAY + ", April Fool's Day"
            CASE 22
                HOLIDAY = HOLIDAY + ", Earth Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE FRIDAY
                IF DAY > MONTH_DAYS(4) - 7 THEN HOLIDAY = HOLIDAY + ", Arbor Day"
        END SELECT
    CASE MAY
        SELECT CASE DAYOFWEEK
            CASE SUNDAY
                IF WEEKDAYINMONTH = 2 THEN HOLIDAY = HOLIDAY + ", Mother's Day"
            CASE MONDAY
                IF DAY > MONTH_DAYS(5) - 7 THEN HOLIDAY = HOLIDAY + ", Memorial Day"
            CASE SATURDAY
                IF WEEKDAYINMONTH = 3 THEN HOLIDAY = HOLIDAY + ", Armed Forces Day"
        END SELECT
    CASE JUNE
        SELECT CASE DAY
            CASE 14
                HOLIDAY = HOLIDAY + ", Flag Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE SUNDAY
                IF WEEKDAYINMONTH = 3 THEN HOLIDAY = HOLIDAY + ", Father's Day"
                IF WEEKDAYINMONTH = 4 THEN HOLIDAY = HOLIDAY + ", Parent's Day"
        END SELECT
    CASE JULY
        SELECT CASE DAY
            CASE 4
                HOLIDAY = HOLIDAY + ", Independence Day"
        END SELECT
    CASE SEPTEMBER
        SELECT CASE DAYOFWEEK
            CASE SUNDAY
                IF (WEEKDAYINMONTH = 1 AND DAY = 7) OR (WEEKDAYINMONTH = 2 AND DAY <> 14) THEN HOLIDAY = HOLIDAY + ", Grandparents' Day"
            CASE MONDAY
                IF WEEKDAYINMONTH = 1 THEN HOLIDAY = HOLIDAY + ", Labor Day"
        END SELECT
    CASE OCTOBER
        SELECT CASE DAY
            CASE 23
                HOLIDAY = HOLIDAY + ", Mole Day"
            CASE 24
                HOLIDAY = HOLIDAY + ", United Nations Day"
            CASE 31
                HOLIDAY = HOLIDAY + ", Halloween"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE MONDAY
                IF WEEKDAYINMONTH = 2 THEN HOLIDAY = HOLIDAY + ", Columbus Day"
        END SELECT
    CASE NOVEMBER
        SELECT CASE DAY
            CASE 11
                HOLIDAY = HOLIDAY + ", Veteran's Day"
        END SELECT
        SELECT CASE DAYOFWEEK
            CASE SUNDAY
                IF WEEKDAYINMONTH = 1 THEN HOLIDAY = HOLIDAY + ", End of Daylight Savings Time"
            CASE TUESDAY
                IF (WEEKDAYINMONTH = 1 AND DAY > 1) OR DAY = 8 THEN HOLIDAY = HOLIDAY + ", Election Day"
            CASE THURSDAY
                IF WEEKDAYINMONTH = 4 THEN HOLIDAY = HOLIDAY + ", Thanksgiving Day"
        END SELECT
    CASE DECEMBER
        SELECT CASE DAY
            CASE 24
                HOLIDAY = HOLIDAY + ", Christmas Eve"
            CASE 25
                HOLIDAY = HOLIDAY + ", Christmas Day"
            CASE 31
                HOLIDAY = HOLIDAY + ", New Year's Eve"
        END SELECT
END SELECT
HOLIDAY = MID$(HOLIDAY, 3)
END FUNCTION

FUNCTION HOLIDAY_X$ (t AS SYSTEMTIME)
SELECT CASE MONTH_X(t)
    CASE JANUARY
        SELECT CASE DAY_X(t)
            CASE 1
                HOLIDAY_X = HOLIDAY_X + ", New Year's Day"
            CASE 20
                IF (YEAR_X(t) - 1937) MOD 4 = 0 THEN HOLIDAY_X = HOLIDAY_X + ", Inauguration Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE MONDAY
                IF WEEKDAYINMONTH_X(t) = 3 THEN HOLIDAY_X = HOLIDAY_X + ", Martin Luther King Jr. Day"
        END SELECT
    CASE FEBRUARY
        SELECT CASE DAY_X(t)
            CASE 2
                HOLIDAY_X = HOLIDAY_X + ", Groundhog Day"
            CASE 12
                HOLIDAY_X = HOLIDAY_X + ", Lincoln's Birthday"
            CASE 14
                HOLIDAY_X = HOLIDAY_X + ", Valentine's Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE MONDAY
                IF WEEKDAYINMONTH_X(t) = 3 THEN HOLIDAY_X = HOLIDAY_X + ", Washington's Birthday"
        END SELECT
    CASE MARCH
        SELECT CASE DAY_X(t)
            CASE 14
                HOLIDAY_X = HOLIDAY_X + ", Pi Day"
            CASE 17
                HOLIDAY_X = HOLIDAY_X + ", St. Patrick's Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE SUNDAY
                IF WEEKDAYINMONTH_X(t) = 2 THEN HOLIDAY_X = HOLIDAY_X + ", Start of Daylight Savings Time"
        END SELECT
    CASE APRIL
        SELECT CASE DAY_X(t)
            CASE 1
                HOLIDAY_X = HOLIDAY_X + ", April Fool's Day"
            CASE 22
                HOLIDAY_X = HOLIDAY_X + ", Earth Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE FRIDAY
                IF DAY_X(t) > MONTH_DAYS(4) - 7 THEN HOLIDAY_X = HOLIDAY_X + ", Arbor Day"
        END SELECT
    CASE MAY
        SELECT CASE DAYOFWEEK_X(t)
            CASE SUNDAY
                IF WEEKDAYINMONTH_X(t) = 2 THEN HOLIDAY_X = HOLIDAY_X + ", Mother's Day"
            CASE MONDAY
                IF DAY_X(t) > MONTH_DAYS(5) - 7 THEN HOLIDAY_X = HOLIDAY_X + ", Memorial Day"
            CASE FRIDAY
                IF WEEKDAYINMONTH_X(t) = 3 THEN HOLIDAY_X = HOLIDAY_X + ", Armed Forces Day"
        END SELECT
    CASE JUNE
        SELECT CASE DAY_X(t)
            CASE 14
                HOLIDAY_X = HOLIDAY_X + ", Flag Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE SUNDAY
                IF WEEKDAYINMONTH_X(t) = 3 THEN HOLIDAY_X = HOLIDAY_X + ", Father's Day"
                IF WEEKDAYINMONTH_X(t) = 4 THEN HOLIDAY_X = HOLIDAY_X + ", Parent's Day"
        END SELECT
    CASE JULY
        SELECT CASE DAY_X(t)
            CASE 4
                HOLIDAY_X = HOLIDAY_X + ", Independence Day"
        END SELECT
    CASE SEPTEMBER
        SELECT CASE DAYOFWEEK_X(t)
            CASE SUNDAY
                IF (WEEKDAYINMONTH_X(t) = 1 AND DAY_X(t) = 7) OR (WEEKDAYINMONTH_X(t) = 2 AND DAY_X(t) <> 14) THEN HOLIDAY_X = HOLIDAY_X + ", Grandparents' Day"
            CASE MONDAY
                IF WEEKDAYINMONTH_X(t) = 1 THEN HOLIDAY_X = HOLIDAY_X + ", Labor Day"
        END SELECT
    CASE OCTOBER
        SELECT CASE DAY_X(t)
            CASE 23
                HOLIDAY_X = HOLIDAY_X + ", Mole Day"
            CASE 24
                HOLIDAY_X = HOLIDAY_X + ", United Nations Day"
            CASE 31
                HOLIDAY_X = HOLIDAY_X + ", Halloween"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE MONDAY
                IF WEEKDAYINMONTH_X(t) = 2 THEN HOLIDAY_X = HOLIDAY_X + ", Columbus Day"
        END SELECT
    CASE NOVEMBER
        SELECT CASE DAY_X(t)
            CASE 11
                HOLIDAY_X = HOLIDAY_X + ", Veteran's Day"
        END SELECT
        SELECT CASE DAYOFWEEK_X(t)
            CASE SUNDAY
                IF WEEKDAYINMONTH_X(t) = 1 THEN HOLIDAY_X = HOLIDAY_X + ", End of Daylight Savings Time"
            CASE TUESDAY
                IF (WEEKDAYINMONTH_X(t) = 1 AND DAY_X(t) > 1) OR DAY_X(t) = 8 THEN HOLIDAY_X = HOLIDAY_X + ", Election Day"
            CASE THURSDAY
                IF WEEKDAYINMONTH_X(t) = 4 THEN HOLIDAY_X = HOLIDAY_X + ", Thanksgiving Day"
        END SELECT
    CASE DECEMBER
        SELECT CASE DAY_X(t)
            CASE 24
                HOLIDAY_X = HOLIDAY_X + ", Christmas Eve"
            CASE 25
                HOLIDAY_X = HOLIDAY_X + ", Christmas Day"
            CASE 31
                HOLIDAY_X = HOLIDAY_X + ", New Year's Eve"
        END SELECT
END SELECT
HOLIDAY_X = MID$(HOLIDAY_X, 3)
END FUNCTION

FUNCTION HOUR%% ()
HOUR = VAL(LEFT$(TIME$, 2))
END FUNCTION

FUNCTION HOUR_12%% ()
HOUR_12 = HOUR
IF HOUR_12 > 12 THEN HOUR_12 = HOUR_12 - 12
END FUNCTION

FUNCTION HOUR_12_X%% (t AS SYSTEMTIME)
HOUR_12_X = HOUR_X(t)
IF HOUR_12_X > 12 THEN HOUR_12_X = HOUR_12_X - 12
END FUNCTION

FUNCTION HOUR_X%% (t AS SYSTEMTIME)
HOUR_X = t.wHour
END FUNCTION

FUNCTION JULIANDAY& () 'http://en.wikipedia.org/wiki/Julian_day#Calculation
DIM a AS _BYTE
DIM d AS _BYTE
DIM m AS _BYTE
DIM y AS INTEGER
a = INT((14 - MONTH) / MonthsInYear)
y = YEAR + 4800 - INT((14 - MONTH) / MonthsInYear)
IF y < 0 THEN y = y + 1
m = MONTH + (MonthsInYear * a) - 3
JULIANDAY = DAY + INT((153 * m + 2) / 5) + (365 * y) + INT(y / 4) - INT(y / YearsInCentury) + INT(y / 400) - 32045
END FUNCTION

FUNCTION JULIANDAY_LONG# ()
JULIANDAY_LONG = JULIANDAY + (HOUR / HoursInDay) + (MINUTE / MinutesInDay) + (SECOND / SecondsInDay)
END FUNCTION

FUNCTION JULIANDAY_LONG_X# (t AS SYSTEMTIME)
JULIANDAY_LONG_X = JULIANDAY_X(t) + (HOUR_X(t) / HoursInDay) + (MINUTE_X(t) / MinutesInDay) + (SECOND_X(t) / SecondsInDay)
END FUNCTION

FUNCTION JULIANDAY_X& (t AS SYSTEMTIME)
DIM a AS _BYTE
DIM d AS _BYTE
DIM m AS _BYTE
DIM y AS INTEGER
a = INT((14 - MONTH_X(t)) / MonthsInYear)
y = YEAR_X(t) + 4800 - INT((14 - MONTH_X(t)) / MonthsInYear)
IF y < 0 THEN y = y + 1
m = MONTH_X(t) + (MonthsInYear * a) - 3
JULIANDAY_X = DAY_X(t) + INT((153 * m + 2) / 5) + (365 * y) + INT(y / 4) - INT(y / YearsInCentury) + INT(y / 400) - 32045
END FUNCTION

FUNCTION LEAPYEAR` ()
LEAPYEAR = (YEAR MOD 4 = 0 AND YEAR MOD YearsInCentury <> 0) OR YEAR MOD 400 = 0
END FUNCTION

FUNCTION LEAPYEAR_X` (t AS SYSTEMTIME)
LEAPYEAR_X = (YEAR_X(t) MOD 4 = 0 AND YEAR_X(t) MOD YearsInCentury <> 0) OR YEAR_X(t) MOD 400 = 0
END FUNCTION

FUNCTION LONGCOUNT$ () 'http://en.wikipedia.org/wiki/Mesoamerican_Long_Count_calendar
DIM d AS LONG
d = LONGCOUNT_DAYS
LONGCOUNT = TRIMnum$(d MOD KinInWinal)
d = INT(d / KinInWinal)
LONGCOUNT = TRIMnum$(d MOD WinalInTun) + "." + LONGCOUNT
d = INT(d / WinalInTun)
LONGCOUNT = TRIMnum$(d MOD TunInKatun) + "." + LONGCOUNT
d = INT(d / TunInKatun)
LONGCOUNT = TRIMnum$(d MOD KatunInBaktun) + "." + LONGCOUNT
d = INT(d / KatunInBaktun)
LONGCOUNT = TRIMnum$(d MOD KinInWinal) + "." + LONGCOUNT
END FUNCTION

FUNCTION LONGCOUNT_BAKTUN%% ()
LONGCOUNT_BAKTUN = INT(INT(INT(INT(LONGCOUNT_DAYS / KinInWinal) / WinalInTun) / TunInKatun) / KatunInBaktun) MOD 20
END FUNCTION

FUNCTION LONGCOUNT_BAKTUN_X%% (t AS SYSTEMTIME)
LONGCOUNT_BAKTUN_X = INT(INT(INT(INT(LONGCOUNT_DAYS_X(t) / KinInWinal) / WinalInTun) / TunInKatun) / KatunInBaktun) MOD 20
END FUNCTION

FUNCTION LONGCOUNT_CALENDARROUND$ ()
LONGCOUNT_CALENDARROUND = LONGCOUNT_TZOLKIN + " " + LONGCOUNT_HAAB
END FUNCTION

FUNCTION LONGCOUNT_CALENDARROUND_X$ (t AS SYSTEMTIME)
LONGCOUNT_CALENDARROUND_X = LONGCOUNT_TZOLKIN_X(t) + " " + LONGCOUNT_HAAB_X(t)
END FUNCTION

FUNCTION LONGCOUNT_DAYS& ()
LONGCOUNT_DAYS = JULIANDAY - JULIANDAY_LONGCOUNT_OFFSET
DO WHILE LONGCOUNT_DAYS < 0
    LONGCOUNT_DAYS = LONGCOUNT_DAYS + (13 * KinInBaktun)
LOOP
DO WHILE LONGCOUNT_DAYS > (13 * KinInBaktun)
    LONGCOUNT_DAYS = LONGCOUNT_DAYS - (13 * KinInBaktun)
LOOP
END FUNCTION

FUNCTION LONGCOUNT_DAYS_LONG&& ()
LONGCOUNT_DAYS_LONG = JULIANDAY - JULIANDAY_LONGCOUNT_OFFSET
END FUNCTION

FUNCTION LONGCOUNT_DAYS_LONG_X&& (t AS SYSTEMTIME)
LONGCOUNT_DAYS_LONG_X = JULIANDAY_X(t) - JULIANDAY_LONGCOUNT_OFFSET
END FUNCTION

FUNCTION LONGCOUNT_DAYS_X& (t AS SYSTEMTIME)
LONGCOUNT_DAYS_X = JULIANDAY_X(t) - JULIANDAY_LONGCOUNT_OFFSET
DO WHILE LONGCOUNT_DAYS_X < 0
    LONGCOUNT_DAYS_X = LONGCOUNT_DAYS_X + (13 * KinInBaktun)
LOOP
DO WHILE LONGCOUNT_DAYS_X > (13 * KinInBaktun)
    LONGCOUNT_DAYS_X = LONGCOUNT_DAYS_X - (13 * KinInBaktun)
LOOP
END FUNCTION

FUNCTION LONGCOUNT_HAAB$ ()
LONGCOUNT_HAAB = TRIMnum$(LONGCOUNT_HAAB_DATE) + " " + LONGCOUNT_HAAB_MONTH
END FUNCTION

FUNCTION LONGCOUNT_HAAB_DATE%% ()
LONGCOUNT_HAAB_DATE = LONGCOUNT_HAAB_DAY MOD 20
END FUNCTION

FUNCTION LONGCOUNT_HAAB_DATE_X%% (t AS SYSTEMTIME)
LONGCOUNT_HAAB_DATE_X = LONGCOUNT_HAAB_DAY_X(t) MOD 20
END FUNCTION

FUNCTION LONGCOUNT_HAAB_DAY% ()
LONGCOUNT_HAAB_DAY = (LONGCOUNT_DAYS_LONG - 17) MOD 365
END FUNCTION

FUNCTION LONGCOUNT_HAAB_DAY_X% (t AS SYSTEMTIME)
LONGCOUNT_HAAB_DAY_X = (LONGCOUNT_DAYS_LONG_X(t) - 17) MOD 365
END FUNCTION

FUNCTION LONGCOUNT_HAAB_MONTH$ () 'http://en.wikipedia.org/wiki/Haab%27
SELECT CASE LONGCOUNT_HAAB_MONTH_NUM
    CASE 0
        LONGCOUNT_HAAB_MONTH = "Pop"
    CASE 1
        LONGCOUNT_HAAB_MONTH = "Wo`"
    CASE 2
        LONGCOUNT_HAAB_MONTH = "Sip"
    CASE 3
        LONGCOUNT_HAAB_MONTH = "Sotz`"
    CASE 4
        LONGCOUNT_HAAB_MONTH = "Sek"
    CASE 5
        LONGCOUNT_HAAB_MONTH = "Xul"
    CASE 6
        LONGCOUNT_HAAB_MONTH = "Yack`in`"
    CASE 7
        LONGCOUNT_HAAB_MONTH = "Mol"
    CASE 8
        LONGCOUNT_HAAB_MONTH = "Ch`en"
    CASE 9
        LONGCOUNT_HAAB_MONTH = "Yax"
    CASE 10
        LONGCOUNT_HAAB_MONTH = "Sak`"
    CASE 11
        LONGCOUNT_HAAB_MONTH = "Keh"
    CASE 12
        LONGCOUNT_HAAB_MONTH = "Mak"
    CASE 13
        LONGCOUNT_HAAB_MONTH = "K`ank`in"
    CASE 14
        LONGCOUNT_HAAB_MONTH = "Muwan`"
    CASE 15
        LONGCOUNT_HAAB_MONTH = "Pax"
    CASE 16
        LONGCOUNT_HAAB_MONTH = "K`ayab"
    CASE 17
        LONGCOUNT_HAAB_MONTH = "Kumk`u"
    CASE 18
        LONGCOUNT_HAAB_MONTH = "Wayeb`"
END SELECT
END FUNCTION

FUNCTION LONGCOUNT_HAAB_MONTH_NUM%% ()
LONGCOUNT_HAAB_MONTH_NUM = INT(LONGCOUNT_HAAB_DAY / 20)
END FUNCTION

FUNCTION LONGCOUNT_HAAB_MONTH_NUM_X%% (t AS SYSTEMTIME)
LONGCOUNT_HAAB_MONTH_NUM_X = INT(LONGCOUNT_HAAB_DAY_X(t) / 20)
END FUNCTION

FUNCTION LONGCOUNT_HAAB_MONTH_X$ (t AS SYSTEMTIME)
SELECT CASE LONGCOUNT_HAAB_MONTH_NUM_X(t)
    CASE 0
        LONGCOUNT_HAAB_MONTH_X = "Pop"
    CASE 1
        LONGCOUNT_HAAB_MONTH_X = "Wo`"
    CASE 2
        LONGCOUNT_HAAB_MONTH_X = "Sip"
    CASE 3
        LONGCOUNT_HAAB_MONTH_X = "Sotz`"
    CASE 4
        LONGCOUNT_HAAB_MONTH_X = "Sek"
    CASE 5
        LONGCOUNT_HAAB_MONTH_X = "Xul"
    CASE 6
        LONGCOUNT_HAAB_MONTH_X = "Yack`in`"
    CASE 7
        LONGCOUNT_HAAB_MONTH_X = "Mol"
    CASE 8
        LONGCOUNT_HAAB_MONTH_X = "Ch`en"
    CASE 9
        LONGCOUNT_HAAB_MONTH_X = "Yax"
    CASE 10
        LONGCOUNT_HAAB_MONTH_X = "Sak`"
    CASE 11
        LONGCOUNT_HAAB_MONTH_X = "Keh"
    CASE 12
        LONGCOUNT_HAAB_MONTH_X = "Mak"
    CASE 13
        LONGCOUNT_HAAB_MONTH_X = "K`ank`in"
    CASE 14
        LONGCOUNT_HAAB_MONTH_X = "Muwan`"
    CASE 15
        LONGCOUNT_HAAB_MONTH_X = "Pax"
    CASE 16
        LONGCOUNT_HAAB_MONTH_X = "K`ayab"
    CASE 17
        LONGCOUNT_HAAB_MONTH_X = "Kumk`u"
    CASE 18
        LONGCOUNT_HAAB_MONTH_X = "Wayeb`"
END SELECT
END FUNCTION

FUNCTION LONGCOUNT_HAAB_X$ (t AS SYSTEMTIME)
LONGCOUNT_HAAB_X = TRIMnum$(LONGCOUNT_HAAB_DATE_X(t)) + " " + LONGCOUNT_HAAB_MONTH_X(t)
END FUNCTION

FUNCTION LONGCOUNT_KATUN%% ()
LONGCOUNT_KATUN = INT(INT(INT(LONGCOUNT_DAYS / KinInWinal) / WinalInTun) / TunInKatun) MOD KatunInBaktun
END FUNCTION

FUNCTION LONGCOUNT_KATUN_X%% (t AS SYSTEMTIME)
LONGCOUNT_KATUN_X = INT(INT(INT(LONGCOUNT_DAYS_X(t) / KinInWinal) / WinalInTun) / TunInKatun) MOD KatunInBaktun
END FUNCTION

FUNCTION LONGCOUNT_KIN%% ()
LONGCOUNT_KIN = LONGCOUNT_DAYS MOD KinInWinal
END FUNCTION

FUNCTION LONGCOUNT_KIN_X%% (t AS SYSTEMTIME)
LONGCOUNT_KIN_X = LONGCOUNT_DAYS_X(t) MOD KinInWinal
END FUNCTION

FUNCTION LONGCOUNT_LONG$ ()
LONGCOUNT_LONG = LONGCOUNT + " " + LONGCOUNT_CALENDARROUND
END FUNCTION

FUNCTION LONGCOUNT_LONG_X$ (t AS SYSTEMTIME)
LONGCOUNT_LONG_X = LONGCOUNT_X(t) + " " + LONGCOUNT_CALENDARROUND_X(t)
END FUNCTION

FUNCTION LONGCOUNT_TUN%% ()
LONGCOUNT_TUN = INT(INT(LONGCOUNT_DAYS / KinInWinal) / WinalInTun) MOD 20
END FUNCTION

FUNCTION LONGCOUNT_TUN_X%% (t AS SYSTEMTIME)
LONGCOUNT_TUN_X = INT(INT(LONGCOUNT_DAYS_X(t) / KinInWinal) / WinalInTun) MOD 20
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN$ ()
LONGCOUNT_TZOLKIN = TRIMnum$(LONGCOUNT_TZOLKIN_DATE) + " " + LONGCOUNT_TZOLKIN_DAY
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_X$ (t AS SYSTEMTIME)
LONGCOUNT_TZOLKIN_X = TRIMnum$(LONGCOUNT_TZOLKIN_DATE_X(t)) + " " + LONGCOUNT_TZOLKIN_DAY_X(t)
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DATE%% ()
LONGCOUNT_TZOLKIN_DATE = (4 + LONGCOUNT_DAYS_LONG) MOD 13
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DATE_X%% (t AS SYSTEMTIME)
LONGCOUNT_TZOLKIN_DATE_X = (4 + LONGCOUNT_DAYS_LONG_X(t)) MOD 13
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DAY$ () 'http://en.wikipedia.org/wiki/Tzolk%27in
SELECT CASE LONGCOUNT_TZOLKIN_DAY_NUM
    CASE 0
        LONGCOUNT_TZOLKIN_DAY = "Imix`"
    CASE 1
        LONGCOUNT_TZOLKIN_DAY = "Ik`"
    CASE 2
        LONGCOUNT_TZOLKIN_DAY = "Ak`b`al"
    CASE 3
        LONGCOUNT_TZOLKIN_DAY = "K`an"
    CASE 4
        LONGCOUNT_TZOLKIN_DAY = "Chikchan"
    CASE 5
        LONGCOUNT_TZOLKIN_DAY = "Kimi"
    CASE 6
        LONGCOUNT_TZOLKIN_DAY = "Manik`"
    CASE 7
        LONGCOUNT_TZOLKIN_DAY = "Lamat"
    CASE 8
        LONGCOUNT_TZOLKIN_DAY = "Muluk"
    CASE 9
        LONGCOUNT_TZOLKIN_DAY = "Ok"
    CASE 10
        LONGCOUNT_TZOLKIN_DAY = "Chuwen"
    CASE 11
        LONGCOUNT_TZOLKIN_DAY = "Eb`"
    CASE 12
        LONGCOUNT_TZOLKIN_DAY = "B`en"
    CASE 13
        LONGCOUNT_TZOLKIN_DAY = "Ix"
    CASE 14
        LONGCOUNT_TZOLKIN_DAY = "Men"
    CASE 15
        LONGCOUNT_TZOLKIN_DAY = "Kib`"
    CASE 16
        LONGCOUNT_TZOLKIN_DAY = "Kab`an"
    CASE 17
        LONGCOUNT_TZOLKIN_DAY = "Etz`nab`"
    CASE 18
        LONGCOUNT_TZOLKIN_DAY = "Kawak"
    CASE 19
        LONGCOUNT_TZOLKIN_DAY = "Ajaw"
END SELECT
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DAY_NUM%% ()
LONGCOUNT_TZOLKIN_DAY_NUM = LONGCOUNT_DAYS_LONG MOD 20 - 1
IF LONGCOUNT_TZOLKIN_DAY_NUM < 0 THEN LONGCOUNT_TZOLKIN_DAY_NUM = LONGCOUNT_TZOLKIN_DAY_NUM + 20
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DAY_NUM_X%% (t AS SYSTEMTIME)
LONGCOUNT_TZOLKIN_DAY_NUM_X = LONGCOUNT_DAYS_LONG_X(t) MOD 20 - 1
IF LONGCOUNT_TZOLKIN_DAY_NUM_X < 0 THEN LONGCOUNT_TZOLKIN_DAY_NUM_X = LONGCOUNT_TZOLKIN_DAY_NUM_X + 20
END FUNCTION

FUNCTION LONGCOUNT_TZOLKIN_DAY_X$ (t AS SYSTEMTIME)
SELECT CASE LONGCOUNT_TZOLKIN_DAY_NUM_X(t)
    CASE 0
        LONGCOUNT_TZOLKIN_DAY_X = "Imix`"
    CASE 1
        LONGCOUNT_TZOLKIN_DAY_X = "Ik`"
    CASE 2
        LONGCOUNT_TZOLKIN_DAY_X = "Ak`b`al"
    CASE 3
        LONGCOUNT_TZOLKIN_DAY_X = "K`an"
    CASE 4
        LONGCOUNT_TZOLKIN_DAY_X = "Chikchan"
    CASE 5
        LONGCOUNT_TZOLKIN_DAY_X = "Kimi"
    CASE 6
        LONGCOUNT_TZOLKIN_DAY_X = "Manik`"
    CASE 7
        LONGCOUNT_TZOLKIN_DAY_X = "Lamat"
    CASE 8
        LONGCOUNT_TZOLKIN_DAY_X = "Muluk"
    CASE 9
        LONGCOUNT_TZOLKIN_DAY_X = "Ok"
    CASE 10
        LONGCOUNT_TZOLKIN_DAY_X = "Chuwen"
    CASE 11
        LONGCOUNT_TZOLKIN_DAY_X = "Eb`"
    CASE 12
        LONGCOUNT_TZOLKIN_DAY_X = "B`en"
    CASE 13
        LONGCOUNT_TZOLKIN_DAY_X = "Ix"
    CASE 14
        LONGCOUNT_TZOLKIN_DAY_X = "Men"
    CASE 15
        LONGCOUNT_TZOLKIN_DAY_X = "Kib`"
    CASE 16
        LONGCOUNT_TZOLKIN_DAY_X = "Kab`an"
    CASE 17
        LONGCOUNT_TZOLKIN_DAY_X = "Etz`nab`"
    CASE 18
        LONGCOUNT_TZOLKIN_DAY_X = "Kawak"
    CASE 19
        LONGCOUNT_TZOLKIN_DAY_X = "Ajaw"
END SELECT
END FUNCTION

FUNCTION LONGCOUNT_WINAL%% ()
LONGCOUNT_WINAL = INT(LONGCOUNT_DAYS / KinInWinal) MOD WinalInTun
END FUNCTION

FUNCTION LONGCOUNT_WINAL_X%% (t AS SYSTEMTIME)
LONGCOUNT_WINAL_X = INT(LONGCOUNT_DAYS_X(t) / KinInWinal) MOD WinalInTun
END FUNCTION

FUNCTION LONGCOUNT_X$ (t AS SYSTEMTIME)
DIM d AS LONG
d = LONGCOUNT_DAYS_X(t)
LONGCOUNT_X = TRIMnum$(d MOD KinInWinal)
d = INT(d / KinInWinal)
LONGCOUNT_X = TRIMnum$(d MOD WinalInTun) + "." + LONGCOUNT_X
d = INT(d / WinalInTun)
LONGCOUNT_X = TRIMnum$(d MOD TunInKatun) + "." + LONGCOUNT_X
d = INT(d / TunInKatun)
LONGCOUNT_X = TRIMnum$(d MOD KatunInBaktun) + "." + LONGCOUNT_X
d = INT(d / KatunInBaktun)
LONGCOUNT_X = TRIMnum$(d MOD 20) + "." + LONGCOUNT_X
END FUNCTION

FUNCTION LUNAR_AGE%% () 'http://www.paulsadowski.com/wsh/moonphase.htm
LUNAR_AGE = INT(LUNAR_SYNODIC * LUNAR_CYCLE_LENGTH)
END FUNCTION

FUNCTION LUNAR_AGE_LONG! ()
LUNAR_AGE_LONG = LUNAR_SYNODIC * LUNAR_CYCLE_LENGTH
END FUNCTION

FUNCTION LUNAR_AGE_LONG_X! (t AS SYSTEMTIME)
LUNAR_AGE_LONG_X = LUNAR_SYNODIC_X(t) * LUNAR_CYCLE_LENGTH
END FUNCTION

FUNCTION LUNAR_AGE_X%% (t AS SYSTEMTIME)
LUNAR_AGE_X = INT(LUNAR_SYNODIC_X(t) * LUNAR_CYCLE_LENGTH)
END FUNCTION

FUNCTION LUNAR_ANOMALISTIC! ()
LUNAR_ANOMALISTIC = (JULIANDAY - JULIANDAY_LUNAR_OFFSET) / LUNAR_ANOMALISTIC_CYCLE_LENGTH
LUNAR_ANOMALISTIC = LUNAR_ANOMALISTIC - INT(LUNAR_ANOMALISTIC)
IF LUNAR_ANOMALISTIC < 0 THEN LUNAR_ANOMALISTIC = LUNAR_ANOMALISTIC + 1
END FUNCTION

FUNCTION LUNAR_ANOMALISTIC_RAD ()
LUNAR_ANOMALISTIC_RAD = LUNAR_ANOMALISTIC * PI2
END FUNCTION

FUNCTION LUNAR_ANOMALISTIC_RAD_X (t AS SYSTEMTIME)
LUNAR_ANOMALISTIC_RAD_X = LUNAR_ANOMALISTIC_X(t) * PI2
END FUNCTION

FUNCTION LUNAR_ANOMALISTIC_X! (t AS SYSTEMTIME)
LUNAR_ANOMALISTIC_X = (JULIANDAY_X(t) - JULIANDAY_LUNAR_OFFSET) / LUNAR_ANOMALISTIC_CYCLE_LENGTH
LUNAR_ANOMALISTIC_X = LUNAR_ANOMALISTIC_X - INT(LUNAR_ANOMALISTIC_X)
IF LUNAR_ANOMALISTIC_X < 0 THEN LUNAR_ANOMALISTIC_X = LUNAR_ANOMALISTIC_X + 1
END FUNCTION

FUNCTION LUNAR_DISTANCE! () 'http://www.paulsadowski.com/wsh/moonphase.htm
LUNAR_DISTANCE = 60.4 - 3.3 * COS(LUNAR_ANOMALISTIC_RAD) - .6 * COS(2 * LUNAR_SYNODIC_RAD - LUNAR_ANOMALISTIC_RAD) - .5 * COS(2 * LUNAR_SYNODIC_RAD)
END FUNCTION

FUNCTION LUNAR_DISTANCE_X! (t AS SYSTEMTIME)
LUNAR_DISTANCE_X = 60.4 - 3.3 * COS(LUNAR_ANOMALISTIC_RAD_X(t)) - .6 * COS(2 * LUNAR_SYNODIC_RAD_X(t) - LUNAR_ANOMALISTIC_RAD_X(t)) - .5 * COS(2 * LUNAR_SYNODIC_RAD_X(t))
END FUNCTION

FUNCTION LUNAR_DRAW& (r AS SINGLE) 'http://www.codeproject.com/Articles/100174/Calculate-and-Draw-Moon-Phase
DIM ip AS SINGLE
DIM rpos AS SINGLE
DIM xpos AS SINGLE
DIM xpos1 AS SINGLE
DIM xpos2 AS SINGLE
DIM ypos AS SINGLE
DIM d AS LONG
LUNAR_DRAW = _NEWIMAGE(r * 2, r * 2, 32)
ip = LUNAR_AGE_LONG / LUNAR_CYCLE_LENGTH
_DEST LUNAR_DRAW
FOR ypos = 0 TO r
    xpos = INT(SQR(r * r - ypos * ypos))
    LINE (r - xpos, ypos + r)-(xpos + r, ypos + r), _RGBA32(255, 255, 255, 255)
    LINE (r - xpos, r - ypos)-(xpos + r, r - ypos), _RGBA32(255, 255, 255, 255)
    rpos = 2 * xpos
    IF ip < .5 THEN
        xpos1 = -xpos
        xpos2 = INT(rpos - 2 * ip * rpos - xpos)
    ELSE
        xpos1 = xpos
        xpos2 = INT(xpos - 2 * ip * rpos + rpos)
    END IF
    LINE (xpos1 + r, r - ypos)-(xpos2 + r, r - ypos), _RGBA32(0, 0, 0, 255)
    LINE (xpos1 + r, ypos + r)-(xpos2 + r, ypos + r), _RGBA32(0, 0, 0, 255)
NEXT ypos
_DEST 0
END SUB

FUNCTION LUNAR_DRAW_X& (t AS SYSTEMTIME, r AS SINGLE)
DIM ip AS SINGLE
DIM rpos AS SINGLE
DIM xpos AS SINGLE
DIM xpos1 AS SINGLE
DIM xpos2 AS SINGLE
DIM ypos AS SINGLE
DIM d AS LONG
LUNAR_DRAW_X = _NEWIMAGE(r * 2, r * 2, 32)
ip = LUNAR_AGE_LONG_X(t) / LUNAR_CYCLE_LENGTH
_DEST LUNAR_DRAW_X
FOR ypos = 0 TO r
    xpos = INT(SQR(r * r - ypos * ypos))
    LINE (r - xpos, ypos + r)-(xpos + r, ypos + r), _RGBA32(255, 255, 255, 255)
    LINE (r - xpos, r - ypos)-(xpos + r, r - ypos), _RGBA32(255, 255, 255, 255)
    rpos = 2 * xpos
    IF ip < .5 THEN
        xpos1 = -xpos
        xpos2 = INT(rpos - 2 * ip * rpos - xpos)
    ELSE
        xpos1 = xpos
        xpos2 = INT(xpos - 2 * ip * rpos + rpos)
    END IF
    LINE (xpos1 + r, r - ypos)-(xpos2 + r, r - ypos), _RGBA32(0, 0, 0, 255)
    LINE (xpos1 + r, ypos + r)-(xpos2 + r, ypos + r), _RGBA32(0, 0, 0, 255)
NEXT ypos
_DEST 0
END SUB

FUNCTION LUNAR_ECLIPTIC_LATITUDE! () 'http://www.paulsadowski.com/wsh/moonphase.htm
LUNAR_ECLIPTIC_LATITUDE = (JULIANDAY - JULIANDAY_LUNAR_OFFSET) / LUNAR_ECLIPTIC_LATITUDE_CYCLE_LENGTH
LUNAR_ECLIPTIC_LATITUDE = LUNAR_ECLIPTIC_LATITUDE - INT(LUNAR_ECLIPTIC_LATITUDE)
IF LUNAR_ECLIPTIC_LATITUDE < 0 THEN LUNAR_ECLIPTIC_LATITUDE = LUNAR_ECLIPTIC_LATITUDE + 1
LUNAR_ECLIPTIC_LATITUDE = LUNAR_ECLIPTIC_LATITUDE * PI2
LUNAR_ECLIPTIC_LATITUDE = 5.1 * SIN(LUNAR_ECLIPTIC_LATITUDE)
END FUNCTION

FUNCTION LUNAR_ECLIPTIC_LATITUDE_X! (t AS SYSTEMTIME)
LUNAR_ECLIPTIC_LATITUDE_X = (JULIANDAY_X(t) - JULIANDAY_LUNAR_OFFSET) / LUNAR_ECLIPTIC_LATITUDE_CYCLE_LENGTH
LUNAR_ECLIPTIC_LATITUDE_X = LUNAR_ECLIPTIC_LATITUDE_X - INT(LUNAR_ECLIPTIC_LATITUDE_X)
IF LUNAR_ECLIPTIC_LATITUDE_X < 0 THEN LUNAR_ECLIPTIC_LATITUDE_X = LUNAR_ECLIPTIC_LATITUDE_X + 1
LUNAR_ECLIPTIC_LATITUDE_X = LUNAR_ECLIPTIC_LATITUDE_X * PI2
LUNAR_ECLIPTIC_LATITUDE_X = 5.1 * SIN(LUNAR_ECLIPTIC_LATITUDE_X)
END FUNCTION

FUNCTION LUNAR_ECLIPTIC_LONGITUDE! () 'http://www.paulsadowski.com/wsh/moonphase.htm
LUNAR_ECLIPTIC_LONGITUDE = (JULIANDAY - JULIANDAY_LUNAR_LONGITUDE_OFFSET) / LUNAR_ECLIPTIC_LONGITUDE_CYCLE_LENGTH
LUNAR_ECLIPTIC_LONGITUDE = LUNAR_ECLIPTIC_LONGITUDE - INT(LUNAR_ECLIPTIC_LONGITUDE)
IF LUNAR_ECLIPTIC_LONGITUDE < 0 THEN LUNAR_ECLIPTIC_LONGITUDE = LUNAR_ECLIPTIC_LONGITUDE + 1
LUNAR_ECLIPTIC_LONGITUDE = 360 * LUNAR_ECLIPTIC_LONGITUDE + 6.3 * SIN(LUNAR_ANOMALISTIC_RAD) + 1.3 * SIN(2 * LUNAR_SYNODIC_RAD - LUNAR_ANOMALISTIC_RAD) + .7 * SIN(2 * LUNAR_SYNODIC_RAD)
END FUNCTION

FUNCTION LUNAR_ECLIPTIC_LONGITUDE_X! (t AS SYSTEMTIME)
LUNAR_ECLIPTIC_LONGITUDE_X = (JULIANDAY_X(t) - JULIANDAY_LUNAR_LONGITUDE_OFFSET) / LUNAR_ECLIPTIC_LONGITUDE_CYCLE_LENGTH
LUNAR_ECLIPTIC_LONGITUDE_X = LUNAR_ECLIPTIC_LONGITUDE_X - INT(LUNAR_ECLIPTIC_LONGITUDE_X)
IF LUNAR_ECLIPTIC_LONGITUDE_X < 0 THEN LUNAR_ECLIPTIC_LONGITUDE_X = LUNAR_ECLIPTIC_LONGITUDE_X + 1
LUNAR_ECLIPTIC_LONGITUDE_X = 360 * LUNAR_ECLIPTIC_LONGITUDE_X + 6.3 * SIN(LUNAR_ANOMALISTIC_RAD_X(t)) + 1.3 * SIN(2 * LUNAR_SYNODIC_RAD_X(t) - LUNAR_ANOMALISTIC_RAD_X(t)) + .7 * SIN(2 * LUNAR_SYNODIC_RAD_X(t))
END FUNCTION

FUNCTION LUNAR_PHASE$ () 'http://home.hiwaay.net/~krcool/Astro/moon/moonphase/
SELECT CASE LUNAR_AGE
    CASE 0, 29
        LUNAR_PHASE = "New"
    CASE 1, 2, 3, 4, 5, 6
        LUNAR_PHASE = "Waxing Cresent"
    CASE 7
        LUNAR_PHASE = "First Quarter"
    CASE 8, 9, 10, 11, 12, 13
        LUNAR_PHASE = "Waxing Gibbous"
    CASE 14
        LUNAR_PHASE = "Full"
    CASE 15, 16, 17, 18, 19, 20, 21
        LUNAR_PHASE = "Waning Gibbous"
    CASE 22
        LUNAR_PHASE = "Last Quarter"
    CASE 23, 24, 25, 26, 27, 28
        LUNAR_PHASE = "Waning Cresent"
END SELECT
END FUNCTION

FUNCTION LUNAR_PHASE_X$ (t AS SYSTEMTIME)
SELECT CASE LUNAR_AGE_X(t)
    CASE 0, 29
        LUNAR_PHASE_X = "New"
    CASE 1, 2, 3, 4, 5, 6
        LUNAR_PHASE_X = "Waxing Cresent"
    CASE 7
        LUNAR_PHASE_X = "First Quarter"
    CASE 8, 9, 10, 11, 12, 13
        LUNAR_PHASE_X = "Waxing Gibbous"
    CASE 14
        LUNAR_PHASE_X = "Full"
    CASE 15, 16, 17, 18, 19, 20, 21
        LUNAR_PHASE_X = "Waning Gibbous"
    CASE 22
        LUNAR_PHASE_X = "Last Quarter"
    CASE 23, 24, 25, 26, 27, 28
        LUNAR_PHASE_X = "Waning Cresent"
END SELECT
END FUNCTION

FUNCTION LUNAR_SYNODIC! ()
LUNAR_SYNODIC = (JULIANDAY - JULIANDAY_LUNAR_SYNODIC_OFFSET) / LUNAR_CYCLE_LENGTH
LUNAR_SYNODIC = LUNAR_SYNODIC - INT(LUNAR_SYNODIC)
IF LUNAR_SYNODIC < 0 THEN LUNAR_SYNODIC = LUNAR_SYNODIC + 1
END FUNCTION

FUNCTION LUNAR_SYNODIC_RAD! ()
LUNAR_SYNODIC_RAD = LUNAR_SYNODIC * PI2
END FUNCTION

FUNCTION LUNAR_SYNODIC_RAD_X! (t AS SYSTEMTIME)
LUNAR_SYNODIC_RAD_X = LUNAR_SYNODIC_X(t) * PI2
END FUNCTION

FUNCTION LUNAR_SYNODIC_X! (t AS SYSTEMTIME)
LUNAR_SYNODIC_X = (JULIANDAY_X(t) - JULIANDAY_LUNAR_SYNODIC_OFFSET) / LUNAR_CYCLE_LENGTH
LUNAR_SYNODIC_X = LUNAR_SYNODIC_X - INT(LUNAR_SYNODIC_X)
IF LUNAR_SYNODIC_X < 0 THEN LUNAR_SYNODIC_X = LUNAR_SYNODIC_X + 1
END FUNCTION

FUNCTION MILLISECOND% ()
MILLISECOND = TIMER(.001) * 1000 MOD 1000
END FUNCTION

FUNCTION MILLISECOND_STR$ ()
MILLISECOND_STR = zeroset$(MILLISECOND, 2)
END FUNCTION

FUNCTION MILLISECOND_STR_X$ (t AS SYSTEMTIME)
MILLISECOND_STR_X = zeroset$(MILLISECOND_X(t), 2)
END FUNCTION

FUNCTION MILLISECOND_X% (t AS SYSTEMTIME)
MILLISECOND_X = t.wMilliseconds
END FUNCTION

FUNCTION MINUTE%% ()
MINUTE = VAL(MID$(TIME$, 4, 2))
END FUNCTION

FUNCTION MINUTE_X%% (t AS SYSTEMTIME)
MINUTE_X = t.wMinute
END FUNCTION

FUNCTION MONTH%% ()
MONTH = VAL(LEFT$(DATE$, 2))
END FUNCTION

FUNCTION MONTH_X%% (t AS SYSTEMTIME)
MONTH_X = t.wMonth
END FUNCTION

FUNCTION MONTHNAME$ ()
SELECT CASE MONTH
    CASE JANUARY
        MONTHNAME = "January"
    CASE FEBRUARY
        MONTHNAME = "February"
    CASE MARCH
        MONTHNAME = "March"
    CASE APRIL
        MONTHNAME = "April"
    CASE MAY
        MONTHNAME = "May"
    CASE JUNE
        MONTHNAME = "June"
    CASE JULY
        MONTHNAME = "July"
    CASE AUGUST
        MONTHNAME = "August"
    CASE SEPTEMBER
        MONTHNAME = "September"
    CASE OCTOBER
        MONTHNAME = "October"
    CASE NOVEMBER
        MONTHNAME = "November"
    CASE DECEMBER
        MONTHNAME = "December"
END SELECT
END FUNCTION

FUNCTION MONTHNAME_X$ (t AS SYSTEMTIME)
SELECT CASE MONTH_X(t)
    CASE JANUARY
        MONTHNAME_X = "January"
    CASE FEBRUARY
        MONTHNAME_X = "February"
    CASE MARCH
        MONTHNAME_X = "March"
    CASE APRIL
        MONTHNAME_X = "April"
    CASE MAY
        MONTHNAME_X = "May"
    CASE JUNE
        MONTHNAME_X = "June"
    CASE JULY
        MONTHNAME_X = "July"
    CASE AUGUST
        MONTHNAME_X = "August"
    CASE SEPTEMBER
        MONTHNAME_X = "September"
    CASE OCTOBER
        MONTHNAME_X = "October"
    CASE NOVEMBER
        MONTHNAME_X = "November"
    CASE DECEMBER
        MONTHNAME_X = "December"
END SELECT
END FUNCTION

FUNCTION MONTH_DAYS%% (m AS _BYTE)
SELECT CASE m
    CASE 2
        MONTH_DAYS = 28 - LEAPYEAR
    CASE 4, 6, 9, 11
        MONTH_DAYS = 30
    CASE ELSE
        MONTH_DAYS = 31
END SELECT
END FUNCTION

FUNCTION PLATONICYEAR_CYCLE% ()
PLATONICYEAR_CYCLE = YEAR - PLATONICYEAR_START
END FUNCTION

FUNCTION PLATONICYEAR_CYCLE_X% (t AS SYSTEMTIME)
PLATONICYEAR_CYCLE_X = YEAR_X(t) - PLATONICYEAR_START_X(t)
END FUNCTION

FUNCTION PLATONICYEAR_END& ()
PLATONICYEAR_END = PLATONICYEAR_START + PLATONICAGE_LENGTH
END FUNCTION

FUNCTION PLATONICYEAR_END_X& (t AS SYSTEMTIME)
PLATONICYEAR_END_X = PLATONICYEAR_START_X(t) + PLATONICAGE_LENGTH
END FUNCTION

FUNCTION PLATONICYEAR_LEFT% ()
PLATONICYEAR_LEFT = PLATONICYEAR_END - YEAR
END FUNCTION

FUNCTION PLATONICYEAR_LEFT_X% (t AS SYSTEMTIME)
PLATONICYEAR_LEFT_X = PLATONICYEAR_END_X(t) - YEAR_X(t)
END FUNCTION

FUNCTION PLATONICYEAR_START& ()
PLATONICYEAR_START = -4700 + (INT((YEAR + 4700) / PLATONICAGE_LENGTH) + (YEAR < -4700) * PLATONICAGE_LENGTH)
END FUNCTION

FUNCTION PLATONICYEAR_START_X& (t AS SYSTEMTIME)
PLATONICYEAR_START_X = -4700 + (INT((YEAR_X(t) + 4700) / PLATONICAGE_LENGTH) + (YEAR_X(t) < -4700) * PLATONICAGE_LENGTH)
END FUNCTION

FUNCTION SECOND%% ()
SECOND = VAL(RIGHT$(TIME$, 2))
END FUNCTION

FUNCTION SECOND_X%% (t AS SYSTEMTIME)
SECOND_X = t.wSecond
END FUNCTION

FUNCTION TICKCOUNT~& ()
TICKCOUNT = GetTickCount
IF TICKCOUNT + 1 = 0 THEN TICKCOUNT = 0
END FUNCTION

FUNCTION TIMEAFTER&& (t AS SYSTEMTIME, y AS INTEGER, m AS _BYTE, d AS _BYTE, h AS _BYTE, n AS _BYTE, s AS _BYTE)
DIM ft AS FILETIME
SystemTimeToFileTime _OFFSET(t), _OFFSET(ft)
TIMEAFTER = ft.dwLowDateTime + ft.dwHighDateTime * 4294967295
TIMEAFTER = TIMEAFTER + (((y * SecondsInYear) + (m * DaysInMonth * SecondsInDay) + (d * SecondsInDay) + (h * SecondsInHour) + (n * SecondsInMinute) + s) * 10000000)
TIMEAFTER = _ROUND(TIMEAFTER / 10000000) * 10000000
END FUNCTION

FUNCTION TIMEBEFORE&& (t AS SYSTEMTIME, y AS INTEGER, m AS _BYTE, d AS _BYTE, h AS _BYTE, n AS _BYTE, s AS _BYTE)
DIM ft AS FILETIME
SystemTimeToFileTime _OFFSET(t), _OFFSET(ft)
TIMEBEFORE = ft.dwLowDateTime + ft.dwHighDateTime * 4294967295
TIMEBEFORE = TIMEBEFORE - (((y * SecondsInYear) + (m * DaysInMonth * SecondsInDay) + (d * SecondsInDay) + (h * SecondsInHour) + (n * SecondsInMinute) + s) * 10000000)
TIMEBEFORE = _ROUND(TIMEBEFORE / 10000000) * 10000000
END FUNCTION

FUNCTION TIMEBETWEEN&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM ft1 AS FILETIME
DIM ft2 AS FILETIME
SystemTimeToFileTime _OFFSET(t1), _OFFSET(ft1)
SystemTimeToFileTime _OFFSET(t2), _OFFSET(ft2)
TIMEBETWEEN = (ft2.dwLowDateTime - ft1.dwLowDateTime) + (4294967295 * (ft2.dwHighDateTime - ft1.dwHighDateTime))
TIMEBETWEEN = _ROUND(TIMEBETWEEN / 10000000) * 10000000
END FUNCTION

FUNCTION TIMEBETWEEN_DAY&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_DAY = INT(TIMEBETWEEN_SECOND(t1, t2) / SecondsInDay)
END FUNCTION

FUNCTION TIMEBETWEEN_HOUR&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_HOUR = INT(TIMEBETWEEN_SECOND(t1, t2) / SecondsInHour)
END FUNCTION

FUNCTION TIMEBETWEEN_MILLISECOND&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_MILLISECOND = INT(TIMEBETWEEN(t1, t2) / 10000)
END FUNCTION

FUNCTION TIMEBETWEEN_MINUTE&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_MINUTE = INT(TIMEBETWEEN_SECOND(t1, t2) / SecondsInMinute)
END FUNCTION

FUNCTION TIMEBETWEEN_MONTH&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_MONTH = INT(TIMEBETWEEN_DAY(t1, t2) / 30)
END FUNCTION

FUNCTION TIMEBETWEEN_SECOND&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_SECOND = INT(TIMEBETWEEN(t1, t2) / 10000000)
END FUNCTION

FUNCTION TIMEBETWEEN_STR$ (t AS SYSTEMTIME)
IF t.wYear > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wYear) + " years "
ELSE IF t.wYear > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 year "
    END IF
END IF
IF t.wMonth > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wMonth) + " months "
ELSE IF t.wMonth > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 month "
    END IF
END IF
IF t.wDay > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wDay) + " days "
ELSE IF t.wDay > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 day "
    END IF
END IF
IF t.wHour > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wHour) + " hours "
ELSE IF t.wHour > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 hour "
    END IF
END IF
IF t.wMinute > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wMinute) + " minutes "
ELSE IF t.wMinute > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 minute "
    END IF
END IF
IF t.wSecond > 1 THEN
    TIMEBETWEEN_STR = TIMEBETWEEN_STR + TRIMnum$(t.wSecond) + " seconds "
ELSE IF t.wSecond > 0 THEN
        TIMEBETWEEN_STR = TIMEBETWEEN_STR + "1 second"
    END IF
END IF
IF RIGHT$(TIMEBETWEEN_STR, 1) = " " THEN TIMEBETWEEN_STR = LEFT$(TIMEBETWEEN_STR, LEN(TIMEBETWEEN_STR) - 1)
END FUNCTION

FUNCTION TIMEBETWEEN_STR_LONG$ (t AS SYSTEMTIME)
TIMEBETWEEN_STR_LONG = TIMEBETWEEN_STR(t)
IF t.wMilliseconds > 1 THEN
    IF LEN(TIMEBETWEEN_STR_LONG) THEN
        TIMEBETWEEN_STR_LONG = TIMEBETWEEN_STR_LONG + ", " + TRIMnum$(t.wMilliseconds) + " milliseconds"
    ELSE
        TIMEBETWEEN_STR_LONG = TIMEBETWEEN_STR_LONG + TRIMnum$(t.wMilliseconds) + " milliseconds"
    END IF
ELSE IF t.wMilliseconds > 0 THEN
        IF LEN(TIMEBETWEEN_STR_LONG) THEN
            TIMEBETWEEN_STR_LONG = TIMEBETWEEN_STR_LONG + " 1 milisecond"
        ELSE
            TIMEBETWEEN_STR_LONG = TIMEBETWEEN_STR_LONG + "1 milisecond"
        END IF
    END IF
END IF
END FUNCTION

FUNCTION TIMEBETWEEN_WEEK&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_WEEK = INT(TIMEBETWEEN_DAY(t1, t2) / DaysInWeek)
END FUNCTION

FUNCTION TIMEBETWEEN_YEAR&& (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEBETWEEN_YEAR = INT(TIMEBETWEEN_SECOND(t1, t2) / SecondsInYear)
END FUNCTION

FUNCTION TIMECOMPARE%% (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
DIM ft1 AS FILETIME
DIM ft2 AS FILETIME
DIM cft1 AS _INTEGER64
DIM cft2 AS _INTEGER64
SystemTimeToFileTime _OFFSET(t1), _OFFSET(ft1)
SystemTimeToFileTime _OFFSET(t2), _OFFSET(ft2)
cft1 = ft1.dwLowDateTime + ft1.dwHighDateTime * 4294967295
cft2 = ft2.dwLowDateTime + ft2.dwHighDateTime * 4294967295
IF cft1 > cft2 THEN
    TIMECOMPARE = 1
ELSE IF cft1 < cft2 THEN
        TIMECOMPARE = -1
    END IF
END IF
END FUNCTION

FUNCTION TIMEISAFTER` (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEISAFTER = (TIMECOMPARE(t1, t2) = 1)
END FUNCTION

FUNCTION TIMEISBEFORE` (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEISBEFORE = (TIMECOMPARE(t1, t2) = -1)
END FUNCTION

FUNCTION TIMEISEQUAL` (t1 AS SYSTEMTIME, t2 AS SYSTEMTIME)
TIMEISEQUAL = (TIMECOMPARE(t1, t2) = 0)
END FUNCTION

FUNCTION TIMENAME$ ()
SELECT CASE INT(TIMER)
    CASE MIDNIGHT
        TIMENAME = "Midnight"
    CASE NOON
        TIMENAME = "Noon"
    CASE IS < MORNING
        TIMENAME = "Night"
    CASE IS < AFTERNOON
        TIMENAME = "Morning"
    CASE IS < EVENING
        TIMENAME = "Afternoon"
    CASE IS < NIGHT
        TIMENAME = "Evening"
    CASE ELSE
        TIMENAME = "Night"
END SELECT
END FUNCTION

FUNCTION TIMENAME_X$ (t AS SYSTEMTIME)
SELECT CASE t.wHour * SecondsInHour + t.wMinute * SecondsInMinute + t.wSecond
    CASE MIDNIGHT
        TIMENAME_X = "Midnight"
    CASE NOON
        TIMENAME_X = "Noon"
    CASE IS < MORNING
        TIMENAME_X = "Night"
    CASE IS < AFTERNOON
        TIMENAME_X = "Morning"
    CASE IS < EVENING
        TIMENAME_X = "Afternoon"
    CASE IS < NIGHT
        TIMENAME_X = "Evening"
    CASE ELSE
        TIMENAME_X = "Night"
END SELECT
END FUNCTION

FUNCTION TIMESTAMP$ ()
TIMESTAMP = TRIM$(STR$(YEAR)) + zeroset$(MONTH, 2) + zeroset$(DAY, 2) + zeroset$(HOUR, 2) + zeroset$(MINUTE, 2) + zeroset$(SECOND, 2)
END FUNCTION

FUNCTION TIMESTAMP_LONG$ ()
TIMESTAMP_LONG = TIMESTAMP + MILLISECOND_STR
END FUNCTION

FUNCTION TIMESTAMP_LONG_X$ (t AS SYSTEMTIME)
TIMESTAMP_LONG_X = TIMESTAMP_X(t) + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION TIMESTAMP_LONG_SHORTYEAR$ ()
TIMESTAMP_LONG_SHORTYEAR = TIMESTAMP_SHORTYEAR + MILLISECOND_STR
END FUNCTION

FUNCTION TIMESTAMP_LONG_SHORTYEAR_X$ (t AS SYSTEMTIME)
TIMESTAMP_LONG_SHORTYEAR_X = TIMESTAMP_SHORTYEAR_X(t) + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION TIMESTAMP_SHORT$ ()
TIMESTAMP_SHORT = LEFT$(TIMESTAMP, 12)
END FUNCTION

FUNCTION TIMESTAMP_SHORT_X$ (t AS SYSTEMTIME)
TIMESTAMP_SHORT_X = LEFT$(TIMESTAMP_X(t), 12)
END FUNCTION

FUNCTION TIMESTAMP_SHORTYEAR$ ()
TIMESTAMP_SHORTYEAR = zeroset$(YEAR_SHORT, 2) + zeroset$(MONTH, 2) + zeroset$(DAY, 2) + zeroset$(HOUR, 2) + zeroset$(MINUTE, 2) + zeroset$(SECOND, 2)
END FUNCTION

FUNCTION TIMESTAMP_SHORTYEAR_X$ (t AS SYSTEMTIME)
TIMESTAMP_SHORTYEAR_X = zeroset$(YEAR_SHORT_X(t), 2) + zeroset$(MONTH_X(t), 2) + zeroset$(DAY_X(t), 2) + zeroset$(HOUR_X(t), 2) + zeroset$(MINUTE_X(t), 2) + zeroset$(SECOND_X(t), 2)
END FUNCTION

FUNCTION TIMESTAMP_SHORT_SHORTYEAR$ ()
TIMESTAMP_SHORT_SHORTYEAR = LEFT$(TIMESTAMP_SHORTYEAR, 10)
END FUNCTION

FUNCTION TIMESTAMP_SHORT_SHORTYEAR_X$ (t AS SYSTEMTIME)
TIMESTAMP_SHORT_SHORTYEAR_X = LEFT$(TIMESTAMP_SHORTYEAR_X(t), 10)
END FUNCTION

FUNCTION TIMESTAMP_X$ (t AS SYSTEMTIME)
TIMESTAMP_X = TRIM$(STR$(YEAR_X(t))) + zeroset$(MONTH_X(t), 2) + zeroset$(DAY_X(t), 2) + zeroset$(HOUR_X(t), 2) + zeroset$(MINUTE_X(t), 2) + zeroset$(SECOND_X(t), 2)
END FUNCTION

FUNCTION TIMEZONE$ ()
DIM tz AS TIME_ZONE_INFORMATION
GetTimeZoneInformation _OFFSET(tz)
TIMEZONE = UnicodeToANSI$(tz.StandardName)
END FUNCTION

FUNCTION TIMEZONE_DST%% ()
DIM tz AS TIME_ZONE_INFORMATION
TIMEZONE_DST = GetTimeZoneInformation(_OFFSET(tz))
END FUNCTION

FUNCTION TIMEZONE_OFFSET! ()
DIM tz AS TIME_ZONE_INFORMATION
GetTimeZoneInformation _OFFSET(tz)
TIMEZONE_OFFSET = tz.Bias / MinutesInHour
END FUNCTION

FUNCTION TIME_12$ ()
TIME_12 = zeroset$(HOUR_12, 2) + ":" + RIGHT$(TIME$, 5)
END FUNCTION

FUNCTION TIME_12_X$ (t AS SYSTEMTIME)
TIME_12_X = zeroset$(HOUR_12_X(t), 2) + ":" + RIGHT$(TIME_X(t), 5)
END FUNCTION

FUNCTION TIME_LONG$ ()
TIME_LONG = TIME$ + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION TIME_LONG_12$ ()
TIME_LONG_12 = TIME_12 + ":" + MILLISECOND_STR
END FUNCTION

FUNCTION TIME_LONG_12_X$ (t AS SYSTEMTIME)
TIME_LONG_12_X = TIME_12_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION TIME_LONG_X$ (t AS SYSTEMTIME)
TIME_LONG_X = TIME_X(t) + ":" + MILLISECOND_STR_X(t)
END FUNCTION

FUNCTION TIME_SHORT$ ()
TIME_SHORT = LEFT$(TIME$, 5)
END FUNCTION

FUNCTION TIME_SHORT_12$ ()
TIME_SHORT_12 = LEFT$(TIME_12, 5)
END FUNCTION

FUNCTION TIME_SHORT_12_X$ (t AS SYSTEMTIME)
TIME_SHORT_12_X = LEFT$(TIME_12_X(t), 5)
END FUNCTION

FUNCTION TIME_SHORT_X$ (t AS SYSTEMTIME)
TIME_SHORT_X = LEFT$(TIME_X(t), 5)
END FUNCTION

FUNCTION TIME_X$ (t AS SYSTEMTIME)
TIME_X = zeroset$(HOUR_X(t), 2) + ":" + zeroset$(MINUTE_X(t), 2) + ":" + zeroset$(SECOND_X(t), 2)
END FUNCTION

FUNCTION UNIXTIME& ()
DIM st AS SYSTEMTIME
DIM ft AS FILETIME
GetSystemTime _OFFSET(st)
SystemTimeToFileTime _OFFSET(st), _OFFSET(ft)
UNIXTIME = INT(((ft.dwLowDateTime + ft.dwHighDateTime * 4294967295) - t19700101) / 10000000)
END FUNCTION

FUNCTION UNIXTIME_LONG&& ()
DIM st AS SYSTEMTIME
DIM ft AS FILETIME
GetSystemTime _OFFSET(st)
SystemTimeToFileTime _OFFSET(st), _OFFSET(ft)
UNIXTIME_LONG = (ft.dwLowDateTime + ft.dwHighDateTime * 4294967295) - t19700101
END FUNCTION

FUNCTION UNIXTIME_LONG_X&& (t AS SYSTEMTIME)
DIM ft AS FILETIME
SystemTimeToFileTime _OFFSET(t), _OFFSET(ft)
UNIXTIME_LONG_X = (ft.dwLowDateTime + ft.dwHighDateTime * 4294967295) - t19700101
END FUNCTION

FUNCTION UNIXTIME_X& (t AS SYSTEMTIME)
DIM ft AS FILETIME
SystemTimeToFileTime _OFFSET(t), _OFFSET(ft)
UNIXTIME_X = INT(((ft.dwLowDateTime + ft.dwHighDateTime * 4294967295) - t19700101) / 10000000)
END FUNCTION

FUNCTION WEEK%% ()
WEEK = INT((YEAR_CYCLE - 1) / DaysInWeek) + 1
END FUNCTION

FUNCTION WEEKDAYINMONTH%% ()
WEEKDAYINMONTH = INT(DAY / DaysInWeek) + 1
END FUNCTION

FUNCTION WEEKDAYINMONTH_X%% (t AS SYSTEMTIME)
WEEKDAYINMONTH_X = INT(DAY_X(t) / DaysInWeek) + 1
END FUNCTION

FUNCTION WEEK_X%% (t AS SYSTEMTIME)
WEEK_X = INT((YEAR_CYCLE_X(t) - 1) / DaysInWeek) + 1
END FUNCTION

FUNCTION YEAR% ()
YEAR = VAL(RIGHT$(DATE$, 4))
END FUNCTION

FUNCTION YEAR_CYCLE% ()
DIM x AS _BYTE
FOR x = 1 TO MONTH - 1
    YEAR_CYCLE = YEAR_CYCLE + MONTH_DAYS(x)
NEXT x
YEAR_CYCLE = YEAR_CYCLE + DAY
END FUNCTION

FUNCTION YEAR_CYCLE_X% (t AS SYSTEMTIME)
DIM x AS _BYTE
FOR x = 1 TO MONTH_X(t) - 1
    YEAR_CYCLE_X = YEAR_CYCLE_X + MONTH_DAYS(x)
NEXT x
YEAR_CYCLE_X = YEAR_CYCLE_X + DAY_X(t)
END FUNCTION

FUNCTION YEAR_SHORT%% ()
YEAR_SHORT = VAL(RIGHT$(DATE$, 2))
END FUNCTION

FUNCTION YEAR_SHORT_X%% (t AS SYSTEMTIME)
YEAR_SHORT_X = t.wYear MOD YearsInCentury
END FUNCTION

FUNCTION YEAR_X% (t AS SYSTEMTIME)
YEAR_X = t.wYear
END FUNCTION

FUNCTION ZODIAC$ () 'http://www.psychicguild.com/horoscopes_explained.php
SELECT CASE YEAR_CYCLE + LEAPYEAR
    CASE IS < 20
        ZODIAC = "Capricorn"
    CASE IS < 50
        ZODIAC = "Aquarius"
    CASE IS < 81
        ZODIAC = "Pisces"
    CASE IS < 111
        ZODIAC = "Aries"
    CASE IS < 142
        ZODIAC = "Taurus"
    CASE IS < 173
        ZODIAC = "Gemini"
    CASE IS < 205
        ZODIAC = "Cancer"
    CASE IS < 236
        ZODIAC = "Leo"
    CASE IS < 267
        ZODIAC = "Virgo"
    CASE IS < 297
        ZODIAC = "Libra"
    CASE IS < 327
        ZODIAC = "Scorpio"
    CASE IS < 356
        ZODIAC = "Sagittarius"
    CASE ELSE
        ZODIAC = "Capricorn"
END SELECT
END FUNCTION

FUNCTION ZODIAC_X$ (t AS SYSTEMTIME)
SELECT CASE YEAR_CYCLE_X(t) + LEAPYEAR_X(t)
    CASE IS < 20
        ZODIAC_X = "Capricorn"
    CASE IS < 50
        ZODIAC_X = "Aquarius"
    CASE IS < 81
        ZODIAC_X = "Pisces"
    CASE IS < 111
        ZODIAC_X = "Aries"
    CASE IS < 142
        ZODIAC_X = "Taurus"
    CASE IS < 173
        ZODIAC_X = "Gemini"
    CASE IS < 205
        ZODIAC_X = "Cancer"
    CASE IS < 236
        ZODIAC_X = "Leo"
    CASE IS < 267
        ZODIAC_X = "Virgo"
    CASE IS < 297
        ZODIAC_X = "Libra"
    CASE IS < 327
        ZODIAC_X = "Scorpio"
    CASE IS < 356
        ZODIAC_X = "Sagittarius"
    CASE ELSE
        ZODIAC_X = "Capricorn"
END SELECT
END FUNCTION