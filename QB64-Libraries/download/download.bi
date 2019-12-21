'******************************************************************************
'
'QB64 Download Library by Gorlock
'v0.1
'
'******************************************************************************
'
'Version 0.1  - 2013-12-20
'
'Future Updates:
'  - fix lag with multiple downloads
'
'******************************************************************************
'
'SUB       DownloadFree       (handle%)                  Frees a download handle
'SUB       DownloadPause      (handle%)                  Pauses a download
'SUB       DownloadStart      (handle%)                  Starts a download
'SUB       DownloadStop       (handle%)                  Stops a download
'SUB       DownloadUpdate     (handle%)                  Updates a download
'FUNCTION  Download%          (url$, file$, timelimit!)  Creates a new quick download
'FUNCTION  DownloadCount%%    ()                         Returns the number of downloads currently created
'FUNCTION  DownloadFile$      (handle%)                  Returns the local file name of the file being downloaded
'FUNCTION  DownloadNew%       (url$, file$, timelimit!)  Creates a new download
'FUNCTION  DownloadPercent!   (handle%)                  Returns the progress of the download as a percent
'FUNCTION  DownloadProgress&  (handle%)                  Returns the progress of the download in bytes
'FUNCTION  DownloadSize&      (handle%)                  Returns the size of the file being downloaded
'FUNCTION  DownloadStatus%%   (handle%)                  Returns the status of a download
'FUNCTION  DownloadURL$       (handle%)                  Returns the URL of a download
'FUNCTION  DownloadValid`     (handle%)                  Returns if a download handle is valid or not
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/1NUPDJAm
'
'******************************************************************************
'
'This library requires Gorlock's QB64 Common Library: https://db.tt/k9Tb6QJ4
'
'******************************************************************************
'
'For documentation on how to get the most out of the features and functions of
'  this library, see the documentation:
'https://db.tt/mX90HAWO
'
'******************************************************************************

SUB DownloadFree (handle AS INTEGER)
DIM client AS LONG
DIM fh AS LONG
SHARED downloads() AS DOWNLOAD
SHARED downloads_header() AS STRING
SELECT CASE handle
    CASE DOWNLOAD_ALLDOWNLOADS
        FOR handle = UBOUND(downloads) TO 1 STEP -1
            DownloadUpdate handle
        NEXT handle
    CASE ELSE
        IF NOT DownloadValid(handle) THEN EXIT SUB
        fh = 10000 + handle
        client = downloads(handle).client
        IF downloads(handle).timer THEN TIMER(downloads(handle).timer) FREE
        IF handle = UBOUND(downloads) AND handle > 1 THEN
            REDIM _PRESERVE downloads(1 TO handle - 1) AS DOWNLOAD
            REDIM _PRESERVE downloads_header(1 TO handle - 1) AS STRING
        ELSE
            downloads(handle).inuse = FALSE
        END IF
                IF downloads(handle).status <> DOWNLOAD_COMPLETED THEN
            CLOSE #fh
            CLOSE client
        END IF
END SELECT
END SUB

SUB DownloadPause (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
SELECT CASE handle
    CASE DOWNLOAD_ALLDOWNLOADS
        FOR handle = UBOUND(downloads) TO 1 STEP -1
            DownloadUpdate handle
        NEXT handle
    CASE ELSE
        IF NOT DownloadValid(handle) THEN EXIT SUB
        TIMER(downloads(handle).timer) OFF
        downloads(handle).status = DOWNLOAD_PAUSED
END SELECT
END SUB

SUB DownloadStart (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
SELECT CASE handle
    CASE DOWNLOAD_ALLDOWNLOADS
        FOR handle = UBOUND(downloads) TO 1 STEP -1
            DownloadUpdate handle
        NEXT handle
    CASE ELSE
        IF NOT DownloadValid(handle) THEN EXIT SUB
        TIMER(downloads(handle).timer) ON
        downloads(handle).status = DOWNLOAD_INPROGRESS
END SELECT
END SUB

SUB DownloadStop (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
SELECT CASE handle
    CASE DOWNLOAD_ALLDOWNLOADS
        FOR handle = UBOUND(downloads) TO 1 STEP -1
            DownloadUpdate handle
        NEXT handle
    CASE ELSE
        IF NOT DownloadValid(handle) THEN EXIT SUB
        TIMER(downloads(handle).timer) OFF
        downloads(handle).status = DOWNLOAD_FAILED
END SELECT
END SUB

SUB DownloadUpdate (handle AS INTEGER)
DIM client AS LONG
DIM x AS _BYTE
DIM fh AS LONG
DIM i AS LONG
DIM i2 AS LONG
DIM i3 AS LONG
DIM response AS STRING
SHARED downloads() AS DOWNLOAD
SHARED downloads_header() AS STRING
SELECT CASE handle
    CASE DOWNLOAD_ALLDOWNLOADS
        FOR handle = UBOUND(downloads) TO 1 STEP -1
            DownloadUpdate handle
        NEXT handle
    CASE ELSE
        IF NOT DownloadValid(handle) THEN EXIT SUB
        client = downloads(handle).client
        fh = 10000 + handle
        GET #client, , response
        IF downloads(handle).filesize = FALSE THEN
            downloads_header(handle) = downloads_header(handle) + response
            i = INSTR(downloads_header(handle), "Content-Length:")
            IF i THEN
                i2 = INSTR(i, downloads_header(handle), Crlf)
                IF i2 THEN
                    i3 = INSTR(i2, downloads_header(handle), Crlf + Crlf)
                    IF i3 THEN
                        downloads(handle).filesize = VAL(MID$(downloads_header(handle), i + 15, i2 - i - 14))
                        response = MID$(downloads_header(handle), i3 + 4)
                        PUT #fh, , response
                    END IF
                END IF
            END IF
        ELSE
            PUT #fh, , response
            downloads(handle).progress = LOF(fh)
            IF downloads(handle).progress = downloads(handle).filesize THEN
                TIMER(downloads(handle).timer) OFF
                CLOSE #fh
                CLOSE client
                downloads(handle).status = DOWNLOAD_COMPLETED
                IF downloads(handle).quick = DOWNLOAD_QUICK THEN DownloadFree handle
            END IF
        END IF
        IF downloads(handle).timelimit THEN
            IF downloads(handle).timeinit > TIMER THEN downloads(handle).timeinit = downloads(handle).timeinit - 86400
            IF TIMER > downloads(handle).timeinit + downloads(handle).timelimit THEN
                DownloadStop handle
                IF downloads(handle).quick = DOWNLOAD_QUICK THEN DownloadFree handle
            END IF
        END IF
END SELECT
END SUB

FUNCTION Download% (url AS STRING, file AS STRING, timelimit AS SINGLE)
SHARED downloads() AS DOWNLOAD
Download = DownloadNew(url, file, timelimit)
IF Download = FALSE THEN DownloadFree Download
downloads(Download).quick = DOWNLOAD_QUICK
END FUNCTION

FUNCTION DownloadCount%% ()
DIM x AS _BYTE
SHARED downloads() AS DOWNLOAD
FOR x = 1 TO UBOUND(downloads)
    IF downloads(x).inuse = TRUE THEN DownloadCount = DownloadCount + 1
NEXT x
END FUNCTION

FUNCTION DownloadFile$ (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
DownloadFile = RTRIM$(downloads(handle).file)
END FUNCTION

FUNCTION DownloadNew% (url AS STRING, file AS STRING, timelimit AS SINGLE)
DIM client AS LONG
DIM fh AS LONG
DIM s AS LONG
DIM x AS LONG
DIM filedir AS STRING
DIM request AS STRING
DIM url2 AS STRING
DIM url3 AS STRING
SHARED downloads() AS DOWNLOAD
SHARED downloads_header() AS STRING
DO
    FOR DownloadNew = 1 TO UBOUND(downloads)
        IF downloads(DownloadNew).inuse = FALSE THEN EXIT DO
    NEXT DownloadNew
    DownloadNew = DownloadNew + 1
    REDIM _PRESERVE downloads(1 TO DownloadNew) AS DOWNLOAD
    REDIM _PRESERVE downloads_header(1 TO DownloadNew) AS STRING
    EXIT DO
LOOP
downloads(DownloadNew).inuse = TRUE
downloads(DownloadNew).status = DOWNLOAD_FAILED
downloads(DownloadNew).url = url
downloads(DownloadNew).file = file
downloads(DownloadNew).filesize = FALSE
downloads(DownloadNew).progress = FALSE
downloads(DownloadNew).timelimit = timelimit
IF LEFT$(url, 4) = "http" THEN url = MID$(url, INSTR(url, "//") + 2)
url2 = url
x = INSTR(url2, "/")
IF x THEN url2 = LEFT$(url, x - 1)
client = _OPENCLIENT("TCP/IP:80:" + url2)
IF client = 0 THEN EXIT FUNCTION
downloads(DownloadNew).client = client
s = INSTR(file, Slash)
IF s THEN filedir = LEFT$(file, s - 1)
IF NOT _DIREXISTS(filedir) THEN MKDIR filedir
fh = 10000 + DownloadNew
OPEN file FOR BINARY AS #fh
url3 = RIGHT$(url, LEN(url) - x + 1)
request = "GET " + url3 + " HTTP/1.1" + Crlf
request = request + "Host: " + url2 + Crlf + Crlf
PUT #client, , request
downloads(DownloadNew).timeinit = TIMER
downloads(DownloadNew).timer = _FREETIMER
downloads(DownloadNew).quick = DOWNLOAD_FULL
downloads_header(DownloadNew) = ""
ON TIMER(downloads(DownloadNew).timer, .2) DownloadUpdate DownloadNew
DownloadStart DownloadNew
END FUNCTION

FUNCTION DownloadPercent! (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
IF downloads(handle).filesize THEN DownloadPercent = downloads(handle).progress / downloads(handle).filesize * 100
IF downloads(handle).status = DOWNLOAD_COMPLETED THEN DownloadPercent = 100
END FUNCTION

FUNCTION DownloadProgress& (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
DownloadProgress = downloads(handle).progress
END FUNCTION

FUNCTION DownloadSize& (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
DownloadSize = downloads(handle).filesize
END FUNCTION

FUNCTION DownloadStatus%% (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
DownloadStatus = downloads(handle).status
END FUNCTION

FUNCTION DownloadURL$ (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF NOT DownloadValid(handle) THEN EXIT SUB
DownloadURL = RTRIM$(downloads(handle).url)
END FUNCTION

FUNCTION DownloadValid` (handle AS INTEGER)
SHARED downloads() AS DOWNLOAD
IF handle = FALSE THEN EXIT FUNCTION
IF handle > UBOUND(downloads) THEN EXIT FUNCTION
IF downloads(handle).inuse = FALSE THEN EXIT FUNCTION
DownloadValid = TRUE
END FUNCTION