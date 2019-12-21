'declare download sub/func
DECLARE SUB DownloadFree (handle AS INTEGER)
DECLARE SUB DownloadStart (handle)
DECLARE SUB DownloadStop (handle)
DECLARE SUB DownloadPause (handle)
DECLARE SUB DownloadUpdate (handle AS INTEGER)
DECLARE FUNCTION Download% (url AS STRING, file AS STRING, timelimit AS SINGLE)
DECLARE FUNCTION DownloadCount%% ()
DECLARE FUNCTION DownloadFile$ (handle AS INTEGER)
DECLARE FUNCTION DownloadNew% (url AS STRING, file AS STRING, timelimit AS SINGLE)
DECLARE FUNCTION DownloadPercent! (handle AS INTEGER)
DECLARE FUNCTION DownloadProgress& (handle AS INTEGER)
DECLARE FUNCTION DownloadSize& (handle AS INTEGER)
DECLARE FUNCTION DownloadStatus%% (handle AS INTEGER)
DECLARE FUNCTION DownloadURL$ (handle AS INTEGER)
DECLARE FUNCTION DownloadValid` (handle AS INTEGER)

'download type declarations
TYPE DOWNLOAD
    inuse AS _BYTE
    status AS _BYTE
    url AS STRING * 1024
    file AS STRING * 128
    filesize AS LONG
    progress AS LONG
    client AS LONG
    timelimit AS SINGLE
    timeinit AS SINGLE
    timer AS LONG
    quick AS _BYTE
END TYPE

'download constant definitions
CONST DOWNLOAD_ALLDOWNLOADS = TRUE
CONST DOWNLOAD_COMPLETED = -1
CONST DOWNLOAD_FAILED = 0
CONST DOWNLOAD_FULL = FALSE
CONST DOWNLOAD_INPROGRESS = 1
CONST DOWNLOAD_PAUSED = 2
CONST DOWNLOAD_QUICK = TRUE

'dimension shared arrays
REDIM downloads(1 TO 0) AS DOWNLOAD
REDIM downloads_header(1 TO 0) AS STRING