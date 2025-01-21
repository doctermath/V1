/* gdmdcall.p : Get Demand Call For Spart Parts Data */
/* Flexible For Classic AppServer or Progress Application Server */
/* Output to temp table */

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE INPUT  PARAMETER oJsonIn     AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER oJsonOut    AS JsonObject NO-UNDO.

/* Temp-Table
--------------------------------------------------------------------------------*/
DEFINE TEMP-TABLE dataRange
    FIELD id AS INTEGER
    FIELD mn AS INTEGER
    FIELD yr AS INTEGER.

/* Variables
--------------------------------------------------------------------------------*/
DEFINE VARIABLE ix        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLimit    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDataLen  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMonthLen AS INTEGER   NO-UNDO.
DEFINE VARIABLE iYearLen  AS INTEGER   NO-UNDO.

DEFINE VARIABLE cBranch   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAgency   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPartno   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cq1       AS CHARACTER NO-UNDO. /* Character Query 1 */
DEFINE VARIABLE cq2       AS CHARACTER NO-UNDO. /* Character Query 2 */

DEFINE VARIABLE dtStart   AS DATE      NO-UNDO.
DEFINE VARIABLE dtEnd     AS DATE      NO-UNDO.
DEFINE VARIABLE dtExclude AS DATE      NO-UNDO.

DEFINE VARIABLE lok       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hTable    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbsrc     AS HANDLE    NO-UNDO. /* Handle Buffer Source */
DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.

MESSAGE "BEGIN Get Demand Call Program xx".

// TEST VARIABLE - Limit Data Retriver Per Table - ZERO for unlimited
ASSIGN
    iLimit = 0.

/* Assign Initial Value */
ASSIGN
    dtStart   = oJsonIn:GetDate("start-date")
    dtEnd     = oJsonIn:GetDate("end-date")
    dtExclude = oJsonIn:GetDate("exclude-older")
    cBranch   = oJsonIn:GetCharacter("branch")
    cAgency   = oJsonIn:GetCharacter("agency")
    cPartno   = oJsonIn:GetCharacter("partno")
    .
    
 // Create Dynamic Temp-Table
CREATE TEMP-TABLE hTable.
hTable:ADD-LIKE-FIELD("branch", "brctable.brc").
hTable:ADD-LIKE-FIELD("agency", "cpmf.agency").
hTable:ADD-LIKE-FIELD("partno", "cpmf.partno").
    
/* create dynamic d extent field */
iMonthLen = INTERVAL(dtEnd, dtStart, "MONTH") + 1. /* +1 to include end date to the data */
hTable:ADD-NEW-FIELD("d", "INTEGER", iMonthLen).
/*hTable:ADD-NEW-FIELD("dx", "CHARACTER", iMonthLen).*/ /* For Debugging */

// Index
hTable:ADD-NEW-INDEX('idx1', TRUE, TRUE).
hTable:ADD-INDEX-FIELD('idx1', 'branch').
hTable:ADD-INDEX-FIELD('idx1', 'agency').
hTable:ADD-INDEX-FIELD('idx1', 'partno').

// prepare
hTable:TEMP-TABLE-PREPARE("data").
hBuffer = hTable:DEFAULT-BUFFER-HANDLE.

// Utility table for looping the sparhis data
DEFINE VARIABLE dt AS DATE NO-UNDO.
REPEAT ix = 1 TO iMonthLen:
    CREATE dataRange.
    ASSIGN
        id = ix
        dt = ADD-INTERVAL(dtStart, ix - 1, "MONTH")
        mn = MONTH(dt)
        yr = INT(SUBSTR(STRING(dt, "999999"), 5)). // left 2 digit year
END.

/* loop coresponding table */
REPEAT ix = YEAR(dtStart) TO YEAR(dtEnd):
    iYearLen = iYearLen + 1.
    
    CREATE BUFFER hbsrc FOR TABLE "sparthis" + SUBSTR(STRING(ix),3).  
    
    cq1 = SUBSTITUTE(
        "FOR EACH &1 NO-LOCK WHERE &1.brc BEGINS '&2' AND &1.agency BEGINS '&3' AND &1.partno BEGINS '&4'",
        hbsrc:NAME,
        cBranch,
        cAgency,
        cPartno
    ).
    
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hbsrc).
    hQuery:QUERY-PREPARE(cq1).
    hQuery:QUERY-OPEN.
    
    iy = 1.
    REPEAT WHILE hQuery:GET-NEXT():

        MESSAGE "Looping" iy "data" hbsrc:NAME hbsrc::brc hbsrc::agency hbsrc::partno.
        iy = iy + 1.
        IF iy > iLimit AND iLimit <> 0 THEN LEAVE.
        
        /* Exclude Partno Data with older LASTACT from table spartqty */
        FIND FIRST spartqty WHERE
            hbsrc::brc    = spartqty.brc AND
            hbsrc::agency = spartqty.agency AND
            hbsrc::partno = spartqty.partno
            NO-LOCK NO-ERROR.
            
        IF AVAILABLE spartqty THEN DO:
            IF spartqty.lastact < dtExclude THEN DO:
                MESSAGE 'Parno' spartqty.partno ' lastact ' + STRING(spartqty.lastact) + ' is older than ' + STRING(dtExclude) + ', skipping...'. 
                NEXT.
            END.      
        END.
        ELSE NEXT.
        
        RELEASE spartqty.
        
        cq2 = SUBSTITUTE (
            "WHERE &1.branch = '&2' and &1.agency = '&3' and &1.partno = '&4'",
            hBuffer:NAME,
            hbsrc::brc,
            hbsrc::agency,
            hbsrc::partno
            ).
        
        lok = hBuffer:FIND-FIRST(cq2, NO-LOCK) NO-ERROR.
        IF lok THEN 
        DO:
            MESSAGE "Appending Data...".
            RUN dData(hBuffer, hbsrc). 
        END.
        ELSE 
        DO:
            MESSAGE "Creating New Data...".
            hBuffer:BUFFER-CREATE().
            ASSIGN
                iDataLen = iDataLen + 1
                hBuffer::branch = hbsrc::brc
                hBuffer::agency = hbsrc::agency
                hBuffer::partno = hbsrc::partno.
            RUN dData(hBuffer, hbsrc).    
        END.       
    END.
    hQuery:QUERY-CLOSE().    
END.
    
PROCEDURE dData:
    /* Reusable Procedure to calculate Demand data to each month */
    DEFINE INPUT  PARAMETER hBuffer AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER hbsrc AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE iPSI AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPSR AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWS  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWR  AS INTEGER NO-UNDO.

    FOR EACH dataRange WHERE yr = INTEGER(SUBSTRING(hbsrc:NAME, 9)) NO-LOCK:  
         
        ASSIGN
            iPSI = hbsrc::psi(dataRange.mn)
            iPSR = hbsrc::psr(dataRange.mn)
            iWS  = hbsrc::ws(dataRange.mn)
            iWR  = hbsrc::wr(dataRange.mn)

            hBuffer::d(dataRange.id) = (iPSI - iPSR) + (iWS - iWR).
            /*hBuffer::dx(dataRange.id) = 'month ' + string(dataRange.mn) + "y" + string(dataRange.yr).*/
    END.
END.

oJsonOut = NEW JsonObject().
hTable:WRITE-JSON('JsonObject', oJsonOut, TRUE).
oJsonOut:Add("data-count", iDataLen).
oJsonOut:Add("month-count", iMonthLen).
oJsonOut:Add("year-count", iYearLen).

MESSAGE "DELETING DYNAMIC OBJECT".
IF VALID-HANDLE(hTable)     THEN DELETE OBJECT hTable.
IF VALID-HANDLE(hbsrc)      THEN DELETE OBJECT hbsrc.
IF VALID-HANDLE(hQuery)     THEN DELETE OBJECT hQuery.




