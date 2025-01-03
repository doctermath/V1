USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

/* Define Temp Table To Save part number  */
DEFINE TEMP-TABLE ttSpartData
    FIELD branch                LIKE brctable.CODE
    FIELD agency                LIKE cpmf.agency
    FIELD partno                LIKE cpmf.partno
    FIELD D                     AS INT64       EXTENT 13   /* Demand Per Month */ 
    FIELD FD                    AS DECIMAL                 /* Forcast Demand of Target Month */
    FIELD DemandFrom            AS CHARACTER   EXTENT 13   /* Test Query */
    FIELD ForecastDemandTarget  AS CHARACTER               /* Test Query */      
    .
    
DEFINE VARIABLE ix              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iy              AS INTEGER     NO-UNDO.
DEFINE VARIABLE ip              AS INTEGER     NO-UNDO. /* Pointer to D[P] */

DEFINE VARIABLE daTargetDate    AS DATE        NO-UNDO.
DEFINE VARIABLE daStartDate     AS DATE        NO-UNDO.
DEFINE VARIABLE daEndDate       AS DATE        NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer         AS HANDLE      NO-UNDO EXTENT 2.

/* Function */
FUNCTION excludeOlderLastActData RETURN LOGICAL (INPUT iphBuffer AS HANDLE):
    /* Find SPARTQTY LASTACT Older than November Previous Year 
     * IF Available, the data is skipped 
     */
    FIND FIRST spartqty WHERE
            iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() = spartqty.agency AND
            iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()    = spartqty.brc AND
            iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE() = spartqty.partno AND
            spartqty.lastact < DATE(11, 1, YEAR(TODAY) - 1) 
            NO-LOCK NO-ERROR.
        IF AVAILABLE spartqty THEN RETURN TRUE.
END.
RUN Main.

/* Query The Data
--------------------------------------------------------------------------------*/
PROCEDURE Main:

    /* Target Date As MM,DD,YY */
    ASSIGN 
        daTargetDate = DATE(MONTH(TODAY),1,YEAR(TODAY))
        daStartDate  = ADD-INTERVAL(daTargetDate, -13, "MONTH") 
        daEndDate    = ADD-INTERVAL(daTargetDate, -1, "MONTH").
     
    /* Previous Year Table Always will used */
    CREATE BUFFER hBuffer[1] FOR TABLE "sparthis" + SUBSTR(STRING(YEAR(daStartDate)),3).
    CREATE BUFFER hBuffer[2] FOR TABLE "sparthis" + SUBSTR(STRING(YEAR(daEndDate)), 3).
    
    RUN QueryData(MONTH(daStartDate), 12, hBuffer[1], FALSE).
    RUN QueryData(1, MONTH(daEndDate), hBuffer[2], TRUE).
    RUN AddOuterData(1, MONTH(daEndDate), hBuffer[2]).
    
    /* Test The Value ======================== */
    TEMP-TABLE ttSpartData:WRITE-JSON('FILE', "C:\Users\User\Desktop\bbb.txt", TRUE).
    
    /* Memory Clean Up */
    DO ix = 1 TO EXTENT(hBuffer):
        IF VALID-HANDLE(hBuffer[ix]) THEN DELETE OBJECT hBuffer[ix].
    END.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.

END PROCEDURE.



PROCEDURE AddOuterData:
    /* Outer Join the data so all right table still included */
    /* Get The Right Table And Create New Row if not exist on ttSpartData */
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + iphBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN.

    ASSIGN ix = 1.
    DO WHILE hQuery:GET-NEXT() AND ix < 100:
        
        /* Function To Exclude Older SpareParts Data */
        IF excludeOlderLastActData(iphBuffer) THEN NEXT.
        
        ASSIGN ix = ix + 1.
        
        FIND FIRST ttSpartData WHERE
            ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE() AND 
            ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() AND
            ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
            NO-LOCK NO-ERROR.    
        IF AVAILABLE ttSpartData THEN NEXT.
        ELSE DO:
            CREATE ttSpartData.
            ASSIGN
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                . 
                
            ASSIGN ip = 14 - ipiEndMonth.  
            RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            ASSIGN ttSpartData.ForecastDemandTarget = "target MMYY " + STRING(MONTH(daTargetDate)) + " " + STRING(YEAR(daTargetDate)).
        END.
    END.
    hQuery:QUERY-CLOSE(). 
END PROCEDURE.

PROCEDURE QueryData:
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER iplAddData     AS LOGICAL     NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + iphBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN.

    ASSIGN ix = 1.
    DO WHILE hQuery:GET-NEXT() AND ix < 100:
        ASSIGN ix = ix + 1.
        
        /* Function To Exclude Older SpareParts Data */
        IF excludeOlderLastActData(iphBuffer) THEN NEXT.
        
        /* If Create New Data */
        IF NOT iplAddData THEN DO:
            CREATE ttSpartData.
            ASSIGN
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                .
                
            ASSIGN ip = 1.  
            RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            ASSIGN ttSpartData.ForecastDemandTarget = "target MMYY " + STRING(MONTH(daTargetDate)) + " " + STRING(YEAR(daTargetDate)). /* debug */  
        END.
        
        /* If Querying Second Table To Add Data */
        ELSE DO:
            FIND FIRST ttSpartData WHERE
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE() AND 
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() AND
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                EXCLUSIVE-LOCK NO-ERROR.
                
            IF AVAILABLE ttSpartData THEN DO:
                ASSIGN ip = 14 - ipiEndMonth.
                RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            END.
        END.
    END.
    hQuery:QUERY-CLOSE().     

END PROCEDURE. 

PROCEDURE AddData:
    DEFINE INPUT  PARAMETER ip             AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.
    
    DEFINE VARIABLE iPSI AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iPSR AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWS  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWR  AS INTEGER     NO-UNDO.

    DO iy = ipiStartMonth TO ipiEndMonth:
        ASSIGN
            iPSI  = iphBuffer:BUFFER-FIELD('psi'):BUFFER-VALUE(iy)
            iPSR  = iphBuffer:BUFFER-FIELD('psr'):BUFFER-VALUE(iy)
            iWS   = iphBuffer:BUFFER-FIELD('ws'):BUFFER-VALUE(iy)
            iWR   = iphBuffer:BUFFER-FIELD('wr'):BUFFER-VALUE(iy).
    
        ASSIGN 
            ttSpartData.D[ip] = (iPSI - iPSR) + (iWS - iWR)
            ttSpartData.DemandFrom[ip] = 'month ' + STRING(iy) + ' table ' + iphBuffer:NAME /* debug */
            ip = ip + 1.  
    END.     
END PROCEDURE.
