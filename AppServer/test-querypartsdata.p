USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

/* Define Temp Table To Save part number  */
DEFINE TEMP-TABLE ttSpartData
    FIELD branch            LIKE brctable.CODE
    FIELD agency            LIKE cpmf.agency
    FIELD partno            LIKE cpmf.partno
    FIELD D                 AS INT64 EXTENT 12 /* Demand Per Month */ 
    FIELD DT                AS CHARACTER EXTENT 12 /* Test Query */
    FIELD FD                AS DECIMAL    /* Forcast Demand of Target Month */
    FIELD FDT               AS C.        

DEFINE VARIABLE ix              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iy              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iz              AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFindQuery      AS CHARACTER   NO-UNDO.   
DEFINE VARIABLE lFindResult     AS LOGICAL     NO-UNDO.

DEFINE VARIABLE iTargetMonth    AS INTEGER     NO-UNDO. /* Target Month To Calculate The FD */
DEFINE VARIABLE iTargetYear     AS INTEGER     NO-UNDO. /* Target Year To Calculate The FD */
DEFINE VARIABLE lDoubleYear     AS LOGICAL     NO-UNDO. /* IF True, use 2 table to query data */
DEFINE VARIABLE iStartMonth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEndMonth       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartYear      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEndYear        AS INTEGER     NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer         AS HANDLE      NO-UNDO EXTENT 2.

ASSIGN iTargetMonth = 7. 
ASSIGN iTargetYear  = 24.

RUN Main.

/* Query The Data
--------------------------------------------------------------------------------*/
PROCEDURE Main:

    /* Previous Year Table Always will used */
    CREATE BUFFER hBuffer[1] FOR TABLE "sparthis" + STRING(iTargetYear - 1).

    /* First : Determine If Using Different Year Or Not */
    IF iTargetMonth > 1 THEN DO:
        CREATE BUFFER hBuffer[2] FOR TABLE "sparthis" + STRING(iTargetYear).                                   
        RUN QueryData(INPUT 1, INPUT iTargetMonth - 1, INPUT hBuffer[1]).
        RUN QueryData(INPUT 1, INPUT iTargetMonth, INPUT hBuffer[2]).
    END.
    /* Else if target month is january, query all 12 month of previous period */
    ELSE DO:
        RUN QueryData(INPUT 1, INPUT iTargetMonth - 1, INPUT hBuffer[1]).
    END.
    
    /* Test The Value */
    DEFINE VARIABLE aaa AS LONGCHAR      NO-UNDO.
    TEMP-TABLE ttSpartData:WRITE-JSON('Longchar', aaa, TRUE).
    MESSAGE STRING(aaa)
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    /* Memory Clean Up */
    DO ix = 1 TO EXTENT(hBuffer):
        IF VALID-HANDLE(hBuffer[ix]) THEN DELETE OBJECT hBuffer[ix].
    END.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.

END PROCEDURE.

PROCEDURE QueryData:
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + iphBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN.

    ASSIGN ix = 1.
    DO WHILE hQuery:GET-NEXT() AND ix < 10:
        ASSIGN ix = ix + 1.
        CREATE ttSpartData.
        ASSIGN
            ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
            ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
            ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
            .
        DO iy = ipiStartMonth TO ipiEndMonth:
            ASSIGN 
                ttSpartData.D[iy] = iphBuffer:BUFFER-FIELD('psi'):BUFFER-VALUE(iy)
                ttSpartData.DT[iy] = 'month ' + STRING(iy) + ' year ' + STRING(iEndYear) /* debug */
            .
        END.
        
        ASSIGN
            ttSpartData.FDT = "target MMYY " + STRING(iTargetMonth) + " " + STRING(iTargetYear) /* debug */ 
            .
    END.
    hQuery:QUERY-CLOSE().     

END PROCEDURE.


/* If Target Month Is January, Then Use all Previous Year Table to store demand history */
/*IF iTargetMonth = 1 THEN DO:  
    CREATE BUFFER hBuffer[1] FOR TABLE "sparthis" + STRING(iTargetYear - 1).
    ASSIGN
        iStartYear  = iTargetYear - 1
        iEndYear    = iStartYear
        iStartMonth = iTargetMonth
        iEndMonth   = 12.
END.

/* If Target Month is above 1, use current Year Table and Previous to store demand history */
ELSE IF iTargetMonth > 1 THEN DO:                                    
    CREATE BUFFER hBuffer[1] FOR TABLE "sparthis" + STRING(iTargetYear - 1).
    CREATE BUFFER hBuffer[2] FOR TABLE "sparthis" + STRING(iTargetYear).
    ASSIGN 
        lDoubleYear = TRUE
        iStartYear  = iTargetYear - 1
        iEndYear    = iTargetYear
        iStartMonth = iTargetMonth
        iEndMonth = iTargetMonth - 1.
END.*/



/*

ASSIGN iz = 1.
FOR EACH cpmf NO-LOCK:
    ASSIGN iz = iz + 1.
    IF iz > 1000 THEN LEAVE. /* Limit the result for dev */
    
    ASSIGN 
        cFindQuery = SUBSTITUTE(
            "WHERE &1.agency = '&2' and &1.partno = '&3'",
            hBuffer[1]:NAME,
            cpmf.agency,
            cpmf.partno
        )
        lFindResult = hBuffer[1]:FIND-FIRST(cFindQuery, NO-LOCK) NO-ERROR
    .
    
    IF lFindResult THEN DO:
        
    
    
        IF lDoubleYear THEN
            RUN QueryData(INPUT cpmf.agency, INPUT cpmf.partno, INPUT hBuffer[1]).
        ELSE
            RUN QueryData(INPUT cpmf.agency, INPUT cpmf.partno, INPUT hBuffer[1]).
    
    END.
        
END.

/* Query The Data
--------------------------------------------------------------------------------*/
PROCEDURE QueryData:
    DEFINE INPUT  PARAMETER ipcAgency AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPartno AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER iphBuffer AS HANDLE      NO-UNDO.
    
    DEFINE VARIABLE cQuery AS CHARACTER   NO-UNDO.
    
    ASSIGN cQuery = SUBSTITUTE(
        "FOR EACH &1 NO-LOCK WHERE &1.agency = '&2' AND &1.partno = '&3'",
        iphBuffer:NAME,
        ipcAgency,
        ipcPartno
    ).

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE(cQuery).
    hQuery:QUERY-OPEN.

    ASSIGN ix = 1.
    DO WHILE hQuery:GET-NEXT() AND ix < 10:
        ASSIGN ix = ix + 1.
        CREATE ttSpartData.
        ASSIGN
            ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
            ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
            ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
            .
        DO iy = 1 TO iEndMonth:
            ASSIGN 
                ttSpartData.D[iy] = iphBuffer:BUFFER-FIELD('psi'):BUFFER-VALUE(iy)
                ttSpartData.DT[iy] = 'month ' + STRING(iy) + ' year ' + STRING(iEndYear) /* debug */
            .
        END.
        
        ASSIGN
            ttSpartData.FDT = "target MMYY " + STRING(iTargetMonth) + " " + STRING(iTargetYear) /* debug */ 
            .
    END.
    hQuery:QUERY-CLOSE().        
END PROCEDURE.*/









