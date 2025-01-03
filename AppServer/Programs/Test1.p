/* gdmdcall.p */

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

MESSAGE "BEGIN Get Demand Call Program" VIEW-AS ALERT-BOX.

/* Temp-Table Definitions
--------------------------------------------------------------------------------*/
/* Define Temp Table To Save part number  */
DEFINE TEMP-TABLE ttSpartData
    FIELD period AS DATE
    FIELD branch LIKE brctable.CODE
    FIELD agency LIKE cpmf.agency
    FIELD partno LIKE cpmf.partno
    FIELD D      AS INT64   EXTENT 13       /* Demand Per Month */ 
    FIELD FD     AS DECIMAL                 /* Forcast Demand of Target Month */
    FIELD DemandFrom            AS CHARACTER   EXTENT 13   /* Test Query */
    FIELD ForecastDemandTarget  AS CHARACTER               /* Test Query */      
    .

DEFINE TEMP-TABLE ttSpartData2 LIKE ttSpartData.
 
/* Definitions
--------------------------------------------------------------------------------*/
DEFINE VARIABLE oJsonArr     AS JsonArray NO-UNDO. 
    
DEFINE VARIABLE iCount       AS INTEGER NO-UNDO.
DEFINE VARIABLE ix           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iz           AS INTEGER   NO-UNDO.
DEFINE VARIABLE ip           AS INTEGER   NO-UNDO. /* Pointer to D[P] */

DEFINE VARIABLE daTargetDate AS DATE      NO-UNDO.
DEFINE VARIABLE daStartDate  AS DATE      NO-UNDO.
DEFINE VARIABLE daEndDate    AS DATE      NO-UNDO.

DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO EXTENT 2.

DEFINE VARIABLE iLimitRow    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lAllBranch   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dtPeriod     AS DATE      NO-UNDO.

/* Fucntion
--------------------------------------------------------------------------------*/
FUNCTION excludeOlderLastActData RETURN LOGICAL (INPUT iphBuffer AS HANDLE):
    /* Find SPARTQTY LASTACT Older than 13 Previous Month
     * IF Available, the data is skipped */
     
    DEFINE VARIABLE activeDateCap AS DATE NO-UNDO.
    ASSIGN 
        activeDateCap = ADD-INTERVAL(TODAY, -13 , "MONTH")
        activeDateCap = DATE(MONTH(activeDateCap), 1, YEAR(ActiveDateCap))
        .
     
    FIND FIRST spartqty WHERE
        iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() = spartqty.agency AND
        iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()    = spartqty.brc AND
        iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE() = spartqty.partno AND
        spartqty.lastact < activeDateCap 
        NO-LOCK NO-ERROR.
        
    IF AVAILABLE spartqty THEN DO:
        MESSAGE 'Parno' spartqty.partno ' lastact ' + STRING(spartqty.lastact) + ' is older than ' + STRING(activeDateCap) + ', skipping...'
            VIEW-AS ALERT-BOX.
        RELEASE spartqty.
        RETURN TRUE.
    END.
    RELEASE spartqty.
END.

/* MAIN_BLOCK
--------------------------------------------------------------------------------*/
DO ON ERROR UNDO, LEAVE:
    ASSIGN 
        iLimitRow  = 0
        lAllBranch = LOGICAL(poRequest:URI:GetQueryValue('all-branch')) 
        dtPeriod   = DATE(MONTH(TODAY),1,YEAR(TODAY)). // ONLY FOR DUMMY DATA
    
    /* Target Date As MM,DD,YY */
    ASSIGN 
        daTargetDate = DATE(MONTH(TODAY),1,YEAR(TODAY))
        daStartDate  = ADD-INTERVAL(daTargetDate, -13, "MONTH") 
        daEndDate    = ADD-INTERVAL(daTargetDate, -1, "MONTH").
     
    /* Previous Year Table Always will used */
    CREATE BUFFER hBuffer[1] FOR TABLE "sparthis" + SUBSTR(STRING(YEAR(daStartDate)),3).
    CREATE BUFFER hBuffer[2] FOR TABLE "sparthis" + SUBSTR(STRING(YEAR(daEndDate)), 3).
    
    ASSIGN 
        iCount = 0.
    RUN QueryData(MONTH(daStartDate), 12, hBuffer[1], FALSE).
    RUN QueryData(1, MONTH(daEndDate), hBuffer[2], TRUE).
    RUN AddOuterData(1, MONTH(daEndDate), hBuffer[2]).
    
    IF lAllBranch THEN RUN CombineBranch.
    
    oJsonArr = NEW JsonArray().
    oJsonArr:read(TEMP-TABLE ttSpartData:HANDLE).
    
    /* Output To JSON */
    oJson:Add('total-data', iCount).
    oJson:Add('data', oJsonArr).
    
    /* Memory Clean Up */
    DO ix = 1 TO EXTENT(hBuffer):
        IF VALID-HANDLE(hBuffer[ix]) THEN DELETE OBJECT hBuffer[ix].
    END.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
END.

/* Internal Procedures
--------------------------------------------------------------------------------*/
PROCEDURE QueryData:
    /* QUERY DATA FROM SPARTHIS TABLE AND PUT TO TEMP-TABLE */
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER iplAddData     AS LOGICAL     NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + iphBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN.

    ASSIGN ix = 0.
    DO WHILE hQuery:GET-NEXT():
        ASSIGN 
            ix = ix + 1
            iCount = iCount + 1.
        IF ix > iLimitRow AND iLimitRow <> 0 THEN LEAVE. 
        
        /* Function To Exclude Older SpareParts Data based on LASTACT*/
        IF excludeOlderLastActData(iphBuffer) THEN NEXT.
        
        /* If Create New Data */
        IF NOT iplAddData THEN 
        DO:
            MESSAGE "Retrieving Record No" ix "from" iphBuffer:NAME
                VIEW-AS ALERT-BOX.
            
            CREATE ttSpartData.
            ASSIGN
                ttSpartData.period = daTargetDate
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                .
                
            ASSIGN 
                ip = 1.  
            RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            ASSIGN ttSpartData.ForecastDemandTarget = "target MMYY " + STRING(MONTH(daTargetDate)) + " " + STRING(YEAR(daTargetDate)). /* debug */  
        END.
        
        /* QUERYING SECOND TABLE TO APPEND D DATA TO PREVIOUS QUERIED DATA */
        ELSE 
        DO:
            
            FIND FIRST ttSpartData WHERE
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE() AND 
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() AND
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                EXCLUSIVE-LOCK NO-ERROR.
                
            IF AVAILABLE ttSpartData THEN 
            DO:
                MESSAGE "Adding Record No" ix "from" iphBuffer:NAME "partno" iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                    VIEW-AS ALERT-BOX.
                ASSIGN 
                    ip = 14 - ipiEndMonth.
                RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            END.
        END.
    END.
    hQuery:QUERY-CLOSE().     

END PROCEDURE. 

PROCEDURE AddOuterData:
    /* ADD RIGHT TABLE TO DATA SO RIGHT TABLE WILL NOT EXCLUDED ON FIRST TABLE JOIN */
    /* Get The Right Table And Create New Row if not exist on ttSpartData */
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(iphBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + iphBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN.

    ASSIGN 
        ix = 0.
    DO WHILE hQuery:GET-NEXT() /*AND ix < iLimitRow*/ :
        ASSIGN 
            ix = ix + 1
            iCount = iCount + 1.
        IF ix > iLimitRow AND iLimitRow <> 0 THEN LEAVE. 
                        
        /* Function To Exclude Older SpareParts Data */
        IF excludeOlderLastActData(iphBuffer) THEN NEXT.
        
        
        FIND FIRST ttSpartData WHERE
            ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE() AND 
            ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() AND
            ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
            NO-LOCK NO-ERROR.    
        IF AVAILABLE ttSpartData THEN NEXT.
        ELSE 
        DO:
            MESSAGE "Retrieving Outer Record No" ix "from" iphBuffer:NAME
                VIEW-AS ALERT-BOX.
            CREATE ttSpartData.
            ASSIGN
                ttSpartData.period = daTargetDate
                ttSpartData.branch = iphBuffer:BUFFER-FIELD('brc'):BUFFER-VALUE()
                ttSpartData.agency = iphBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
                ttSpartData.partno = iphBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
                . 
                
            ASSIGN 
                ip = 14 - ipiEndMonth.  
            RUN AddData(ip, ipiStartMonth, ipiEndMonth, iphBuffer).
            ASSIGN ttSpartData.ForecastDemandTarget = "target MMYY " + STRING(MONTH(daTargetDate)) + " " + STRING(YEAR(daTargetDate)).
        END.
    END.
    hQuery:QUERY-CLOSE(). 
END PROCEDURE.

PROCEDURE AddData:
    /* CALCULATE D DATA AND PUT INTO TEMP-TABLE */
    DEFINE INPUT  PARAMETER ip             AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiStartMonth  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndMonth    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iphBuffer      AS HANDLE      NO-UNDO.
    
    DEFINE VARIABLE iPSI AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPSR AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWS  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWR  AS INTEGER NO-UNDO.

    DO iy = ipiStartMonth TO ipiEndMonth:
        ASSIGN
            iPSI = iphBuffer:BUFFER-FIELD('psi'):BUFFER-VALUE(iy)
            iPSR = iphBuffer:BUFFER-FIELD('psr'):BUFFER-VALUE(iy)
            iWS  = iphBuffer:BUFFER-FIELD('ws'):BUFFER-VALUE(iy)
            iWR  = iphBuffer:BUFFER-FIELD('wr'):BUFFER-VALUE(iy).
    
        ASSIGN 
            ttSpartData.D[ip] = (iPSI - iPSR) + (iWS - iWR)
            ttSpartData.DemandFrom[ip] = 'month ' + STRING(iy) + ' table ' + iphBuffer:NAME /* debug */
            ip                = ip + 1.  
    END.     
END PROCEDURE.

PROCEDURE CombineBranch:
    /* IF ALL BRANCH SPAREPART DATA REQUESTED, COMBINE ALL BRANCH FROM TEMP-TABLE */
    FOR EACH ttSpartData NO-LOCK:
        FIND FIRST ttSpartData2 WHERE
            ttSpartData.agency = ttSpartData2.agency AND 
            ttSpartData.partno = ttSpartData2.partno 
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE ttSpartData2 THEN 
        DO:
            DO iz = 1 TO EXTENT(ttSpartData.D):
                ttSpartData2.D[iz] = ttSpartData2.D[iz] + ttSpartData.D[iz].
            END.   
        END. 
        ELSE 
        DO:
            CREATE ttSpartData2.
            BUFFER-COPY ttSpartData EXCEPT branch TO ttSpartData2.
            ASSIGN 
                ttSpartData2.branch = 'ALL'.
        END.
    END.

    TEMP-TABLE ttSpartData:COPY-TEMP-TABLE(TEMP-TABLE ttSpartData2:HANDLE).
END PROCEDURE.