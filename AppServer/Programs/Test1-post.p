/* Object Input Parameter */

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray  FROM PROPATH.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE ix       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lAppend  AS LOGICAL NO-UNDO.
DEFINE VARIABLE oJsonArr AS JsonArray NO-UNDO.

MESSAGE "Retriving Data From Python Begins... "
    VIEW-AS ALERT-BOX.

ASSIGN 
    oJsonArr = CAST(poRequest:Entity,JsonArray)
    lAppend = LOGICAL(poRequest:URI:GetQueryValue('append')) 
    .
    
DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
CREATE TEMP-TABLE hTable.
hTable:READ-JSON('JsonArray', oJsonArr, "empty").

IF NOT lAppend THEN DO:
    ix = 0.
    MESSAGE "Deleting Previous Data in TMPTBL ..."
        VIEW-AS ALERT-BOX.
    
    FOR EACH tmptbl WHERE prtid = 'fccpmf' OR prtid = 'fcsqty' EXCLUSIVE-LOCK:
        ix = ix + 1.
        /*MESSAGE ix 'deleting' prtid 'in tmptbl' char3
            VIEW-AS ALERT-BOX.*/
        DELETE tmptbl.
    END.
    MESSAGE "Deleted Previous Data in TMPTBL"
        VIEW-AS ALERT-BOX.
    
END.

DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
hBuffer = hTable:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.

hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE('FOR EACH ' + hBuffer:NAME + ' NO-LOCK').
hQuery:QUERY-OPEN().

MESSAGE "Creating TMPTBL Begins..."
    VIEW-AS ALERT-BOX.

ASSIGN ix = 0.
REPEAT WHILE hQuery:GET-NEXT():
    ix = ix + 1.
    CREATE tmptbl.
    
    IF hBuffer:BUFFER-FIELD('branch'):BUFFER-VALUE() = 'ALL' THEN ASSIGN tmptbl.prtid = 'fccpmf'.
    ELSE ASSIGN tmptbl.prtid = 'fcsqty'.

    ASSIGN
        tmptbl.char1 = hBuffer:BUFFER-FIELD('period'):BUFFER-VALUE()
        tmptbl.char2 = hBuffer:BUFFER-FIELD('branch'):BUFFER-VALUE()
        tmptbl.char3 = hBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE()
        tmptbl.char4 = hBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
        tmptbl.int1  = hBuffer:BUFFER-FIELD('FD'):BUFFER-VALUE()        /* Forecast Demand */
        tmptbl.dec1  = hBuffer:BUFFER-FIELD('mean_12'):BUFFER-VALUE()   /* Mean(AVG) of First 12 data */
        tmptbl.dec2  = hBuffer:BUFFER-FIELD('std_12'):BUFFER-VALUE()    /* Standard Deviation of First 12 data */
        tmptbl.dec3  = hBuffer:BUFFER-FIELD('ub'):BUFFER-VALUE()        /* Upper Bound of First 12 data */
        .

    /*MESSAGE 'Created TMPTBL No' ix 'Data :' prtid char4 
        VIEW-AS ALERT-BOX.*/
    RELEASE tmptbl.
END.


FIND LAST tmptbl WHERE prtid = 'fcsqty' OR prtid = 'fccpmf' NO-LOCK NO-ERROR.
IF AVAILABLE tmptbl THEN MESSAGE ix "TMPTBL Data Created" prtid char1 char4
        VIEW-AS ALERT-BOX.

oJson:Add('success', TRUE).
oJson:Add('message', STRING(ix) + ' records updated.').

hQuery:QUERY-CLOSE().

IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
IF VALID-HANDLE(hTable) THEN DELETE OBJECT hTable.