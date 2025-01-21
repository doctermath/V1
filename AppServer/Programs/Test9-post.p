/* Object Input Parameter */

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray  FROM PROPATH.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE oInputEntity AS JsonArray NO-UNDO.

DEFINE VARIABLE ix       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy       AS INTEGER   NO-UNDO.

DEFINE VARIABLE hTable   AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer  AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.

/* Write Python Sparepart forecaset data to table
========================================================================== */
MESSAGE "Retriving Data From Python Begins... ".
ASSIGN oInputEntity = CAST(poRequest:Entity, JsonArray).
    
CREATE TEMP-TABLE hTable.
hTable:READ-JSON('JsonArray', oInputEntity, "empty").

hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
    
CREATE QUERY hQuery.
hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE('FOR EACH ' + hBuffer:NAME + ' NO-LOCK').
hQuery:QUERY-OPEN().

ASSIGN ix = 0 iy = 0.
REPEAT WHILE hQuery:GET-NEXT():
    ix = ix + 1.
    MESSAGE "Processing No" ix hBuffer::branch hBuffer::agency hBuffer::partno.
    IF hBuffer::branch <> 'ALL' THEN DO:
        FIND FIRST spartqty WHERE
            hBuffer::branch = spartqty.brc AND
            hBuffer::agency = spartqty.agency AND
            hBuffer::partno = spartqty.partno
            EXCLUSIVE-LOCK NO-ERROR. 
               
        IF AVAILABLE spartqty THEN DO:
            iy = iy + 1.
            ASSIGN
                spartqty.fcastd  = hBuffer::fd
                spartqty.sdevi   = hBuffer::std
                spartqty.meanqty = hBuffer::mean
                spartqty.upbound = hBuffer::ub
                .
        END.
        ELSE DO:
            MESSAGE 'NO spartqty data found...'.    
        END.
        RELEASE spartqty.
    END.
    ELSE DO:
        FIND FIRST cpmf WHERE
            hBuffer::agency = cpmf.agency AND
            hBuffer::partno = cpmf.partno
            EXCLUSIVE-LOCK NO-ERROR.
                
        IF AVAILABLE cpmf THEN DO:
            iy = iy + 1.
            ASSIGN
                cpmf.fcastd  = hBuffer::fd
                cpmf.sdevi   = hBuffer::std
                cpmf.meanqty = hBuffer::mean
                cpmf.upbound = hBuffer::ub
                .
        END.
        ELSE DO:
            MESSAGE 'NO cpmf data found...'.
        END.
        RELEASE cpmf.
    END.
    
END.

MESSAGE "PROCESSING DATA IS COMPLETED" STRING(iy) "DATA IS UPDATED".
oJson:Add('success', TRUE).
oJson:Add('message', STRING(iy) + ' records updated.').

hQuery:QUERY-CLOSE().

IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
IF VALID-HANDLE(hTable) THEN DELETE OBJECT hTable.