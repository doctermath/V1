/* Object Input Parameter */

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray  FROM PROPATH.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE oJsonArr AS JsonArray NO-UNDO.
ASSIGN oJsonArr = CAST(poRequest:Entity,JsonArray).


DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
CREATE TEMP-TABLE hTable.
hTable:READ-JSON('JsonArray', oJsonArr, "empty").

DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
hBuffer = hTable:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.

hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE('FOR EACH ' + hBuffer:NAME + ' NO-LOCK').
hQuery:QUERY-OPEN().

ASSIGN ix = 0.
DO WHILE hQuery:GET-NEXT():
    FIND FIRST spartqty WHERE
        spartqty.brc    = hBuffer:BUFFER-FIELD('branch'):BUFFER-VALUE() AND 
        spartqty.agency = hBuffer:BUFFER-FIELD('agency'):BUFFER-VALUE() AND
        spartqty.partno = hBuffer:BUFFER-FIELD('partno'):BUFFER-VALUE()
        EXCLUSIVE-LOCK NO-ERROR.
        
    IF AVAILABLE spartqty THEN DO:
        ASSIGN
            spartqty.fcastd = hBuffer:BUFFER-FIELD('FD'):BUFFER-VALUE()
            spartqty.sdevi = hBuffer:BUFFER-FIELD('std_12'):BUFFER-VALUE()
            spartqty.meanqty = hBuffer:BUFFER-FIELD('mean_12'):BUFFER-VALUE()
            spartqty.upbound = hBuffer:BUFFER-FIELD('ub'):BUFFER-VALUE()
            .

        ASSIGN ix = ix + 1.
        MESSAGE 
            'Record No' ix 'Updated :' spartqty.brc 
            spartqty.agency spartqty.partno spartqty.upbound
            VIEW-AS ALERT-BOX.
    END.
    RELEASE spartqty NO-ERROR.
END.

oJson:Add('success', TRUE).
oJson:Add('message', STRING(ix) + ' records updated.').

hQuery:QUERY-CLOSE().

IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
IF VALID-HANDLE(hTable) THEN DELETE OBJECT hTable.