/* gdmdcall.p */

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

MESSAGE "BEGIN TEST 12" VIEW-AS ALERT-BOX.

/* Temp-Table Definitions
--------------------------------------------------------------------------------*/
/* Define Temp Table To Save part number  */
DEFINE TEMP-TABLE ttd
    FIELD branch LIKE brctable.CODE
    FIELD agency LIKE cpmf.agency
    FIELD partno LIKE cpmf.partno
    FIELD d      AS INT64   EXTENT 16.

DEFINE VARIABLE ix AS INT NO-UNDO.


FOR EACH ypart:
    ix = ix + 1.
    CREATE ttd.
    ASSIGN
        ttd.branch = ypart.branch
        ttd.agency = ypart.agency
        ttd.partno = ypart.partno
        ttd.d = ypart.d
        .
END.

DEFINE VARIABLE oJsonArr AS JsonArray NO-UNDO.
oJsonArr = NEW JsonArray().

oJsonArr:Read(TEMP-TABLE ttd:HANDLE).
oJson:add('data-count', 10).
oJson:add('data', oJsonArr).

