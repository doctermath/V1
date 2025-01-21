/* gdmdcall.p */

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

MESSAGE "BEGIN TEST 13" VIEW-AS ALERT-BOX.

DEFINE VARIABLE ix AS INT NO-UNDO.

/* Temp-Table Definitions
--------------------------------------------------------------------------------*/
DEFINE TEMP-TABLE tt_Car NO-UNDO
    FIELD plateNumber AS CHARACTER
    FIELD brand AS CHARACTER
    FIELD type AS CHARACTER
    FIELD colorx AS CHARACTER
    FIELD model AS CHARACTER
    FIELD lastSeen AS DATE
    FIELD statusx AS CHARACTER
    .

IF poRequest:URI:GetQueryValue('plat') = "B 1212 XXX" THEN DO:
    MESSAGE 'PLAT' poRequest:URI:GetQueryValue('plat') 'MATCHED'.
    CREATE tt_Car.
    ASSIGN 
        tt_Car.plateNumber = "B 1212 XXX"
        tt_Car.brand       = "DAIHATSU"
        tt_Car.type        = "XENIA"
        tt_Car.colorx      = "Silver"
        tt_Car.model       = "MPV (Multi-Purpose Vehicle)"
        tt_Car.lastSeen    = DATE(5,1,2024)
        tt_Car.statusx     = "IN".
END.
ELSE DO:
    MESSAGE 'PLAT' poRequest:URI:GetQueryValue('plat') 'NOT MATCHED'.
    oJson:add('message', 'Plat not match').
END.

DEFINE VARIABLE oJsonArr AS JsonArray NO-UNDO.
oJsonArr = NEW JsonArray().

oJsonArr:Read(TEMP-TABLE tt_Car:HANDLE).
oJson:add('data', oJsonArr).

