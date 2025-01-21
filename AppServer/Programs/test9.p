/* Test 9 : Create A Lot Of Record and put it into tmptbl */


USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE ix AS INTEGER NO-UNDO.

FUNCTION LOG RETURN INTEGER (INPUT msg AS CHARACTER):
    MESSAGE STRING(NOW, "99/99/99 HH:MM:SS") "- " msg
        VIEW-AS ALERT-BOX.
END.

DEFINE VARIABLE oJsonIn AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonOut AS JsonObject NO-UNDO.

/* Input Parameter for sourcer  */ 
oJsonIn = NEW JsonObject().
oJsonIn:Add('start-date', poRequest:URI:GetQueryValue('start-date')).
oJsonIn:Add('end-date', poRequest:URI:GetQueryValue('end-date')).
oJsonIn:Add('exclude-older', poRequest:URI:GetQueryValue('exclude-older')).
oJsonIn:Add('branch', poRequest:URI:GetQueryValue('branch')).
oJsonIn:Add('agency', poRequest:URI:GetQueryValue('agency')).
oJsonIn:Add('partno', porequest:URI:GetQueryValue('partno')).

RUN Logic/gdmdcall.p(INPUT oJsonIn, OUTPUT oJsonOut).

oJson:Add('start-date', oJsonIn:GetDate("start-date")).
oJson:Add('end-date', oJsonIn:GetDate("end-date")).
oJson:Add('data-count', oJsonOut:GetInteger("data-count")).
oJson:Add('month-count', oJsonOut:GetInteger("month-count")).
oJson:Add('year-count', oJsonOut:GetInteger("year-count")).
oJson:Add('data', oJsonOut:GetJsonArray('data')).

    