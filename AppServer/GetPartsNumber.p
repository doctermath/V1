/* Object Input Parameter */

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray  FROM PROPATH.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE oJsonArr AS JsonArray NO-UNDO.
oJsonArr = NEW JsonArray().

DEFINE VARIABLE ix AS INTEGER NO-UNDO.

FIND FIRST SPARTHIS24.
DO ix = 1 TO 10:
    oJsonArr:Add(sparthis24.partno).
    FIND NEXT SPARTHIS24.
END.

oJsonArr:Add('129888').
oJsonArr:Add('130083').
oJsonArr:Add('131584').
oJsonArr:Add('132770').
oJsonArr:Add('205230').

oJson:Add('pn_list', oJsonArr).