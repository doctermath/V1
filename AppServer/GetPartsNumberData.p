/* Object Input Parameter */

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray FROM PROPATH.

DEFINE TEMP-TABLE ttData
    FIELD PartNum AS CHARACTER
    FIELD DTM     AS INTEGER
    FIELD D-1      AS INTEGER
    FIELD D-2      AS INTEGER
    FIELD D-3      AS INTEGER
    FIELD D-4      AS INTEGER
    FIELD D-5      AS INTEGER
    FIELD D-6      AS INTEGER
    FIELD D-7      AS INTEGER
    FIELD D-8      AS INTEGER
    FIELD D-9      AS INTEGER
    FIELD D-10     AS INTEGER
    FIELD D-11     AS INTEGER
    FIELD D-12     AS INTEGER.
    
DEFINE TEMP-TABLE ttData2 LIKE ttData.

DEFINE INPUT PARAMETER poRequest    AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE INPUT PARAMETER oJson        AS JsonObject NO-UNDO.

DEFINE VARIABLE cPartNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE oJsonArr  AS JsonArray NO-UNDO.
DEFINE VARIABLE oJson2     AS JsonObject NO-UNDO.

CREATE ttData.
ASSIGN PartNum = "129888" DTM = 2 D-1 = 10 D-2 = 6 D-3 = 2 D-4 = 2 D-5 = 12
    D-6 = 0 D-7 = 8 D-8 = 4 D-9 = 14 D-10 = 8 D-11 = 2 D-12 = 8.

CREATE ttData.
ASSIGN PartNum = "130083" DTM = 0 D-1 = 4 D-2 = 18 D-3 = 6 D-4 = 6 D-5 = 10
    D-6 = 24 D-7 = 6 D-8 = 16 D-9 = 12 D-10 = 18 D-11 = 0 D-12 = 6.

CREATE ttData.
ASSIGN PartNum = "131584" DTM = 2 D-1 = 2 D-2 = 7 D-3 = 6 D-4 = 0 D-5 = 2
    D-6 = 2 D-7 = 3 D-8 = 7 D-9 = 8 D-10 = 8 D-11 = 0 D-12 = 1.

CREATE ttData.
ASSIGN PartNum = "132770" DTM = 0 D-1 = 2 D-2 = 0 D-3 = 0 D-4 = 0 D-5 = 2
    D-6 = 0 D-7 = 0 D-8 = 2 D-9 = 0 D-10 = 0 D-11 = 0 D-12 = 0.

CREATE ttData.
ASSIGN PartNum = "205230" DTM = 60 D-1 = 60 D-2 = 60 D-3 = 60 D-4 = 60 D-5 = 60
    D-6 = 102 D-7 = 0 D-8 = 94 D-9 = 26 D-10 = 26 D-11 = 94 D-12 = 34.


ASSIGN cPartNumber = poRequest:URI:GetQueryValue('partnumber').
FIND FIRST ttData WHERE ttData.PartNum = cPartNumber NO-LOCK NO-ERROR.
IF AVAILABLE ttData THEN DO:
    CREATE ttData2.
    BUFFER-COPY ttData TO ttData2.    
END.

oJsonArr = NEW JsonArray().
oJsonArr:READ(TEMP-TABLE ttData2:HANDLE).

oJson:Add('data', oJsonArr).


/*oArray = NEW JsonArray().

/* Add data rows to the JSON array */
row = NEW JsonObject().
row:Add("P/N", "129888").
row:Add("DTM", 2).
row:Add("D-1", 10).
row:Add("D-2", 6).
row:Add("D-3", 2).
row:Add("D-4", 2).
row:Add("D-5", 12).
row:Add("D-6", 0).
row:Add("D-7", 8).
row:Add("D-8", 4).
row:Add("D-9", 14).
row:Add("D-10", 8).
row:Add("D-11", 2).
row:Add("D-12", 8).
oArray:Add(row).

row = NEW JsonObject().
row:Add("P/N", "130083").
row:Add("DTM", 0).
row:Add("D-1", 4).
row:Add("D-2", 18).
row:Add("D-3", 6).
row:Add("D-4", 6).
row:Add("D-5", 10).
row:Add("D-6", 24).
row:Add("D-7", 6).
row:Add("D-8", 16).
row:Add("D-9", 12).
row:Add("D-10", 18).
row:Add("D-11", 0).
row:Add("D-12", 6).
oArray:Add(row).

row = NEW JsonObject().
row:Add("P/N", "131584").
row:Add("DTM", 2).
row:Add("D-1", 2).
row:Add("D-2", 7).
row:Add("D-3", 6).
row:Add("D-4", 0).
row:Add("D-5", 2).
row:Add("D-6", 2).
row:Add("D-7", 3).
row:Add("D-8", 7).
row:Add("D-9", 8).
row:Add("D-10", 8).
row:Add("D-11", 0).
row:Add("D-12", 1).
oArray:Add(row).

row = NEW JsonObject().
row:Add("P/N", "132770").
row:Add("DTM", 0).
row:Add("D-1", 2).
row:Add("D-2", 0).
row:Add("D-3", 0).
row:Add("D-4", 0).
row:Add("D-5", 2).
row:Add("D-6", 0).
row:Add("D-7", 0).
row:Add("D-8", 2).
row:Add("D-9", 0).
row:Add("D-10", 0).
row:Add("D-11", 0).
row:Add("D-12", 0).
oArray:Add(row).

row = NEW JsonObject().
row:Add("P/N", "205230").
row:Add("DTM", 60).
row:Add("D-1", 60).
row:Add("D-2", 60).
row:Add("D-3", 60).
row:Add("D-4", 60).
row:Add("D-5", 60).
row:Add("D-6", 102).
row:Add("D-7", 0).
row:Add("D-8", 94).
row:Add("D-9", 26).
row:Add("D-10", 26).
row:Add("D-11", 94).
row:Add("D-12", 34).
oArray:Add(row).

/* Add the array to the main JSON object */
oJson:Add("data", oArray).*/


