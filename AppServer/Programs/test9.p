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

FOR EACH tmptbl WHERE prtid = 'yordan':
    LOG('Deleting TMPTBL ' + prtid + ' ' + char1).
    DELETE tmptbl.
END.

REPEAT ix = 1 TO 65000:
    CREATE tmptbl.
    ASSIGN 
        prtid = 'yordan' 
        char1 = 'seq ' + STRING(ix, "9999999").
    LOG('Created TMPTBL ' + prtid + ' ' + char1).
END.


/*DO ix = 1 TO 10000 ON ERROR UNDO, LEAVE:
    CREATE tmptbl.
    ASSIGN 
        prtid = 'yordan' 
        char1 = 'seq ' + STRING(ix).
    LOG('Created TMPTBL ' + prtid + ' ' + char1).
    RELEASE tmptbl.
END.*/
    

FOR EACH tmptbl WHERE prtid = 'yordan':
    LOG('Creating JSON Record ' + char1).
    oJson:Add(char1, prtid).
    
END.
 
    