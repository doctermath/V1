&SCOPED-DEFINE a spartqty  
  
DEFINE TEMP-TABLE ttdata NO-UNDO LIKE {&a}.  
  
DEFINE VARIABLE ix AS INTEGER NO-UNDO.  
DEFINE VARIABLE i AS INTEGER NO-UNDO.  
DEFINE VARIABLE j AS INTEGER NO-UNDO.  
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.  
DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.  
  
/* Copy related data to ttdata */  
ix = 1.
FOR EACH {&a} NO-LOCK:
    ix = ix + 1.
    IF ix > 10000 THEN DO:
        LEAVE.
    END.
    CREATE ttdata.  
    BUFFER-COPY {&a} TO ttdata.  
END.  
  
/* Start the output */  
OUTPUT TO VALUE("C:\Users\User\Desktop\Hellstad\{&a}.csv").  
ASSIGN iCount = BUFFER ttData:NUM-FIELDS .  
DO i = 1 TO iCount:  
    iExtent = BUFFER ttData:BUFFER-FIELD (i):EXTENT .  
    IF iExtent > 1 THEN  
    DO:  
        DO j = 1 TO iExtent:  
            PUT UNFORMATTED  
                (IF i > 1 OR j > 1 THEN "," ELSE "")  
                SUBSTITUTE (  
                "&1[&2]",  
                BUFFER ttData:BUFFER-FIELD (i):NAME,  
                j) .  
        END.  
    END.  
    ELSE  
        PUT UNFORMATTED  
            (IF i > 1 THEN "," ELSE "")  
            BUFFER ttData:BUFFER-FIELD (i):LABEL .  
END.  
  
PUT UNFORMATTED SKIP.  
  
FOR EACH ttdata NO-LOCK:  
    EXPORT DELIMITER "," ttdata.  
END.  
  
OUTPUT CLOSE.