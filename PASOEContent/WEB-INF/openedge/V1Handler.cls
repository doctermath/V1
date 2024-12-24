
BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Web.WebHandler FROM PROPATH.
USING OpenEdge.Web.WebResponseWriter FROM PROPATH.
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.

CLASS V1Handler INHERITS WebHandler: 
    
    /* Private variable definitions */
    DEFINE PRIVATE VARIABLE oResponse AS OpenEdge.Web.WebResponse NO-UNDO.
    DEFINE PRIVATE VARIABLE oJson     AS JsonObject               NO-UNDO.
    DEFINE PRIVATE VARIABLE cUri      AS CHARACTER                NO-UNDO.

    METHOD OVERRIDE PROTECTED INTEGER HandleNotAllowedMethod( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
        UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT ALLOWED").
    END METHOD.

    METHOD OVERRIDE PROTECTED INTEGER HandleNotImplemented( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
        UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").
    END METHOD.

    /* Get Method */
    METHOD OVERRIDE PROTECTED INTEGER HandleGet( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
          
        /* Object Declarations ==================  */      
        ASSIGN 
            oResponse            = NEW OpenEdge.Web.WebResponse()
            oJson                = NEW JsonObject()
            oResponse:StatusCode = 200.

        
            /* URI Conditioning */
            cUri = RIGHT-TRIM(poRequest:UriTemplate, '/':u).
            CASE cUri:
                /* Get Parts Number List Per Offset, Limit, Or Search */
                WHEN '/get-parts-number' THEN 
                RUN GetPartsNumber.p (poRequest, oJson).
                
                /* Get Parts Stock Data */
                WHEN '/get-spare-parts-all-history-data' THEN 
                RUN GetPartsNumberData.p (poRequest, oJson).
            
    
            END CASE.
           

        /* Response Content ================== */           
        ASSIGN 
            oResponse:Entity      = ojson
            oResponse:ContentType = 'application/json'.
        
        /* Send Response Back  ================== */    
        sendResponse(oResponse).
        RETURN 0.
    END METHOD. 
    
    /* Response Writer */
    METHOD PROTECTED VOID sendResponse(INPUT oResponse AS OpenEdge.Web.WebResponse):
        DEFINE VARIABLE oWriter AS OpenEdge.Web.WebResponseWriter NO-UNDO.
           
        ASSIGN 
            oWriter = NEW WebResponseWriter(oResponse).
        oWriter:Open().
        oWriter:Close().
        
    END METHOD.
    
END CLASS.