--- src/core/aws-client.adb.orig	2010-12-25 19:41:43 +0000
+++ src/core/aws-client.adb
@@ -225,7 +225,7 @@ package body AWS.Client is
 
       if Real_Time.Clock - Stamp >= Connection.Timeouts.Response
         or else Net.Is_Timeout (E)
-        or else Strings.Fixed.Index (Message, "] Connection timed out") > 0
+        or else Strings.Fixed.Index (Message, "] Operation timed out") > 0
       then
          Result := Response.Build
            (MIME.Text_Plain, Context & " Timeout", Messages.S408);
