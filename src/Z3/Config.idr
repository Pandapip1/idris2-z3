module Z3.Config

import Z3.Types

%default total

%foreign "C:Z3_set_param_value,libz3,z3.h"
prim__z3_set_param_value : Z3_config -> String -> String -> PrimIO ()

||| Set a configuration parameter
public export
setParamValue : Context -> String -> String -> IO ()
setParamValue (MkContext ctx) param value = 
  primIO $ prim__z3_set_param_value ctx param value
