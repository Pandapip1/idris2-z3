module Z3.Symbol

import Z3.Types

%default total

%foreign "C:Z3_mk_string_symbol,libz3,z3.h"
prim__z3_mk_string_symbol : Z3_context -> String -> PrimIO Z3_symbol

%foreign "C:Z3_mk_int_symbol,libz3,z3.h"
prim__z3_mk_int_symbol : Z3_context -> Int -> PrimIO Z3_symbol

||| Create a symbol from a string
public export
mkStringSymbol : Context -> String -> IO Z3_symbol
mkStringSymbol (MkContext ctx) name = 
  primIO $ prim__z3_mk_string_symbol ctx name

||| Create a symbol from an integer
public export
mkIntSymbol : Context -> Int -> IO Z3_symbol
mkIntSymbol (MkContext ctx) n = 
  primIO $ prim__z3_mk_int_symbol ctx n
