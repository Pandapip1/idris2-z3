module Z3.FuncDecl

import Z3.Types
import Z3.Sort
import Z3.Symbol

%default total

%foreign "C:Z3_mk_func_decl,libz3,z3.h"
prim__z3_mk_func_decl : Z3_context -> Z3_symbol -> Int -> AnyPtr -> Z3_sort -> PrimIO Z3_func_decl

||| Create a function declaration
public export
mkFuncDecl : Context -> Z3_symbol -> Vect n Z3_sort -> Z3_sort -> IO Z3_func_decl
mkFuncDecl (MkContext ctx) sym domain_sorts range_sort = do
  arr <- believe_me (the (List AnyPtr) domain_sorts)
  primIO $ prim__z3_mk_func_decl ctx sym (cast (length domain_sorts)) arr range_sort
