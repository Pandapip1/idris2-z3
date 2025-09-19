module Z3.Model

import Z3.Types

%default total

%foreign "C:Z3_model_eval,libz3,z3.h"
prim__z3_model_eval : Z3_context -> Z3_model -> Z3_ast -> Int -> AnyPtr -> PrimIO Int

%foreign "C:Z3_model_get_num_consts,libz3,z3.h"
prim__z3_model_get_num_consts : Z3_context -> Z3_model -> PrimIO Int

%foreign "C:Z3_model_get_const_decl,libz3,z3.h"
prim__z3_model_get_const_decl : Z3_context -> Z3_model -> Int -> PrimIO Z3_func_decl

%foreign "C:Z3_model_get_const_interp,libz3,z3.h"
prim__z3_model_get_const_interp : Z3_context -> Z3_model -> Z3_func_decl -> PrimIO Z3_ast

%foreign "C:Z3_model_to_string,libz3,z3.h"
prim__z3_model_to_string : Z3_context -> Z3_model -> PrimIO String

%foreign "C:idris_mk_null_ptr,libidris2_support,idris_support.h"
prim__mk_null_ptr : PrimIO AnyPtr

||| Create a null pointer
mkNullPtr : IO AnyPtr
mkNullPtr = primIO prim__mk_null_ptr

||| Evaluate AST in model
public export
modelEval : Context -> Z3_model -> Z3_ast -> Bool -> IO (Maybe Z3_ast)
modelEval (MkContext ctx) model ast complete = do
  nullPtr <- mkNullPtr
  result <- primIO $ prim__z3_model_eval ctx model ast (if complete then 1 else 0) nullPtr
  if result == 0
     then pure Nothing
     else pure (Just ast)

||| Get number of constants in model
public export
modelGetNumConsts : Context -> Z3_model -> IO Int
modelGetNumConsts (MkContext ctx) model = 
  primIO $ prim__z3_model_get_num_consts ctx model

||| Convert model to string
public export
modelToString : Context -> Z3_model -> IO String
modelToString (MkContext ctx) model = 
  primIO $ prim__z3_model_to_string ctx model
