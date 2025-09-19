module Z3.Sort

import Z3.Types

%default total

%foreign "C:Z3_mk_bool_sort,libz3,z3.h"
prim__z3_mk_bool_sort : Z3_context -> PrimIO Z3_sort

%foreign "C:Z3_mk_int_sort,libz3,z3.h"
prim__z3_mk_int_sort : Z3_context -> PrimIO Z3_sort

%foreign "C:Z3_mk_real_sort,libz3,z3.h"
prim__z3_mk_real_sort : Z3_context -> PrimIO Z3_sort

%foreign "C:Z3_mk_bv_sort,libz3,z3.h"
prim__z3_mk_bv_sort : Z3_context -> Int -> PrimIO Z3_sort

||| Create boolean sort
public export
mkBoolSort : Context -> IO Z3_sort
mkBoolSort (MkContext ctx) = primIO $ prim__z3_mk_bool_sort ctx

||| Create integer sort
public export
mkIntSort : Context -> IO Z3_sort
mkIntSort (MkContext ctx) = primIO $ prim__z3_mk_int_sort ctx

||| Create real sort
public export
mkRealSort : Context -> IO Z3_sort
mkRealSort (MkContext ctx) = primIO $ prim__z3_mk_real_sort ctx

||| Create bitvector sort of given size
public export
mkBvSort : Context -> Int -> IO Z3_sort
mkBvSort (MkContext ctx) size = primIO $ prim__z3_mk_bv_sort ctx size
