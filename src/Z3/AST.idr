module Z3.AST

import Z3.Types
import Z3.Sort
import Z3.Symbol

%default total

%foreign "C:Z3_mk_const,libz3,z3.h"
prim__z3_mk_const : Z3_context -> Z3_symbol -> Z3_sort -> PrimIO Z3_ast

%foreign "C:Z3_mk_true,libz3,z3.h"
prim__z3_mk_true : Z3_context -> PrimIO Z3_ast

%foreign "C:Z3_mk_false,libz3,z3.h"
prim__z3_mk_false : Z3_context -> PrimIO Z3_ast

%foreign "C:Z3_mk_int,libz3,z3.h"
prim__z3_mk_int : Z3_context -> Int -> Z3_sort -> PrimIO Z3_ast

%foreign "C:Z3_mk_real,libz3,z3.h"
prim__z3_mk_real : Z3_context -> Int -> Int -> PrimIO Z3_ast

%foreign "C:Z3_mk_eq,libz3,z3.h"
prim__z3_mk_eq : Z3_context -> Z3_ast -> Z3_ast -> PrimIO Z3_ast

%foreign "C:Z3_mk_and,libz3,z3.h"
prim__z3_mk_and : Z3_context -> Int -> AnyPtr -> PrimIO Z3_ast

%foreign "C:Z3_mk_or,libz3,z3.h"
prim__z3_mk_or : Z3_context -> Int -> AnyPtr -> PrimIO Z3_ast

%foreign "C:Z3_mk_not,libz3,z3.h"
prim__z3_mk_not : Z3_context -> Z3_ast -> PrimIO Z3_ast

||| Create a constant
public export
mkConst : Context -> Z3_symbol -> Z3_sort -> IO Z3_ast
mkConst (MkContext ctx) sym sort = 
  primIO $ prim__z3_mk_const ctx sym sort

||| Create true constant
public export
mkTrue : Context -> IO Z3_ast
mkTrue (MkContext ctx) = primIO $ prim__z3_mk_true ctx

||| Create false constant
public export
mkFalse : Context -> IO Z3_ast
mkFalse (MkContext ctx) = primIO $ prim__z3_mk_false ctx

||| Create integer constant
public export
mkInt : Context -> Int -> Z3_sort -> IO Z3_ast
mkInt (MkContext ctx) n sort = 
  primIO $ prim__z3_mk_int ctx n sort

||| Create equality
public export
mkEq : Context -> Z3_ast -> Z3_ast -> IO Z3_ast
mkEq (MkContext ctx) a b = 
  primIO $ prim__z3_mk_eq ctx a b

||| Create AND operation
public export
mkAnd : Context -> List Z3_ast -> IO Z3_ast
mkAnd (MkContext ctx) asts = do
  arr <- believe_me (the (List AnyPtr) asts)
  primIO $ prim__z3_mk_and ctx (cast (length asts)) arr

||| Create OR operation
public export
mkOr : Context -> List Z3_ast -> IO Z3_ast
mkOr (MkContext ctx) asts = do
  arr <- believe_me (the (List AnyPtr) asts)
  primIO $ prim__z3_mk_or ctx (cast (length asts)) arr

||| Create NOT operation
public export
mkNot : Context -> Z3_ast -> IO Z3_ast
mkNot (MkContext ctx) ast = 
  primIO $ prim__z3_mk_not ctx ast