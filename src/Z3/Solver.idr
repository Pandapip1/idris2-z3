module Z3.Solver

import Z3.Types
import Z3.AST

%default total

%foreign "C:Z3_mk_solver,libz3,z3.h"
prim__z3_mk_solver : Z3_context -> PrimIO Z3_solver

%foreign "C:Z3_solver_del,libz3,z3.h"
prim__z3_solver_del : Z3_context -> Z3_solver -> PrimIO ()

%foreign "C:Z3_solver_assert,libz3,z3.h"
prim__z3_solver_assert : Z3_context -> Z3_solver -> Z3_ast -> PrimIO ()

%foreign "C:Z3_solver_check,libz3,z3.h"
prim__z3_solver_check : Z3_context -> Z3_solver -> PrimIO Int

%foreign "C:Z3_solver_get_model,libz3,z3.h"
prim__z3_solver_get_model : Z3_context -> Z3_solver -> PrimIO Z3_model

||| Create a new solver
public export
mkSolver : Context -> IO Z3_solver
mkSolver (MkContext ctx) = primIO $ prim__z3_mk_solver ctx

||| Delete a solver
public export
delSolver : Context -> Z3_solver -> IO ()
delSolver (MkContext ctx) solver = primIO $ prim__z3_solver_del ctx solver

||| Assert constraint
public export
solverAssert : Context -> Z3_solver -> Z3_ast -> IO ()
solverAssert (MkContext ctx) solver ast = 
  primIO $ prim__z3_solver_assert ctx solver ast

||| Check satisfiability
public export
solverCheck : Context -> Z3_solver -> IO CheckResult
solverCheck (MkContext ctx) solver = do
  res <- primIO $ prim__z3_solver_check ctx solver
  pure $ case res of
    1 => Sat
    0 => Unsat
    _ => Unknown

||| Get model if satisfiable
public export
solverGetModel : Context -> Z3_solver -> IO Z3_model
solverGetModel (MkContext ctx) solver = 
  primIO $ prim__z3_solver_get_model ctx solver
