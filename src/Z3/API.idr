module Z3.API

import Z3.Types
import Z3.Config
import Z3.Symbol
import Z3.Sort
import Z3.AST
import Z3.Solver
import Z3.Model

%default total

||| Get the current context
public export
getContext : Z3M Context
getContext = MkZ3M $ \ctx => pure (Right ctx)

||| Create a boolean variable
public export
boolVar : String -> Z3M Z3_ast
boolVar name = do
  ctx <- getContext
  sym <- liftIO $ mkStringSymbol ctx name
  sort <- liftIO $ mkBoolSort ctx
  liftIO $ mkConst ctx sym sort

||| Create an integer variable
public export
intVar : String -> Z3M Z3_ast
intVar name = do
  ctx <- getContext
  sym <- liftIO $ mkStringSymbol ctx name
  sort <- liftIO $ mkIntSort ctx
  liftIO $ mkConst ctx sym sort

||| Assert a constraint
public export
assert : Z3_ast -> Z3M ()
assert ast = do
  ctx <- getContext
  solver <- liftIO $ mkSolver ctx
  liftIO $ solverAssert ctx solver ast

||| Check satisfiability
public export
check : Z3M CheckResult
check = do
  ctx <- getContext
  solver <- liftIO $ mkSolver ctx
  liftIO $ solverCheck ctx solver
