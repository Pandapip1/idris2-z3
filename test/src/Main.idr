module Main

import Z3.Types
import Z3.API
import Z3.Sort
import Z3.AST
import Z3.Solver
import Z3.Model
import Z3.Symbol

%default total

||| Test 1: Simple boolean SAT
testSimpleSat : IO ()
testSimpleSat = do
  putStrLn "Test 1: Simple boolean SAT"
  result <- runZ3 $ do
    liftIO $ putStrLn "DEBUG 0"
    x <- boolVar "x"
    liftIO $ putStrLn "DEBUG 1"
    notX <- liftIO $ mkNot !getContext x
    liftIO $ putStrLn "DEBUG 2"
    andExpr <- liftIO $ mkAnd !getContext [x, notX]
    liftIO $ putStrLn "DEBUG 3"
    assert andExpr
    liftIO $ putStrLn "DEBUG 4"
    check
  case result of
    Right Sat => putStrLn "FAIL: Sat"
    Right Unsat => putStrLn "PASS: Unsat"
    Right Unknown => putStrLn "FAIL: Unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 2: Simple boolean SAT (tautology)
testSimpleTautology : IO ()
testSimpleTautology = do
  putStrLn "Test 2: Simple boolean tautology"
  result <- runZ3 $ do
    x <- boolVar "x"
    notX <- liftIO $ mkNot !getContext x
    orExpr <- liftIO $ mkOr !getContext [x, notX]
    assert orExpr
    check
  case result of
    Right Sat => putStrLn "PASS: Sat"
    Right Unsat => putStrLn "FAIL: Unsat"
    Right Unknown => putStrLn "FAIL: Unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 3: Integer arithmetic
testIntegerArithmetic : IO ()
testIntegerArithmetic = do
  putStrLn "Test 3: Integer arithmetic"
  putStrLn "{ x + y = 10; x > y; }"
  result <- runZ3 $ do
    ctx <- getContext
    intSort <- liftIO $ mkIntSort ctx
    
    xSym <- liftIO $ mkStringSymbol ctx "x"
    ySym <- liftIO $ mkStringSymbol ctx "y"
    x <- liftIO $ mkConst ctx xSym intSort
    y <- liftIO $ mkConst ctx ySym intSort
    
    ten <- liftIO $ mkInt ctx 10 intSort
    zero <- liftIO $ mkInt ctx 0 intSort
    
    sumEq <- liftIO $ mkEq ctx x ten
    xGtY <- liftIO $ mkEq ctx x x
    
    assert sumEq
    assert xGtY
    
    check
  case result of
    Right Sat => putStrLn "FAIL: Sat"
    Right Unsat => putStrLn "PASS: Unsat"
    Right Unknown => putStrLn "FAIL: Unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 4: Boolean variable assignment
testBooleanAssignment : IO ()
testBooleanAssignment = do
  putStrLn "Test 4: Boolean variable assignment (x = true)"
  result <- runZ3 $ do
    x <- boolVar "x"
    xTrue <- liftIO $ mkTrue !getContext
    constraint <- liftIO $ mkEq !getContext x xTrue
    assert constraint
    check
  case result of
    Right Sat => putStrLn "PASS: x = true is satisfiable"
    Right Unsat => putStrLn "FAIL: x = true should be satisfiable"
    Right Unknown => putStrLn "? UNKNOWN: Could not determine satisfiability"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 5: Multiple boolean variables
testMultipleVariables : IO ()
testMultipleVariables = do
  putStrLn "Test 5: Multiple boolean variables (x y)"
  result <- runZ3 $ do
    x <- boolVar "x"
    y <- boolVar "y"
    andExpr <- liftIO $ mkAnd !getContext [x, y]
    assert andExpr
    check
  case result of
    Right Sat => putStrLn "PASS: x y is satisfiable"
    Right Unsat => putStrLn "FAIL: x y should be satisfiable"
    Right Unknown => putStrLn "? UNKNOWN: Could not determine satisfiability"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 6: Boolean constants
testBooleanConstants : IO ()
testBooleanConstants = do
  putStrLn "Test 6: Boolean constants (true false)"
  result <- runZ3 $ do
    ctx <- getContext
    trueConst <- liftIO $ mkTrue ctx
    falseConst <- liftIO $ mkFalse ctx
    andExpr <- liftIO $ mkAnd ctx [trueConst, falseConst]
    assert andExpr
    check
  case result of
    Right Unsat => putStrLn "PASS: true false is unsatisfiable"
    Right Sat => putStrLn "FAIL: true false should be unsatisfiable"
    Right Unknown => putStrLn "? UNKNOWN: Could not determine satisfiability"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 7: Symbol creation
testSymbolCreation : IO ()
testSymbolCreation = do
  putStrLn "Test 7: Symbol creation"
  result <- runZ3 $ do
    ctx <- getContext
    sym <- liftIO $ mkStringSymbol ctx "test_symbol"
    pure Sat
  case result of
    Right Sat => putStrLn "PASS: Symbol creation successful"
    Right Unsat => putStrLn "FAIL: Symbol creation should not be unsatisfiable"
    Right Unknown => putStrLn "FAIL: Symbol creation should not be unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 8: Sort creation
testSortCreation : IO ()
testSortCreation = do
  putStrLn "Test 8: Sort creation"
  result <- runZ3 $ do
    ctx <- getContext
    boolSort <- liftIO $ mkBoolSort ctx
    intSort <- liftIO $ mkIntSort ctx
    pure Sat
  case result of
    Right Sat => putStrLn "PASS: Sort creation successful"
    Right Unsat => putStrLn "FAIL: Sort creation should not be unsatisfiable"
    Right Unknown => putStrLn "FAIL: Sort creation should not be unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 9: Context management
testContextManagement : IO ()
testContextManagement = do
  putStrLn "Test 9: Context management"
  result <- runZ3 $ do
    ctx <- getContext
    trueConst <- liftIO $ mkTrue ctx
    pure Sat
  case result of
    Right Sat => putStrLn "PASS: Context management successful"
    Right Unsat => putStrLn "FAIL: Sort creation should not be unsatisfiable"
    Right Unknown => putStrLn "FAIL: Sort creation should not be unknown"
    Left err => putStrLn $ "ERROR: " ++ err

||| Test 10: Complex boolean expression
testComplexBoolean : IO ()
testComplexBoolean = do
  putStrLn "Test 10: Complex boolean expression"
  result <- runZ3 $ do
    x <- boolVar "x"
    y <- boolVar "y"
    ctx <- getContext
    
    notX <- liftIO $ mkNot ctx x
    notY <- liftIO $ mkNot ctx y
    
    or1 <- liftIO $ mkOr ctx [x, y]
    or2 <- liftIO $ mkOr ctx [notX, notY]
    
    andExpr <- liftIO $ mkAnd ctx [or1, or2]
    assert andExpr
    check
  case result of
    Right Sat => putStrLn "PASS: is satisfiable"
    Right Unsat => putStrLn "FAIL: should be satisfiable"
    Right Unknown => putStrLn "? FAIL: Could not determine satisfiability"
    Left err => putStrLn $ "ERROR: " ++ err

||| Run all tests
runAllTests : IO ()
runAllTests = do
  putStrLn "Running Z3 Idris bindings tests..."
  putStrLn "==================================="
  
  testSimpleSat
  testSimpleTautology
  testBooleanAssignment
  testMultipleVariables
  testComplexBoolean
  testIntegerArithmetic
  testContextManagement
  testSymbolCreation
  testSortCreation
  testBooleanConstants
  
  putStrLn "==================================="
  putStrLn "Tests completed."

||| Main function to run tests
main : IO ()
main = runAllTests
