module Z3.Types

%default total

-- Core Z3 object types
public export
Z3_config : Type
Z3_config = AnyPtr

public export
Z3_context : Type
Z3_context = AnyPtr

public export
Z3_symbol : Type
Z3_symbol = AnyPtr

public export
Z3_ast : Type
Z3_ast = AnyPtr

public export
Z3_sort : Type
Z3_sort = AnyPtr

public export
Z3_func_decl : Type
Z3_func_decl = AnyPtr

public export
Z3_app : Type
Z3_app = AnyPtr

public export
Z3_pattern : Type
Z3_pattern = AnyPtr

public export
Z3_constructor : Type
Z3_constructor = AnyPtr

public export
Z3_params : Type
Z3_params = AnyPtr

public export
Z3_model : Type
Z3_model = AnyPtr

public export
Z3_func_interp : Type
Z3_func_interp = AnyPtr

public export
Z3_solver : Type
Z3_solver = AnyPtr

public export
Z3_optimize : Type
Z3_optimize = AnyPtr

public export
Z3_tactic : Type
Z3_tactic = AnyPtr

||| Z3 context wrapper
public export
record Context where
  constructor MkContext
  ctx : Z3_context

||| Z3 result codes
public export
data CheckResult = Sat | Unsat | Unknown

||| Z3 exception
public export
Z3Exception : Type
Z3Exception = String

||| Monad for Z3 operations
public export
record Z3M a where
  constructor MkZ3M
  runZ3M : Context -> IO (Either Z3Exception a)

public export
Functor Z3M where
  map f (MkZ3M g) = MkZ3M $ \ctx => do
    res <- g ctx
    pure $ map f res

public export
Applicative Z3M where
  pure x = MkZ3M $ \_ => pure (Right x)
  (MkZ3M f) <*> (MkZ3M x) = MkZ3M $ \ctx => do
    f' <- f ctx
    case f' of
      Left err => pure (Left err)
      Right f'' => do
        x' <- x ctx
        pure $ f'' <$> x'

public export
Monad Z3M where
  (MkZ3M f) >>= g = MkZ3M $ \ctx => do
    res <- f ctx
    case res of
      Left err => pure (Left err)
      Right x  => runZ3M (g x) ctx

||| Lift an IO action into Z3M
public export
liftIO : IO a -> Z3M a
liftIO act = MkZ3M $ \_ => map Right act

%foreign "C:Z3_mk_config,libz3,z3.h"
prim__z3_mk_config : PrimIO Z3_config

%foreign "C:Z3_del_config,libz3,z3.h"
prim__z3_del_config : Z3_config -> PrimIO ()

%foreign "C:Z3_mk_context,libz3,z3.h"
prim__z3_mk_context : Z3_config -> PrimIO Z3_context

%foreign "C:Z3_del_context,libz3,z3.h"
prim__z3_del_context : Z3_context -> PrimIO ()

||| Run a Z3 computation with a new context
public export
runZ3 : Z3M a -> IO (Either String a)
runZ3 comp = do
  cfg <- primIO $ prim__z3_mk_config
  ctx <- primIO $ prim__z3_mk_context cfg
  primIO $ prim__z3_del_config cfg
  res <- runZ3M comp (MkContext ctx)
  primIO $ prim__z3_del_context ctx
  pure res
