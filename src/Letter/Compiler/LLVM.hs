-- You must compile this code.  Interpretation will not work:
--
--     $ cat stack.yaml
--     resolver: lts-3.14
--     packages: []
--     extra-deps:
--     - llvm-general-3.5.1.0
--     - llvm-general-pure-3.5.0.0
--
--     $ stack build llvm-general
--
--     $ stack ghc -- -O2 example.hs
--
--     $ ./example
--     ; ModuleID = 'example-llvm-module'
--
--     define i8 @f(i8 %x) {
--     entry:
--       ret i8 %x
--     }

import qualified Control.Monad.Trans.Except         as Except
import qualified Data.Text                          as Text
import qualified LLVM.General                       as LLVM
import qualified LLVM.General.Context               as Context
import qualified LLVM.General.AST                   as AST
import qualified LLVM.General.AST.Linkage           as Linkage
import qualified LLVM.General.AST.Type              as Type
import qualified LLVM.General.AST.Visibility        as Visibility
import qualified LLVM.General.AST.CallingConvention as Convention
import qualified System.Exit                        as Exit
import qualified System.IO                          as IO

astModule :: Env -> [Exp] -> String -> AST.Module
astModule (Env fs gs) exps fn = AST.Module
    { AST.moduleName         = fn
    , AST.moduleDataLayout   = Nothing
    , AST.moduleTargetTriple = Nothing
    , AST.moduleDefinitions  = map toLLVMFun fs
    }

toLLVMFun name (UserFun args exp) = AST.GlobalDefinition (
            LLVM.functionDefaults {
                returnType = Type.i64,
                name = AST.Name name,
                parameters = (map toLLVMParam args, False),
                basicBlocks = [ AST.BasicBlock
                    (AST.Name "entry")
                    []
                    (AST.Do
                        (AST.Ret
                            (Just (toLLVMExp exp))
                            []
                        )
                    )
                ]
            })

toLLVMParam n = AST.Parameter Type.i64 (AST.Name n) []


main :: IO ()
main =
    Context.withContext (\context -> do
        e <- Except.runExceptT
            (LLVM.withModuleFromAST context astModule (\mod -> do
                bs <- LLVM.moduleLLVMAssembly mod
                putStrLn bs ))
        case e of
            Left str -> do
                IO.hPutStrLn IO.stderr str
                Exit.exitFailure
            Right () -> return () )
