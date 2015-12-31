module Letter.Analyzer where

import Letter.Core

arity :: FunDef -> Maybe Int
arity (UserFun args _) = Just $ length args
arity (BuiltinFun a _) = a

analyzeExp :: Env -> Exp -> [String]
analyzeExp env funCall@(FunCall _ args) = runChecks env funCall funCallChecks ++ (args >>= analyzeExp env)
analyzeExp env (Let n exp)              = analyzeExp env exp
analyzeExp env _                        = []

analyzeFun :: Env -> (String, FunDef) -> [String]
analyzeFun env (n, UserFun args body) = map (\s -> "In function definition for " ++ n ++ ": \n    " ++ s) (analyzeExp env body)

funCallChecks = [ensureArityMatches]

ensureArityMatches :: Env -> Exp -> Maybe String
ensureArityMatches env (FunCall n args) =
    case findFun env n of
        Nothing     -> Just $ "Could not find function named \"" ++ n ++ "\""
        Just funDef -> case arity funDef of
            Nothing -> Nothing
            Just a -> if a == length args
                       then Nothing
                       else Just $ "Function " ++ n ++ " expects " ++ show a ++ " argument(s), but received " ++ show (length args) ++ "."
ensureArityMatches _ _ = Nothing

runChecks :: Env -> Exp -> [Env -> Exp -> Maybe String] -> [String]
runChecks _ _ [] = []
runChecks env exp (f:fs) =
    case f env exp of
        Nothing -> runChecks env exp fs
        Just error -> error:runChecks env exp fs
