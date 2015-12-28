module Letter.Optimizer where

import Letter.Core

optimizeExp :: Exp -> Exp
optimizeExp fun@(FunCall n args@(NExp n1:NExp n2:_)) =
    case transform n of
        Nothing -> optimizeFunCall fun
        Just f  -> NExp $ f n1 n2
optimizeExp fun@(FunCall n args)
    | optimizeFunCall fun == fun = fun
    | otherwise                  = optimizeExp $ optimizeFunCall fun
optimizeExp e = e

optimizeFunCall (FunCall n args) = FunCall n (map optimizeExp args)

optimizeFun :: FunDef -> FunDef
optimizeFun (UserFun args body) = UserFun args (optimizeExp body)
optimizeFun f = f

transform "+" = Just (+)
transform "-" = Just (+)
transform "/" = Just div
transform "*" = Just (*)
transform "mod" = Just mod
transform "min" = Just min
transform "max" = Just max
transform _   = Nothing
