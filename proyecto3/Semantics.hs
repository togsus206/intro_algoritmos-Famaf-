module Language.Semantics where

import Language.Syntax
import Language.ListAssoc

-- Asignación de valores para las variables enteras
type StateI = ListAssoc VarName Int
-- Asignación de valores para las variables booleanas
type StateB = ListAssoc VarName Bool

defaultIntValue :: Int
defaultIntValue = 0

defaultBoolValue :: Bool
defaultBoolValue = True

-- Tipo que representa la continuación de un paso de ejecución.
-- Ésta puede ser: Fata ejecutar una sentencia (ToExec), o ya no hay nada por
-- ejecutar (Finish).
data Continuation = ToExec Statement
                  | Finish


-- El estado consta del valor de las variables enteras y las booleanas
type State = (StateI,StateB)


evalIExpr :: IntExpr -> StateI -> Int
evalIExpr (ConstI i) si = i
evalIExpr (VI nx) si = case la_buscar si nx of
                               Nothing -> error "no existe la variable"
                               Just v -> v
evalIExpr (Neg ax) si = -(evalIExpr ax si)
evalIExpr (Plus ax bx) si = (evalIExpr ax si) + (evalIExpr bx si)
evalIExpr (Prod ax bx) si = (evalIExpr ax si) * (evalIExpr bx si)
evalIExpr (Div ax bx) si = div (evalIExpr ax si) (evalIExpr bx si)
evalIExpr (Mod ax bx) si = mod (evalIExpr ax si) (evalIExpr bx si)

-- Para evaluar las expresiones booleanas
-- necesitamos también el estado de variables enteras
-- porque en Equal y Less tenemos subexpresiones enteras.

evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (ConstB b) s = b
evalBExpr (VB v) (si, sb) = case la_buscar sb v of
                Just e -> e
                Nothing -> defaultBoolValue
evalBExpr (And b1 b2) s = (evalBExpr b1 s) && (evalBExpr b2 s)
evalBExpr (Or b1 b2) s = (evalBExpr b1 s) || (evalBExpr b2 s)
evalBExpr (Not b) s = not (evalBExpr b s)
evalBExpr (Equal i1 i2) (si, sb) = (evalIExpr i1 si) == (evalIExpr i2 si)
evalBExpr (Less i1 i2) (si, sb) = (evalIExpr i1 si) < (evalIExpr i2 si)


-- Evaluar un paso de ejecución en un programa.
evalStep :: Statement -> State -> (State , Continuation)
evalStep Skip (si, sb) = ((si,sb), Finish)
evalStep (AssignB (Var q tb) eb) (si, sb) = ((si, (la_agregar q (evalBExpr eb (si, sb)) sb)), Finish)
evalStep (AssignI (Var q ti) ei) (si, sb) = (((la_agregar q (evalIExpr ei si) si) ,sb), Finish)
evalStep (Seq s1 s2) (si, sb) = case evalStep s1 (si, sb) of
                               (s', Finish) -> (s' , ToExec s2)
                               (s' ,ToExec s1') -> (s',ToExec (Seq s1' s2))
--evalStep (If ((eb,t):xs)) (si, sb) | evalBExpr eb (si, sb) = evalStep t (si,sb)
--                                   | otherwise = (evalStep (If xs) (si,sb))
evalStep (If ((eb,t):xs)) (si, sb) = case evalBExpr eb (si, sb) of
                              True -> ((si,sb), ToExec t)
                              False -> ((si,sb), ToExec (If xs))
--evalStep (Do eb t) (si, sb) | evalBExpr eb (si, sb) = evalStep (Do eb t) (si, sb)
--                            | otherwise = ((si,sb), Finish)
evalStep (Do eb t) (si, sb) = case evalBExpr eb (si, sb) of
                          True -> ((si, sb), ToExec (Seq t (Do eb t)))
                          False -> ((si, sb), Finish)
--evalStep (Do eb t) (si, sb) = case evalBExpr eb (si, sb) of
--                          True -> ((si, sb), ToExec (Do eb t))
--                          False -> ((si, sb), Finish)
