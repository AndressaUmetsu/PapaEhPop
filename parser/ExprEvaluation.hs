module ExprEvaluation where

import Data.Char

data Expr = Bimp Expr Expr | Imp Expr Expr | Or Expr Expr | And Expr Expr | Not Expr | Var String deriving Show

evaluateTab e tab = evaluateTab' e
    where
        evaluateTab' (Not e1) = not (evaluateTab' e1)

        evaluateTab' (And e1 e2) = evaluateTab' e1 && evaluateTab' e2

        evaluateTab' (Or e1 e2) = evaluateTab' e1 || evaluateTab' e2

        evaluateTab' (Imp e1 e2) = not (evaluateTab' e1) || evaluateTab' e2

        evaluateTab' (Bimp e1 e2) = evaluateTab' (Imp e1 e2) && evaluateTab' (Imp e2 e1)

        evaluateTab' (Var v) = checkValue tab v

checkValue (entry:tab) v
    | fst entry == v = snd entry
    | otherwise = checkValue tab v

removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/=x) xs)

countVariables expr = removeDups (countVariables' expr)

countVariables' (Not e1) = countVariables' e1

countVariables' (And e1 e2)  = countVariables' e1 ++ countVariables' e2

countVariables' (Or e1 e2)   = countVariables' e1 ++ countVariables' e2

countVariables' (Imp e1 e2)  = countVariables' e1 ++ countVariables' e2

countVariables' (Bimp e1 e2) = countVariables' e1 ++ countVariables' e2

countVariables' (Var v) = [v]

generateTab' [] _ = []
generateTab' (v:varList) (b:bitString)
    | b == '1' =  (v,True)  : generateTab' varList bitString
    | otherwise = (v,False) : generateTab' varList bitString

int2bin x
    | x `div` 2 == 0 = show x
    | otherwise = int2bin (x `div` 2) ++ show (x `mod` 2) 

fill n list_ = reverse $ take n ((reverse list_) ++ repeat '0')

forceEvaluation' e n it varList
    | it == 2 ^ n = Nothing
    | evaluateTab e tab == True = Just tab
    | otherwise = forceEvaluation' e n (it + 1) varList
    where
        bitString = fill n (int2bin it)
        tab = generateTab' varList bitString
        

forceEvaluation e = forceEvaluation' e n 0 varList
    where
        varList = countVariables e
        n = length varList
