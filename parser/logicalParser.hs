import Text.ParserCombinators.Parsec
import Data.Char

data Expr = Bimp Expr Expr | Imp Expr Expr | Or Expr Expr | And Expr Expr | Not Expr Expr | Var String deriving Show

main = do
    putStr "\nExpressao:"
    e <- getLine;
    case avaliarExpr e of
        Left err -> putStr ((show err)++ "\n")
        Right r  -> putStr ((show r) ++ "\n")}
    return ()


--avaliarTab (Not e1) = not (avali
--avaliarTab (And e1 e2) = avaliarTab e1 && avaliarTab e2
--avaliarTab (Var v) = consultarValor tab v

--tab = [("x1",True),("x2",False)]
