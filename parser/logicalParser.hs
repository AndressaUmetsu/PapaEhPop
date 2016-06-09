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

avaliarExpr e = parse expr "Erro:" e

ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2


 -- E -> FE'
expr = do v1 <- term
		  e <- expr'
		  return (ret v1 e )

o = do v1 <- a -- O -> AO'
	   e <- o'
	   return (ret v1 e)

-- Or
o' = do {string "||"; -- O' -> ||AO'
		 v1 <- a;
		 e <- o';
		 return (Just ((Or), ret v1 e))}
	 <|> return Nothing -- O' -> Vazio

-- And
a = do v1 <- n-- A -> NA'
	   e <- a'
	   return (ret v1 e)

a' = do {string "&&"; -- A' -> &&NA'
		 v1 <- n;
		 e <- a';
		 return (Just ((And), ret v1 e))}
	 <|> return Nothing -- A' -> Vazio


n = do { char '!'; --N -> not N
		 v1 <- n;
		 e <- -- ??
		 return (Just((Not), ret v1 e))}
	<|>	 return	fator  -- N -> F

fator = bool												-- F -> c
		<|> do { char '('; e <- expr; char ')'; return e } 	-- F -> (E)






--avaliarTab (Not e1) = not (avali
--avaliarTab (And e1 e2) = avaliarTab e1 && avaliarTab e2
--avaliarTab (Var v) = consultarValor tab v

--tab = [("x1",True),("x2",False)]
