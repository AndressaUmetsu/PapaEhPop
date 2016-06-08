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

-- O -> AO'    
o = do v1 <- a    											  
	   e <- o'
	   return (ret v1 e)

-- Or
-- O' -> ||AO'     
o' = do {char 'Or';
		 v1 <- a;
		 e <- o';
		 return (Just ((Or), ret v1 e))}	
	 <|>
	 do {char 



	}													

-- And 
-- A -> 
--a = 		  


-- N -> not N
 --n = do { char '~';     									
		  --e <- n; 
		    --
	--
	--}
															-- N -> F	
 
fator = bool												-- F -> c
		<|> do { char '('; e <- expr; char ')'; return e } 	-- F -> (E)






--avaliarTab (Not e1) = not (avali
--avaliarTab (And e1 e2) = avaliarTab e1 && avaliarTab e2
--avaliarTab (Var v) = consultarValor tab v

--tab = [("x1",True),("x2",False)]
