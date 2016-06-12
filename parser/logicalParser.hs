import Text.ParserCombinators.Parsec
import Data.Char

data Expr = Bimp Expr Expr | Imp Expr Expr | Or Expr Expr | And Expr Expr | Not Expr | Var String deriving Show

main = do
    putStr "\nExpressao:"
    e <- getLine
    case avaliarExpr e of
        Left err -> putStr ((show err)++ "\n")
        Right r  -> putStr ((show r) ++ "\n")
    return()


avaliarExpr e = parse expr "Erro:" e

ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2

expr = do
	v1 <- imp  -- E -> IB'
	e <- bimp'
	return (ret v1 e )

bimp' = do 
	{
		string "<->"; -- B' -> <->IB'
		v1 <- imp;
		e <- bimp';
		return (Just (Bimp, ret v1 e))
	}
	<|> return Nothing -- B' -> Vazio

imp = do
	v1 <- _or -- I -> AI'
	e <- _or'
	return (ret v1 e )

imp' = do 
	{
		string "->"; -- I' -> ->AI'
		v1 <- _or;
		e <- imp';
		return (Just (Imp, ret v1 e))
	}
	<|> return Nothing -- expr' -> Vazio

_or = do
	v1 <- _and -- O -> AO'
	e <- _or'
	return (ret v1 e)

_or' = do 
	{
		string "||"; -- O' -> ||AO'
		v1 <- _and;
		e <- _or';
		return (Just (Or, ret v1 e))
	}
	<|> return Nothing -- O' -> Vazio

_and = do
	v1 <- _not-- A -> NA'
	e <- _and'
	return (ret v1 e)

_and' = do 
	{
		string "&&"; -- A' -> &&NA'
		v1 <- _not;
		e <- _and';
		return (Just (And, ret v1 e))
	}
	<|> return Nothing -- A' -> Vazio

--Not
--_not = do 
--	{ 
--		 char '!'; --N -> _not N
--		e <- _not -- ??
--		return (Just((Not), ret  e))}
--	<|>	return	fator  -- N -> F

_not = return fator

fator = do 
	{ 
		char '('; e <- expr; char ')'; return e  -- F -> (E)
	} 	
	<|> bool -- F -> c												

bool = do
	literal <- many1 letter
	return literal



--avaliarTab (Not e1) = not (avali
--avaliarTab (_And e1 e2) = avaliarTab e1 && avaliarTab e2
--avaliarTab (Var v) = consultarValor tab v

--tab = [("x1",True),("x2",False)]
