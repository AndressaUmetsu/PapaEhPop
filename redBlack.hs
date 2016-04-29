data Cor = V|P deriving Show
data Arvore a = No a Cor ( Arvore a ) ( Arvore a ) | Folha deriving Show

ins' e Folha = No e V Folha Folha
ins' e a@( No e1 c esq dir )
	| e < e1 = rot ( No e1 c ( ins' e esq ) dir )
	| e > e1 = rot ( No e1 c esq ( ins' e esq ) dir )
	| e == e1 = a

ins e a = No e1 P esq dir
	where ( No e1 a esq dir ) = ins' e a 

rot ( No x3 P ( No x1 V a ( No x2 V b c ) ) d ) = No x2 V ( No x1 P a b ) ( No x3 P c d )
rot ( No x3 P ( No x1 V ( No x2 V a b ) c ) d ) = No x2 V ( No x1 P a b ) ( No x3 P c d )
rot ( No x3 P a ( b No x1 V ( No x2 V c d ) ) ) = No x2 V ( No x1 P a b ) ( No x3 P c d )	
rot ( No x3 P a ( No x1 V ( No x2 b c ) d ) = No x2 V ( No x1 P a b ) ( No x3 P c d )
rot a = a