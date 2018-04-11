--TP Funcional 2018 - Microprocesador - Entrega
--Punto 1

data Micro = Micro {
	memoria :: [Int],
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	mensajeError :: String	
} deriving (Show)

xt8088 = Micro [] 0 0 0 ""
fp20 = Micro [] 7 24 0 ""
at8086 = Micro [1..20] 0 0 0 ""

--Punto 2

{- Cree la funcion aumentarProgramCounter, porque en cada instruccion se aumenta el programCounter
   y de esta forma no se repetia el codigo, aunque nose si estara bien.
-}
aumentarProgramCounter micro = micro{programCounter=programCounter micro + 1}
nop micro = aumentarProgramCounter micro
--2.2 El programa en consola seria: (nop.nop.nop) xt8088
--Interviene el concepto de Composicion

--Punto 3

lodv valor micro = aumentarProgramCounter micro{acumuladorA = valor}
swap micro = aumentarProgramCounter micro{acumuladorA = acumuladorB micro}{acumuladorB = acumuladorA micro}
add micro = aumentarProgramCounter micro{acumuladorA = acumuladorB micro + acumuladorA micro}{acumuladorB = 0}
--3.2 El programa seria: (add.(lodv 22).swap.(lodv 10)) fp20

--Punto 4
{-
1. Modelar la instrucción DIV , STR y LOD.
2. Desde la consola, modele un programa que intente dividir 2 por 0.
-}

{-
divide micro | acumuladorB micro==0 = aumentarProgramCounter micro{mensajeError = "DIVISION BY ZERO"}
		  | otherwise = aumentarProgramCounter micro{acumuladorA = acumuladorB micro / acumuladorA micro}{acumuladorB = 0}
-}
str addr val micro = micro{memoria = take (addr - 1) (memoria micro) ++ [val] ++ drop addr (memoria micro)}
