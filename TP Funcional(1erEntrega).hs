module TP where

import Text.Show.Functions

-- Modelado de Datos 
-- Punto 1
data Micro = Micro{
	memoria :: [Int],
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	mensajeError :: String
}deriving (Show)

-- Punto 2: Se probo haciendo (nop.nop.nop)xt8088, se uso composicion
xt8088 = Micro {memoria = (replicate 1024 0),acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = ""} 
fp20   = Micro {memoria = [],acumuladorA = 7,acumuladorB = 24,programCounter = 0,mensajeError = ""}
at8086 = Micro {memoria = [1..20],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = ""}

nop :: Micro -> Micro
nop micro = micro{programCounter = (programCounter micro) + 1}
 
--Punto 3: Se probo haciendo (add.(lodv 22).swap.(lodv 10))fp20 se uso composicion
lodv :: Int -> Micro -> Micro
lodv val micro = nop micro{acumuladorA = val} 

swap :: Micro -> Micro 
swap micro = nop micro{acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro}

add :: Micro -> Micro
add micro = nop micro{acumuladorA = acumuladorA micro + acumuladorB micro, acumuladorB = 0}

--Punto 4  Se probo haciendo (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088 

divide :: Micro -> Micro
divide micro  
	| acumuladorB micro == 0 = nop micro{mensajeError = "Division By Zero"}
	| otherwise = nop micro{acumuladorA = div (acumuladorA micro)(acumuladorB micro), acumuladorB = 0}
		
str :: Int -> Int -> Micro -> Micro
str addr val micro = nop micro{memoria = take (addr - 1) (memoria micro) ++ [val] ++ drop addr(memoria micro)}

lod :: Int -> Micro -> Micro
lod addr micro = nop micro{acumuladorA = (!!)(memoria micro) (addr - 1)}