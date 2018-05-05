module TP where

import Text.Show.Functions

--Primera Entrega 
--  Punto 1: Modelar micro
data Micro = Micro{
memoria :: [Int],
acumuladorA :: Int,
acumuladorB :: Int,
programCounter :: Int,
mensajeError :: String,
programas :: [Instruccion]
}deriving (Show)
type Instruccion = Micro -> Micro

-- Punto 2: Se probo haciendo (nop.nop.nop)xt8088, se uso composicion
xt8088 = Micro {memoria = (replicate 1024 0),acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programas=[divide,(lod 1),swap,(lod 2),(str 2 0),(str 1 2)]} 
fp20   = Micro {memoria = [],acumuladorA = 7,acumuladorB = 24,programCounter = 0,mensajeError = "",programas=[]}
at8086 = Micro {memoria = [1..20],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programas=[]}
matias = Micro {memoria = [2,5,1,0,10],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programas=[]}

nop :: Micro -> Micro
nop micro = micro{programCounter = programCounter micro + 1}
 
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

--Segunda Entrega

--Punto 1: Carga de un programa
cargarPrograma :: Instruccion -> Micro -> Micro
cargarPrograma unPrograma micro = micro{programas = programas micro ++ [unPrograma]}

--Punto 2: Ejecución de un programa
ejecutar :: Micro -> Micro
ejecutar micro = foldr ($) micro (programas micro)

--Punto 3: IFNZ
ifnz :: [Instruccion] -> Micro -> Micro
ifnz listaInstrucciones micro
	| (acumuladorA micro /= 0) = foldr ($) micro listaInstrucciones
	| otherwise = micro

--Punto 4: Depuración de un programa
depurar :: [Instruccion] -> Micro -> [Instruccion]
depurar [] micro = []
depurar (x:xs) micro 
	| instruccionNecesaria x micro = [x] ++ (depurar xs micro)
	| otherwise = depurar xs micro
instruccionNecesaria unaInstruccion unMicro = (acumuladorA (unaInstruccion unMicro) /= 0) || (acumuladorB (unaInstruccion unMicro) /= 0) 

--Ni idea si andaa jaja

--Punto 5: Memoria ordenada
tieneMemoriaOrdenada micro = memoriaOrdenada (memoria micro)

memoriaOrdenada [x,y] = x <= y
memoriaOrdenada (x:y:xs) 
	| x <= y = memoriaOrdenada (y:xs)
	| otherwise = False

--Punto 6: Memoria infinita
--xt8088 = Micro {memoria = [0,0..],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programas=[divide,(lod 1),swap,(lod 2),(str 2 0),(str 1 2)]} 
