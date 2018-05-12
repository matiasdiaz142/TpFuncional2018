module TP where

import Text.Show.Functions

-- Primera Entrega 
-- Punto 1
data Micro = Micro{
	memoria :: [Int],
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	mensajeError :: String,
	programa :: [Instruccion]
}deriving (Show)
type Instruccion = Micro -> Micro

-- Punto 2: Se probo haciendo (nop.nop.nop)xt8088, se uso composicion
xt8088 = Micro {memoria = (replicate 1024 0),acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programa=[swap.divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)]} 
fp20   = Micro {memoria = [],acumuladorA = 7,acumuladorB = 24,programCounter = 0,mensajeError = "",programa=suma10y22}
at8086 = Micro {memoria = [1..20],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programa=[]}

nop :: Micro -> Micro
nop micro 
	|hayError micro = micro
	|otherwise = micro{programCounter = programCounter micro + 1}
 
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
	|acumuladorB micro == 0 = nop micro{mensajeError = "Division By Zero"}
	|otherwise = nop micro{acumuladorA = div (acumuladorA micro)(acumuladorB micro), acumuladorB = 0}
		
str :: Int -> Int -> Micro -> Micro
str addr val micro = nop micro{memoria = take (addr - 1) (memoria micro) ++ [val] ++ drop addr(memoria micro)}

lod :: Int -> Micro -> Micro
lod addr micro = nop micro{acumuladorA = (!!)(memoria micro) (addr - 1)}

--Segunda Entrega

suma10y22 = [add,(lodv 22),swap,(lodv 10)]
division2Por0 = [swap,swap,swap,divide,(lod 1),swap,(lod 2),(str 2 0),(str 1 2)]

--Punto 1: Carga de un programa
cargarPrograma :: [Instruccion] -> Micro -> Micro
cargarPrograma unPrograma micro = micro{programa = programa micro ++ unPrograma}

--Punto 2: Ejecución de un programa
ejecutar :: Micro -> Micro
ejecutar micro = foldr ($) micro (programa micro)
--Hacer una funcion auxiliar para ****foldr ($) micro (programa micro)****
--y usarla tambien en ifnz 

hayError::Micro->Bool
hayError = ((>0).length.mensajeError)

--Punto 3: IFNZ
ifnz :: [Instruccion] -> Micro -> Micro
ifnz listaInstrucciones micro
	| (acumuladorA micro /= 0) = foldr ($) micro listaInstrucciones
	| otherwise = micro

--Punto 4: Depuración de un programa
depurar :: [Instruccion] -> Micro -> [Instruccion]
depurar listaInstrucciones micro = filter (\f -> acumuladoresYMemoriaEnCero (f micro)) listaInstrucciones
--depurar (swap.nop.(lodv 133).(lodv 0).(str 1 3).(str 2 0)) xt8088

acumuladoresYMemoriaEnCero :: Micro -> Bool 
acumuladoresYMemoriaEnCero micro = (acumuladorA micro /= 0) || (acumuladorB micro /= 0) || (sum (memoria micro) /= 0)
--all (/=0) (memoria micro)

--Punto 5: Memoria ordenada
tieneMemoriaOrdenada micro = memoriaOrdenada (memoria micro)

memoriaOrdenada [x,y] = x <= y
memoriaOrdenada (x:y:xs) 
	| x <= y = memoriaOrdenada (y:xs)
	| otherwise = False
--Evaluar el caso de si se tiene un solo elemento o el conjunto vacio

--Punto 6: Memoria infinita
xtInfinito = Micro {memoria = [0,0..],acumuladorA = 0,acumuladorB = 0,programCounter = 0,mensajeError = "",programa=[]}
--La operacion se completo pero se queda cargando infinitamente la memoria en la consola, pero aplicando:
--en consola: (acumuladorA.ejecutar.(cargarPrograma suma10y22)) xtInfinito
--se puede ver el resultado de 32 en el acumuladorA

--No llega a tirar False ni True,ya que, se queda analizando infinitamente a la memoria del Micro, por lo tanto,
--la consola no tira ningun resultado

--Al tener una memoria infinita, si una funcion usa esa memoria dependiendo de si tenga que usar toda la memoria o no
--esta podria quedarse infinitamente "analizando"
