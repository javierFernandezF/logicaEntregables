{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea2 where

---------------------------------
--Nombre 1	: Javier Fernandez
--Nro. 1	: 172271
---------------------------------
--Nombre 2 	: Facundo Duarte
--Nro. 2 	: 256083
---------------------------------

type N = Integer

--------------
--PROBLEMA 1--
--------------

suma_entre :: N -> N -> N
suma_entre m n
    | m > n = 0
	| otherwise = m + (suma_entre (m+1) n)

--1.
--suma_entre 2 5 = 14

--2.
{--
Es bien fundada porque en cada llamada recursiva la diferencia 
entre m y n decrece hasta llegar a 0 y en ese caso entra al caso base.

El tamaño del problema que decrece es la diferencia entre m y n. En cada llamada 
recursiva se resta 1 a n hasta llegar a 0.
--}

--3.
suma_entre' :: N -> N -> N
suma_entre' m n 
	| m > n = 0
	| otherwise = n + suma_entre' m (n-1)

--4.
suma_entre_f :: (N -> N) -> N -> N -> N
suma_entre_f f m n  
	| m > n = 0
	| otherwise = f n + suma_entre_f f m (n-1)



--Declaro una funcion que me sirva de ejemplo
funcionEjemplo :: N -> N
funcionEjemplo n = n



--5.
suma_i :: N -> N
suma_i n  = suma_entre_f funcionEjemplo 0 n



{-- 

suma_i 5

1 + suma_entre_f suma_i 0 4
1+ suma_i 4 + suma_entre_f suma_i 0 3


--}

--------------
--PROBLEMA 2--
--------------

--1.
es_divisor :: N -> N -> Bool
es_divisor n k = mod n k == 0

--2.
primer_divisor :: N -> N
primer_divisor n = primerDiv n 2

--Creo un auxiliar para poder dejar en memoria el valor de k
primerDiv :: N -> N -> N
primerDiv n k 
	| es_divisor n k = k
	| otherwise = primerDiv n (k+1)

--3.
es_primo :: N -> Bool
es_primo n = if primer_divisor n == n then True else False

--------------
--PROBLEMA 3--
--------------

minimo_acotado :: (N -> Bool) -> N -> N -> N
minimo_acotado p m n
	| m > n = m
	| m <= n && p m = m
	| m <= n && not (p m) = minimo_acotado p (m+1) n


--Funcion que nos va a ayudar a validar
esPar :: N -> Bool
esPar n = mod n 2 == 0


--1.
{--
Si ningun valor en el intervalo considerado cumple el predicado, entonces:

Si ninguno de los valores del intervalo [m,n] cumple el predicado, pensemos que estamos 
iterando y nos detenemos en el caso m=n, es decir "minimo_acotado p n n" 

primera sentencia n > n es falsa
segunda sentencia n <= n && p n es falsa porque por hipotesis ningun valor del intervalo [m,n] cumple el predicado
tercera sentencia n <= n && not (p n) es verdadera porque por hipotesis ningun valor del intervalo [m,n] cumple el predicado

Entonces la iteración sigue con "minimo_acotado p (n+1) n"

primera sentencia es n+1 > n es verdadera, entonces m=n+1

Se detiene la iteración.


--}

--2.
--primer_divisor' :: N -> N
--primer_divisor' n = minimo_acotado es_divisor n 2 
--VER ESTA PORONGA, HAY QUE REVISAR ESTO Y PARA ABAJO.

--3.
maximo_acotado :: (N -> Bool) -> N -> N -> N
maximo_acotado p m n 
	| m > n = m
	| m <= n && p n = n
	| m <= n && not (p n) = maximo_acotado p m (n-1)

--4.
minimo_p :: (N -> Bool) -> N -> N
minimo_p p n
	| p n = n
	| not (p n) = minimo_p p (n+1)

{--
Esta función termina si p n es verdadero, es decir, si el valor n cumple el predicado. Caso contrario, escala hasta el infinito.
--}

--------------
--PROBLEMA 4--
--------------

--1.
cantidad_p :: (N -> Bool) -> N -> N -> N
cantidad_p p m n = 
			| m > n = 0
			| p m = 1 + cantidad_p p (m+1) n
			| otherwise = cantidad_p p (m+1) n

--2.
suma_p :: (N -> Bool) -> N -> N -> N
suma_p p m n = 
			| m > n = 0
			| p m = m + suma_p p (m+1) n
			| otherwise = suma_p p (m+1) n

--3.
suma2_p :: (N -> Bool) -> N -> N -> N
suma2_p p m n = 
			| m > n = 0
			| p m = m*m + suma2_p p (m+1) n
			| otherwise = suma2_p p (m+1) n

--4.
sumaf_p :: (N -> Bool) -> (N -> N) -> N -> N -> N
sumaf_p p f m n = 
			| m > n = 0
			| p m = f m + sumaf_p p f (m+1) n
			| otherwise = sumaf_p p f (m+1) n

--5.
todos_p :: (N -> Bool) -> N -> N -> Bool
todos_p p m n = 
			| m > n = True
			| not (p m) = False
			| otherwise = todos_p p (m+1) n

--6.
existe_p :: (N -> Bool) -> N -> N -> Bool
existe_p p m n = 
			| m > n = False
			| p m = True
			| otherwise = existe_p p (m+1) n

-------
--FIN--
-------