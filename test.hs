doubleMe :: Int -> Int

doubleMe x = x + x
--1a
f:: Int -> Int

f n       | n==1 = 8    
          | n==4 = 131
          | n==16 = 16

--1b
g::Int -> Int

g 8= 16
g 6 = 4
g 131 = 1
--1c
h:: Int-> Int
h  = f.g 

k:: Int-> Int
k = g.f
--2a
absoluto:: Int -> Int
absoluto x = abs x

--2b 
maxAbsoluto:: Int-> Int -> Int 
maxAbsoluto x y     | abs x > abs y   = x
                    | otherwise = y




--2c
{-- problema maximo(a:Z,b:Z,c:Z):Z{
requiere: true
asegura: resultado es a si a es mayor o igual a b y a es mayor o igual a c
asegura: resultado es b si b es mayor o igual a a y a es mayor o igual a c
asegura: resultado es c si c es mayor o igual a a y a es mayor o igual a b


--}
--}       
maximo :: Int-> Int -> Int -> Int  

maximo a b c | a>=b &&  a>= c = a
             | b>=a &&  b>= c = b
             | otherwise = c
                  



--2d
{-- problema algunoEsCero (x:Q, y:Q) : Boolean {
 requiere: true
 asegura: resultado sera true si alguno de ambos numeros es igual a cero
}
--}

-- Con Pattern Maching
algunoEsCero :: Int -> Int -> Bool
algunoEsCero x y | x==0 || y==0   = True
                | otherwise = False


--Sin Pattern Maching
algunoEsCero' ::  Int -> Int -> Bool
algunoEsCero' x y =  x==0 || y==0



--2e 
{--problema ambosSonCero (x: Q, y:Q) : Boolean{
 requiere: true
 asegura: resultado sera true si ambos numeros son iguales a cero
} --}

-- Con Pattern Maching
ambosSonCero :: Int -> Int -> Bool
ambosSonCero x y | x==0 && y==0   = True
                 | otherwise = False

--Sin Pattern Maching 
ambosSoncero1 :: (Int, Int) -> Bool
ambosSoncero1(0,0) = True
ambosSoncero1 (_ , _) = False


--2f 
{-- problema enMismoIntervalo (x: R, y:R ) : Boolean {
requiere: true
asegura: si ambos numeros pertenecen al mismo  de los siguientes
intervalos dara true, en caso contrario sera false.
Los intervalos son : [-∞,3] [3,7] [7,∞]
}

 --}
enMismoIntervalo :: Int -> Int -> Bool
enMismoIntervalo x y   | (x <=3) && (y <=3) = True
                       | (x >3 && x<7 ) && (y > 3 && y < 7) = True
                       | (x >7) && (y >7) = True
                       | otherwise = False

--2g
{-- problema SumaDistintos(a:Z,b :Z, c:Z) :Z {
  requiere : true
  asegura: {si los 3 par´ametros son distintos entonces res = x + y + z}
  asegura: {si 2 par´ametros son iguales, res es igual al no repetido}
  asegura: {si los 3 par´ametros son iguales, res = 0}
 } --}

sumaDistintos :: Int-> Int -> Int -> Int  
sumaDistintos a b c | a /= b &&  a/= c && b/=c  = a+b+c
                    | a==b && c/= b  = c
                    | b==c && a /= b  = a
                    | a==b && b==c = 0


--2h
{--problema esMultiploDe (x:R, y:R): Boolean 
requiere: true
asegura: {si x es multiplo de  o viceversa, resultado sera true, de lo contrario sera False}
--}
esMultiploDe::  Int -> Int -> Bool
esMultiploDe  x y  | y == 0  = False
                   | mod x y == 0  = True
                   | otherwise= False




--2j 
{--problema digitoDecenas (x: Z) : Z {
requiere: {True}
asegura: { result es el d´ıgito de x correspondiente a las decenas}
} --}

digitoDecenas :: Int -> Int

digitoDecenas n =  digitoUnidades(div (abs n)  10)




--2i
{-- problema digitoUnidades (a:Z):Z {
requiere {true}
asegura: dado un numero mayor o igual a 10 resultado es el ultimo digito
asegura: en otro caso sera el mismo numero
}--}
digitoUnidades :: Int -> Int
digitoUnidades n = mod(abs n ) 10

--3 








--4a
{-- problema productoInterno(x:(ZXZ), y: (ZXZ)):Z
{
requiere:true
asegura: resultado sera el resultado de hacer la suma de los productos de la misma posicion
}
--}
productoInterno:: (Int,Int) -> (Int,Int) -> Int
productoInterno (a,b) (c,d) = (a*c) + (b*d)







--4b 
{-- problema todoMenor (x:(RXR),y:(RXR)): Bool
 {
 requiere : dos tuplas de numeros enteros de la misma cantidad de elementos

 asegura: resultado sera true si cada coordenada de la primera tupla es menor a la misma coordenada de la otra tupla
 }
 --}

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x,y) (z,q) 
                      | (z > x) && (q>y) = True
                      | otherwise = False


type Punto2D = (Float, Float)
todoMenor1:: Punto2D -> Punto2D -> Bool
todoMenor1 punto1 punto2 = (fst punto1 < (fst punto2)) && (snd punto1 < (snd punto2))

--4c
{--problema distanciaEuclideana (x:(ZXZ) y:(ZXZ) ): Z 
{
requiere: true
asegura: resultado sera el resultado de hacer la distancia euclideana entre las dos tuplas

}
--}
distanciaEuclideana::Punto2D -> Punto2D -> Float
distanciaEuclideana (a,b) (c,d) = sqrt (((b-a)**2) + ((d-c)**2))





--4d 
{-- problema sumaTerna (x:(ZXZXZ)):Z{
requiere: true
asegura: resultado sera la suma de los tres elementos de la tupla

} --}
sumaTerna:: (Int,Int,Int) -> Int
sumaTerna (x,y,z) = x+y+z

--4e
{-- problema sumarSoloMultiplos (x:(ZXZXZ), y: N ) : Z
{
requiere : true
asegura: resultado sera la suma de los elementos de la tupla cuales sean multiplos del numero natural
}

--}
sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (x,y,z) n | (mod x n == 0) &&  (mod y n == 0)  && (mod z n == 0) = x+y+z
                             | (mod x n /=0) && (mod y  n== 0)  && (mod z n == 0) = y+z
                             | (mod x n ==0) && (mod y  n/= 0)  && (mod z n == 0) = x+z
                             | (mod x n ==0) && (mod y  n== 0)  && (mod z n /= 0) = x+y
                             | (mod x n ==0) && (mod y  n/= 0)  && (mod z n /= 0) = x
                            | (mod x n /=0) && (mod y  n== 0)  && (mod z n /= 0) = y
                            | (mod x n /=0) && (mod y  n/= 0)  && (mod z n == 0) = z
                         





--4f
{-- problema posPrimerPar(x:(ZXZXZ)): z posPrimerPar 
requiere: true
asegura: resultado sera 4 si todos los numeros de la terna son impares
asegura: resultado sera la posicion del primer numero par encontrado, en el caso que existiese y usaremos del 1 al 3


--}
type Terna = (Int,Int,Int) 


esPar :: Int -> Bool
esPar numero = mod numero 2 == 0

posPrimerPar :: Terna-> Int
posPrimerPar (x,y,z) | esPar x = 1
                     | esPar y = 2
                     | esPar z = 3
                     | otherwise = 4



--4g
{--problemacrearPar (x,y):(x,y){
requiere : true
asegura: resultado sera una tupla con los dos elementos ingresados
} --}


--4h
{--problemaInvertirPar (x,y) : (y,x){
requiere: true
asegura: resultado sera la tupla de los dos elementos ingresados pero con el orden invertido
} --}


invertirPar:: a->   b -> (b,a) 
invertirPar x y = (y,x)





--5 

--problema f (n : Z) : Z {
--requiere: {True}
--asegura: {(n ≤ 7 → res = n**2) ∧ (n > 7 → res = 2n − 1)}
--}

funcion:: Int -> Int
funcion x | x <= 7 =  x^2
          | x>7 = 2*x -1

--problema g (n : Z) : Z {
--requiere: {True}
--asegura: {Si n es un numero par entonces res = n/2, en caso contrario, res = 3n + 1  }

guncion :: Int -> Int
guncion x  | mod x 2 == 0 =  div x 2
           | otherwise = 3*x + 1


--problema todosMenores (t : Z × Z × Z) : Bool {
--requiere: {T rue}
--asegura: {(res = true) ↔ ((f (t0) > g(t0)) ∧ (f (t1) > g(t1)) ∧ (f (t2) > g(t2)))}
--}
type TripleternaInt = (Int,Int,Int)
todosMenores :: TripleternaInt -> Bool
todosMenores (a,b,c) = funcion a > guncion a && funcion b > guncion b && funcion c > guncion c





--6 
type Anio = Int
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto x | mod x 4 == 0 ||   mod x 100 == 0 ||  mod x 400 == 0 = True
           | otherwise = False



--7

--distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
--problema distanciaManhattan (p : R × R × R, q : R × R × R) : R {
--requiere: {T rue}
--asegura: {res = P2
--i=0 |pi − qi|}
--}
--Por ejemplo:
--distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
--distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8 ---
type Tripleterna = (Float,Float,Float) 

distanciaManhattan::Tripleterna -> Tripleterna -> Float

distanciaManhattan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)



--8
sumarUltimoDosDigitos:: Int -> Int
sumarUltimoDosDigitos x = mod (abs x ) 10 + mod (div (abs x) 10 ) 10

comparar :: Int -> Int -> Int
comparar a b   | sumarUltimoDosDigitos a > sumarUltimoDosDigitos b  = -1
                | sumarUltimoDosDigitos b > sumarUltimoDosDigitos a = 1
                | otherwise = 0 




--9a
f1 :: Float-> Float
 f1 n | n == 0 = 1
 | otherwise = 0

 --esta funcion hace que si el numero ingresado es igual a cero el resultado sera uno, en caso contrario sera 0


 --9b
 f2 :: Float-> Float
 f2 n | n == 1 = 15
 | n ==-1 =-15
--en este caso si es 1 el resultado es 15, si es -1 el resultado es -15


--9c

f3 :: Float-> Float
 f3 n | n <= 9 = 7
 | n >= 3 = 5
-- en este caso si el numero ingresado es menor o igual a 9 da 7, pero si es mayor a igual a 3 da 5. en los casos 9,8,6,5,4 dara 7

 --9d
 f4 :: Float-> Float-> Float
 f4 x y = (x+y)/2

--este algoritmo hace un promedio entre ambos numeros ingresados


--9e
f5 :: (Float, Float)-> Float
 f5 (x, y) = (x+y)/2
 
--este algoritmo hace lo mismo que el algoritmo anterior, pero este se ingresa dos valores por medio de una tupla.

--9f
f6 :: Float-> Int-> Bool
 f6 a b = truncate a == b


 --este algoritmo devuelve verdadero si el valor entero mas cercano a  es igual a b, en otro caso devuelve false.

 
