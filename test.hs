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
maxAbsoluto:: Int -> Int 
maxAbsoluto x y | abs x > abs y   = x
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
algunoEsCero :: (Int, Int) -> Bool
algunoEsCero x y | x==0 || y==0   = True
                | otherwise = False


--Sin Pattern Maching
algunoEsCero' :: (Int, Int) -> Bool
algunoEsCero' (x, y) =  x==0 || y==0

--2e 
{--problema ambosSonCero (x: Q, y:Q) : Boolean{
 requiere: true
 asegura: resultado sera true si ambos numeros son iguales a cero
} --}

-- Con Pattern Maching
ambosSonCero :: (Int, Int) -> Bool
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

enMismoIntervalo :: (Int,Int) -> Bool
enMismoIntervalo (x,y) | (x <=3) && (y <=3) = True
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
esMultiploDe:: (Int,Int) -> Bool
esMultiploDe (x,y) | y == 0  = False
                   | mod x y == 0  = True
                   |otherwise= False




--2i
{-- problema digitoUnidades (a:Z):Z {
requiere: {true}
asegura: dado un numero mayor o igual a 10 resultado es el ultimo digito
asegura: en otro caso sera el mismo numero
}--}
digitoUnidades :: Int -> Int
digitoUnidades n = mod(abs n ) 10


--2j 
{--problema digitoDecenas (x: Z) : Z {
requiere: {True}
asegura: { result es el d´ıgito de x correspondiente a las decenas}
} --}

digitoDecenas :: Int -> Int

digitoDecenas n =  digitoUnidades(div (abs n)  10)


--3 



