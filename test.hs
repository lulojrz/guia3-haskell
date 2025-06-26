module GUIA where
--GUIA 3
--ejercicio 1
f::Integer ->Integer
f n | n==1 = 8
    | n==4 = 131
    | n==16 = 16

g::Integer->Integer
g n |n==8 = 16
    |n==16 = 4
    |n==131 =1

h::Integer->Integer
h = f.g
k::Integer->Integer
k = g.f

--ejercicio
redondear::Float->Integer
redondear n | n<0 = redondear(-n)
            | n<1 = 0
            | otherwise = 1 + redondear (n-1)
      
--ejercicio 2a
absoluto::(Num t, Ord t)=> t ->t
absoluto n | n<0 = -n
           | otherwise = n

--ejercicio 2b
maxAbsoluto::Int->Int->Int
maxAbsoluto x y | absoluto x <absoluto y = absoluto y
                | absoluto y <absoluto x = absoluto x
                | otherwise = x

--ejercicio 2c
maximo3::Int->Int->Int->Int
maximo3 x y z | x>y && x > z =x
              | y>x && y > z =y
              | otherwise = z

--ejercicio 2d
algunoesCero::Int->Int->Bool
algunoesCero x y = (x==0) || (y==0)

--ejercicio 2e
ambosCero::Int->Int->Bool
ambosCero x y = (x==0)&&(y==0)

--ejercicio 2f
mismoIntervalo::Int->Int->Bool
mismoIntervalo x y | x <3 && y<3 = True
                   | x>3 && x <7 && y>3 && y <7 = True
                   | x>7 && y>7=True
                   | otherwise = False

--ejercicio 2g
sumaDistintos::(Eq t , Num t)=>t->t->t->t
sumaDistintos x y z | x/=y && y/=z && x/=z  =x+y+z
                    | otherwise = 0

--ejercicio 2h
esMultiplode::Int->Int->Bool
esMultiplode x y = mod x y == 0

--ejercicio 2i
digitoUnidades::Int->Int
digitoUnidades x = mod x 10

--ejercicio 2j 
digitoDecenas::Int->Int
digitoDecenas x = digitoUnidades (div x 10)


--ejercicio 3

--ejercicio 4a
productoInterno:: (Int,Int)->(Int,Int)->Int
productoInterno (x,y) (z,u) = x*z + y*u

--ejercicio 4b
esParMenor::(Int,Int)->(Int,Int)->Bool
esParMenor (x,y) (z,u) = x<z && y<u 

--ejercicio 4d
sumaTerna::(Int,Int,Int)-> Int
sumaTerna (x,y,z) = x+y+z

--ejercicio 4e
sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (x, y, z) n = sumar x (sumar y (sumar z 0))
  where
    sumar a b | mod a n == 0 = a + b
              | otherwise    = b

--ejercicio 4f
posPrimerPar:: [Int] -> Int
posPrimerPar lista = buscar lista 0
    where 
    buscar [] _ = 4
    buscar (y:ys) i | mod y 2 == 0 = i
                    |otherwise = buscar ys (i+1)


--ejercicio 4g
crearPar::a->b->(a,b)
crearPar x y = (x,y)

--ejercicio 4h
invertir:: (a,b)->(b,a)
invertir (x,y) = (y,x)


--ejercicio 5
todosMenores::(Integer,Integer,Integer)-> Bool
todosMenores (x,y,z) = funcion x > guncion x && funcion y > guncion y && funcion z > guncion z



funcion:: Integer-> Integer
funcion n | n<=7 = n*n
          | otherwise = 2*n -1

guncion::Integer-> Integer
guncion n  | mod n 2 == 0 = div n 2
           | otherwise = 3*n +1



--ejercicio 6
bisiesto ::Integer -> Bool
bisiesto n = (mod n 4 == 0) && ((mod n 100 /= 0) || (mod n 400 == 0))

--ejercicio 7
distanciaManhattan:: (Float, Float, Float)-> (Float, Float, Float)-> Float
distanciaManhattan (a,b,c) (d,e,f) = absoluto(a-d) + absoluto(b-e) + absoluto(c-f)

--ejercicio 8
comparar::Integer-> Integer -> Integer
comparar x y  | sumaUltimaDosDigitos x < sumaUltimaDosDigitos y = 1
              | sumaUltimaDosDigitos y < sumaUltimaDosDigitos x = -1
              | sumaUltimaDosDigitos x == sumaUltimaDosDigitos y = 0

sumaUltimaDosDigitos::Integer->Integer
sumaUltimaDosDigitos n = (mod n 10) + (mod (div n 10) 10 )

 
