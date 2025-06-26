module Test where

import Test.HUnit
import GUIA

run = runTestTT testsumaultimodosdigitos

testfuncionf= test[
    "caso 1:"~:(f 1)~?=8,
    "caso 2:"~:(f 4)~?=131,
    "caso 3:"~:(f 16)~?=16
 ]

testfunciong= test[
    "caso 1:"~:(g 8)~?=16,
    "caso 2:"~:(g 16)~?=4,
    "caso 3:"~:(g 131)~?=1
 ]

testfuncionh= test[
    "caso 1:"~:(h 8)~?=16,
    "caso 2:"~:(h 16)~?=131,
    "caso 3:"~:(h 131)~?=8

 ]

testfuncionk= test[
    "caso 1:"~:(k 1)~?=16,
    "caso 2:"~:(k 4)~?=1,
    "caso 3:"~:(k 16)~?=4
 ]

testredondear= test[
    "caso 1:"~:(redondear 67.4)~?=67,
    "caso 2:"~:(redondear 3243.12544)~?=3243,
    "caso 0:"~:(redondear 0.4)~?=0,
    "caso negativo"~:(redondear (-4.4))~?=4
 ]

testabsoluto = test[
    "caso -:"~:(absoluto (-67))~?=67,
    "caso +:"~:(absoluto 67)~?=67,
    "caso 0:"~:(absoluto 0)~?=0
 ]

testmaximoabsoluto = test[
    "caso 1"~:(maxAbsoluto (-34) 45)~?=45,
    "caso 2"~:(maxAbsoluto (-67) 33)~?=67
 ]

testmaximo3 = test[
    "caso 1"~:(maximo3 1 56 77)~?=77,
    "caso 2"~:(maximo3 1 560 77)~?=560,
    "caso 3"~:(maximo3 100 56 77)~?=100,
    "caso iguales"~:(maximo3 2 2 2)~?=2
 ]

testalgunoesCero= test[
    "caso 1"~:(algunoesCero 3 2)~?=False,
    "caso 2"~:(algunoesCero 0 2)~?=True,
    "caso 3"~:(algunoesCero 1 0)~?=True,
    "caso 4"~:(algunoesCero 0 0)~?=True
 ]

testAmbosCero = test[
    "caso 1"~:(ambosCero 3 2)~?=False,
    "caso 2"~:(ambosCero 0 2)~?=False,
    "caso 3"~:(ambosCero 1 0)~?=False,
    "caso 4"~:(ambosCero 0 0)~?=True
 ]

testMismoIntervalo = test[
    "caso 1"~:(mismoIntervalo (-1) 0)~?=True,
    "caso 2"~:(mismoIntervalo 4 5)~?=True,
    "caso 3"~:(mismoIntervalo 10 50)~?=True,
    "caso 4"~:(mismoIntervalo (-4) 50)~?=False
 ]

testsumaDistintos = test[
    "caso 1"~:(sumaDistintos 30 20 10)~?=60,
    "caso repetidos"~:(sumaDistintos 3 3 3)~?=0
 ]

testesMultiplode = test[
    "caso 1"~:(esMultiplode (-60) 3)~?=True,
    "caso 2"~:(esMultiplode 77 2)~?=False
 ]

testdigitounidades = test[
    "caso 1"~:(digitoUnidades 30)~?=0,
    "caso 2"~:(digitoUnidades 61)~?=1,
    "caso 3"~:(digitoUnidades 3)~?=3
 ]

testdigitoDecenas = test[
    "caso 1"~:(digitoDecenas 234)~?=3,
    "caso 2"~:(digitoDecenas 302)~?=0
 ]

testproductointerno = test[
    "caso 1"~:(productoInterno (3,4) (3,4))~?=25,
    "caso 2"~:(productoInterno (1,1) (1,-1))~?=0
  ]

testesparMenor = test[
    "caso true"~:(esParMenor (3,4) (6,7))~?=True,
    "caso false"~:(esParMenor (30,40) (6,7))~?=False
 ]

testSumaTerna = test[
    "caso suma correcta"~:(sumaTerna(3,3,3))~?=9
 ]

testSumarMultiplos= test[
    "caso todos multiplos"~:(sumarSoloMultiplos(4,6,8) 2)~?=18,
    "caso 2 multiplos"~:(sumarSoloMultiplos(3,8,10) 2)~?=18,
    "caso 1 multiplo"~:(sumarSoloMultiplos(40,3,39) 13)~?=39,
    "caso ningun multiplo"~:(sumarSoloMultiplos(13,13,13) 8)~?=0
 ]

testposPrimerPar = test [
    "caso con pares"~:(posPrimerPar [3,5,10,5,5])~?=2,
    "caso con impares"~:(posPrimerPar [3,5,5,5])~?=4
 ]

testCrearpar = test[
    "caso con enteros"~:(crearPar 5 6)~?=(5,6),
    "caso con string"~:(crearPar "luca" "lulo")~?=("luca","lulo") 
    ]

testInvertir = test[
    "caso con enteros"~:(invertir (3,5))~?=(5,3),
    "caso con string"~:(invertir ("luca","lulo"))~?=("lulo","luca")    
 ]

testf = test[
    "caso menor a 7"~:(funcion 5)~?=25,
    "caso mayor a 7"~:(funcion 15)~?=29
 ]

testg = test[
    "caso par"~:(guncion 6)~?=3,
    "caso impar"~:(guncion 7 )~?=22
 ]

testTodosMenores = test[
    "caso correcto"~:(todosMenores (4,6,7))~?=True
 ]
testbisiesto = test[
    "caso 2000"~:(bisiesto 2000)~?=True,
    "caso 1900"~:(bisiesto 1900)~?=False,
    "caso 2100"~:(bisiesto 2100)~?=False
 ]

testDistanciaManhattan = test[
    "caso consigna 1"~:(distanciaManhattan (2,3,4) (7,3,8))~?=9.0,
    "caso consigna 2"~:(distanciaManhattan  ((-1), 0, (-8.5)) (3.3, 4, (-4)))~?=12.8
 ]

testsumaultimodosdigitos = test[
    "caso -1"~:(comparar 45 312)~?=(-1),
    "caso 1"~:(comparar 2312 7)~?=1,
    "caso 0"~:(comparar 45 172)~?=0
 ]