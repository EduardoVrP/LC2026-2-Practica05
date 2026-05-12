{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Test.Hspec
import Practica05
import Terminos
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    describe "Tests apsubT" $ do
        it "Sustitucion sencilla" $ do
            apsubT t1 sigma1 `shouldBe` y
        it "Sustitucion en funcion" $ do
            apsubT t2 sigma3 `shouldBe` Fun "f" [Fun "a" [],Fun "b" []]
        it "Sustitucion anidada" $ do
            apsubT t3 sigma3 `shouldBe` Fun "g" [Fun "f" [Fun "a" []],Var "z"]
        it "Sustitucion anidada 2" $ do
            apsubT t4 sigma4 `shouldBe` Fun "h" [Var "y",Fun "f" [Var "z",Var "y"]]

    describe "Tests simpSus" $ do
        it "Sustitucion trivial" $ do
            simpSus [("x", x)] `shouldBe` []
        it "Sustitucion que no se simplifica" $ do
            simpSus sigma1 `shouldMatchList` sigma1
        it "Sustitucion mas compleja" $ do
            simpSus [("x", y), ("y", y), ("z", f x)] `shouldMatchList` [("x", y), ("z", f x)]
       
    describe "Tests compSus" $ do
        it "sigma3.sigma4" $ do
            compSus sigma3 sigma4 `shouldMatchList` [("x",Fun "a" []),("y",Fun "b" [])]
        it "sigma.rho" $ do
            compSus sigma rho `shouldMatchList` [("x",Fun "f" [Var "y"]),("y",Var "w"),("z",Fun "b" [])]
        it "rho.tau" $ do
            compSus rho tau `shouldMatchList` [("x",Fun "g" [Fun "f" [Fun "c" []]]),("z",Fun "b" []),("y",Fun "b" []),("w",Fun "f" [Fun "c" []]),("v",Var "w")]
        it "sigma.tau" $ do
            compSus sigma tau `shouldMatchList` [("x",Fun "f" [Fun "b" []]),("y",Fun "f" [Fun "c" []]),("w",Fun "f" [Fun "c" []]),("v",Var "w")]

    describe "Tests unifica" $ do
        it "Dos variables" $ do
            unifica x y `shouldMatchList` [[("x",Var "y")]]
        it "Dos constantes iguales" $ do
            unifica a a `shouldBe` [[]]
        it "Dos constantes distintas" $ do
            unifica a c `shouldBe` []
        it "Dos funciones con diferente nombre" $ do
            unifica (Fun "g" [x,y]) (Fun "h" [x, z]) `shouldBe` []
        it "Dos funciones con aridades distintas" $ do
            unifica (Fun "g" [x,y]) (Fun "g" [x,z,y]) `shouldBe` []
        it "Terminos unificables" $ do
            unifica (Fun "f" [g x, Fun "h" [x, Var "u"]]) (Fun "f" [z,Fun "h" [Fun "f" [y,y], z]]) `shouldMatchList` [[("z",Fun "g" [Fun "f" [Var "y",Var "y"]]),("x",Fun "f" [Var "y",Var "y"]),("u",Fun "g" [Fun "f" [Var "y",Var "y"]])]]
        it "Terminos unificables 2" $ do
            unifica (Fun "Q" [x, f a]) (Fun "Q" [y,z]) `shouldMatchList` [[("x",Var "y"),("z",Fun "f" [Fun "a" []])]]
        it "Terminos unificables 3" $ do
            unifica (Fun "f" [w, Fun "f" [x, Fun "h" [z]]]) (Fun "f" [Fun "g" [x], Fun "f" [x,y]]) `shouldMatchList` [[("w", Fun "g" [Var "x"]),("y",Fun "h" [Var "z"])]] 
        it "Terminos NO unificables" $ do
            unifica (Fun "f" [ x, y, x ]) (Fun "f" [ y, g x, x ]) `shouldBe` []
        it "Terminos NO unificables 2" $ do
            unifica (Fun "f" [Fun "g" [x], Fun "f" [x, Fun "h" [z]]]) (Fun "f" [Fun "g" [x], Fun "f" [a,b]]) `shouldBe` []

    describe "Tests unificaConj" $ do
        it "Prueba w1" $ do
            unificaConj w1 `shouldBe` []
        it "Prueba w2" $ do
            unificaConj w2 `shouldMatchList` [[("x",Fun "f" [Fun "a" []]),("y",Fun "f" [Fun "a" []]),("w",Fun "f" [Fun "a" []]),("z",Fun "a" [])]]
        it "Prueba w3" $ do
            unificaConj w3 `shouldBe` []
        it "Prueba w4" $ do
            unificaConj w4 `shouldMatchList` [[("z",Fun "a" []),("x",Fun "f" [Fun "a" []]),("u",Fun "g" [Var "y"])]]
        it "Prueba w5" $ do
            unificaConj w5 `shouldBe` []
        it "Prueba w6" $ do
            unificaConj w6 `shouldBe` []
        it "Prueba w7" $ do
            unificaConj w7 `shouldMatchList` [[("z",Fun "g" [Fun "f" [Var "y",Var "y"]]),("x",Fun "f" [Var "y",Var "y"]),("u",Fun "g" [Fun "f" [Var "y",Var "y"]])]]
        it "Prueba w8" $ do
            unificaConj w8 `shouldMatchList` [[("y",Fun "f" [Fun "f" [Fun "a" []]]),("z",Fun "f" [Fun "a" []]),("x",Fun "f" [Fun "f" [Fun "a" []]])]]