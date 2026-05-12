module Terminos where

type Nombre = String

--Terminos de la logica de primer orden
--Fun recibe los argumentos con una lista de terminos
data Term = Var Nombre | Fun Nombre [Term] deriving (Eq)

instance Show Term where 
    show (Var p) = p
    show (Fun a []) = a
    show (Fun f args) = f ++ "(" ++ showArgs args ++ ")"

-- función auxiliar
showArgs :: [Term] -> String
showArgs [] = ""
showArgs [x] = show x
showArgs (x:xs) = show x ++ "," ++ showArgs xs

--Sinonimo para las sustituciones
type Subst = [(Nombre,Term)]

--COSAS PARA PROBAR (NO BORRAR PORQUE NO VAN A JALAR LOS TEST)
x,y,z,w,v :: Term
x = Var "x"
y = Var "y"
z = Var "z"
w = Var "w"
v = Var "v"

a,b,c :: Term
a = Fun "a" []
b = Fun "b" []
c = Fun "c" []

t1,t2,t3,t4 :: Term
t1 = Var "x"
t2 = Fun "f" [x, y]
t3 = Fun "g" [Fun "f" [x], z]
t4 = Fun "h" [x, Fun "f" [y, x]]

sigma1,sigma2,sigma3,sigma4,sigma5 :: Subst
sigma1 = [("x", y)]
sigma2 = [("x", a)]
sigma3 = [("x", a), ("y", b)]
sigma4 = [("x", y), ("y", z)]
sigma5 = []  -- vacía

f,g :: Term -> Term
f t = Fun "f" [t]
g t = Fun "g" [t]


sigma :: Subst
sigma =
    [ ("x", f y)
    , ("y", w)
    ]

rho :: Subst
rho =
    [ ("x", g w)
    , ("z", b)
    ]

tau :: Subst
tau =
    [ ("y", b)
    , ("w", f c)
    , ("v", w)
    ]


w1 :: [Term]
w1 =
  [ Fun "P" [ x, f x, c ]
  , Fun "P" [ Var "u", w, w ]
  ]

w2 :: [Term]
w2 =
  [ Fun "Q" [ x, y ]
  , Fun "Q" [ f z, x ]
  , Fun "Q" [ Var "w", f a ]
  ]

w3 :: [Term]
w3 =
  [ Fun "f" [ x, f (Fun "f" [x, z]) ]
  , Fun "f" [ g x, f (Fun "f" [x, y]) ]
  , Fun "f" [ g x, f (Fun "f" [a, b]) ]
  ]

w4 :: [Term]
w4 =
  [ Fun "R" [ a, x, f (g y) ]
  , Fun "R" [ z, f z, f (Var "u") ]
  ]

w5 :: [Term]
w5 =
  [ Fun "h" [ f a, g x ]
  , Fun "h" [ z, z ]
  ]

w6 :: [Term]
w6 =[ Fun "f" [ x, y, x ], Fun "f" [ y, g x, x ], Fun "f" [ a, z , a]]

w7 :: [Term]
w7 = [(Fun "f" [Fun "g" [x], Fun "h" [x, Var "u"]]), (Fun "f" [z, Fun "h" [Fun "f" [y, y], z]])]

w8 :: [Term]
w8 = [Fun "Q" [y,z], Fun "Q" [x, f a], Fun  "Q" [f z, z]]
