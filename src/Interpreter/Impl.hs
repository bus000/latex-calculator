module Interpreter.Impl where

import ASTree

interpret :: Expr -> Number
interpret (Sum e1 e2) = (interpret e1) + (interpret e2)
interpret (Product e1 e2) = (interpret e1) * (interpret e2)
interpret (Fraction e1 e2) = (interpret e1) / (interpret e2)
interpret (Minus e1 e2) = (interpret e1) - (interpret e2)
interpret (Literal num) = num
