Se implementa la máquina CEK en CEK.hs.

Se modifica el main para reemplazar el uso de eval por evalCEK en el interprete.

Anotaciones complementarias en CEK.hs.

Antes de que se reemplazaran los operadores unarios por los binarios, los unarios estaban implementados del siguiente modo:

--search--  
```
search (UnaryOp _ Pred t) env k = search t env (KPred : k) 
search (UnaryOp _ Succ t) env k = search t env (KSucc : k) 
```

--destroy--
``` 
destroy (Cons 0) (KPred:k) = destroy (Cons 0)  k
destroy (Cons n) (KPred:k) = case (n < 1) of
                                True -> destroy (Cons 0) k
                                False -> destroy (Cons (n-1)) k
destroy (Cons n) (KSucc:k) = destroy (Cons (n+1)) k
```