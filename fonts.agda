-● 
-•
—•
–•
⊸
↠ 

◦• ⁌⁍


eval(lam λx.E x) ->> return(lam λx.E x).
eval(app E₁ E₂) ->> comp(app₁ E₂) • eval(E₁).
comp(app₁ E₂) • return(V₁) ->> comp(app₂ V₁) • eval(E₂).
comp(app₂ (lam λx.E₀ x)) • return(V₂) ->> eval(E₀ V₂).

%query eval(app (lam λx.x) (app (lam λy.y) (lam λz.c))) ⊢ return(lam λz. z).

eval(app (lam λx.x) (app (lam λy.y) (lam λz.c)))
comp(app₁ (app (lam λy.y) (lam λz.c))) • eval(lam λx.x)
comp(app₁ (app (lam λy.y) (lam λz.c))) • return(lam λx.x)
comp(app₂ (lam λx.x)) • eval(app (lam λy.y) (lam λz.c))
comp(app₂ (lam λx.x)) • comp(app₁ (lam λz.c)) • eval(lam λy.y)
comp(app₂ (lam λx.x)) • comp(app₁ (lam λz.c)) • return(lam λy.y)
comp(app₂ (lam λx.x)) • comp(app₂ (lam λy.y)) • eval(lam λz.c)
comp(app₂ (lam λx.x)) • comp(app₂ (lam λy.y)) • return(lam λz.c)
comp(app₂ (lam λx.x)) • eval(lam λz.c)
comp(app₂ (lam λx.x)) • return(lam λz.c)
eval(lam λz.c)
return(lam λz.c)

