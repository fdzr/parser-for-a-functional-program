#lang play

(print-only-errors #t)

(deftype AE
  (Num)
  (TNum)
  (TFun arg body))

(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id x)
  (fun-db exp)
  (acc n)
  (app fun exp)
  (fun parameter type body type-return))

(deftype Env
  (emptyEnv)
  (aEnv id type-return next))

(define empty-env (emptyEnv))
(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(emptyEnv)(error "Type error: free identifier: "x)]
    [(aEnv id type-return next)(if(equal? id x)
                       type-return
                      (lookup-env x next))]
    )
  )

(define (parse-type src)
  (match src
    [(? symbol?)(if(equal? src 'Num)(TNum) (error "Type must be Num"))]
    [(list exp1 '-> exp2)(TFun (parse-type exp1)(parse-type exp2))]
    [(list exp1 '-> )(error "Parse error")]
    [(list '-> exp2 )(error "Parse error")]))

(define (prettify type)
  (match type
    [(? TNum?)(quote Num)]
    [(TFun arg body)( list (prettify arg) '-> (prettify body))]))


(define (parse s-expr)
  (match s-expr
    [(? number?)(num s-expr)]
    [(? symbol?)(id s-expr)]
    [(list '+ exp1 exp2)(add (parse exp1)(parse exp2))]
    [(list '- exp1 exp2)(sub (parse exp1)(parse exp2))]
    [(list 'with (list id ': type exp) body)(app (fun id (parse-type type)(parse body)#f)(parse exp))]
    [(list 'fun (list x ': type)s-expr)(fun x (parse-type type)(parse s-expr)#f)]
    [(list 'fun (list x ': type)': type-return s-expr)(fun x (parse-type type)(parse s-expr)(parse-type type-return))]
    [(list exp1 exp2)(app(parse exp1)(parse exp2))]
    ))

; 2 ejercicio

(define (typeof expr env)
  (match expr
      [(num x)(TNum)]
      [(id x)(lookup-env x env)]
      [(add exp1 exp2)(if(not(equal?(typeof exp1 env) (TNum)))
                         (error "Type error in expression + position 1: expected Num found" (prettify (typeof exp1 env)))
                       (if(not(equal?(typeof exp2 env) (TNum)))
                          (error (~a "Type error in expresion + position 2: expected Num found " (prettify (typeof exp2 env))))
                          (TNum)))]
      [(sub exp1 exp2)(if(not(equal?(typeof exp1 env) (TNum)))
                         (error "Type error in expression + position 1: expected Num found ")
                       (if(not(equal?(typeof exp2 env) (TNum)))
                          (error (~a "Type error in expresion + position 2: expected Num found"))
                          (TNum)))]
      [(fun id type-parameter body type-return)(if(equal? type-return #f)
                                                  (TFun type-parameter (typeof body (extend-env id type-parameter env)))
                                                (if(equal? type-return (typeof body (extend-env id type-parameter env)))
                                                   (TFun type-parameter (typeof body (extend-env id type-parameter env)))
                                                   (error (~a "Type error in expression fun position 1: expected" (prettify type-return) " found " (prettify (typeof body (extend-env id type-parameter env)))))))]
      [(app function arg)(if(not(TFun? (typeof function env)))
                            (error "Type error in expression app position 1: expected (T -> S) found" (prettify(typeof function env)))
                            (if(equal?(TFun-arg(typeof function env))(typeof arg env))
                               (TFun-body(typeof function env))
                               (error ("Type error in expression app position 2: expected " (prettify (TFun-arg(typeof function env))) "found" (prettify (typeof arg env))))
                               ))]
      )
  )

(define (typecheck expr)
  (prettify(typeof (parse expr) empty-env)))


; 3 ejercicios

(deftype AST-for-De-Bruijn
  (ADD)
  (SUB)
  (INT-CONST n)
  (RETURN)
  (APPLY)
  (CLOSURE function-body return)
  (ACCESS n)
  )

(define (deBruijn expr env)
  (match expr
    [(num n)(num n)]
    [(id x)(acc (lookup-env-set-indice x env 0))]
    [(add exp1 exp2)(add (deBruijn exp1 env)(deBruijn exp2 env))]
    [(sub exp1 exp2)(add (deBruijn exp1 env)(deBruijn exp2 env))]
    [(fun id type-parameter body type-return)(fun-db (deBruijn body (extend-env id 0 env)))]
    [(app function arg)(app (deBruijn function env)(deBruijn arg env))]
    )
  )

;(aEnv 'y 0 (aEnv 'x 0 (emptyEnv)))
(define (lookup-env-set-indice x env initializer)
  (match env
    [(emptyEnv)(error "Free identifier: "x)]
    [(aEnv id type-return next)(if(equal? id x)
                       initializer
                      (lookup-env-set-indice x next (+ 1 initializer)))]
    )
  )

(define (compile-aux expr)
  (match expr
    [x x])
  )

(define (compile expr)
  (match expr
    [(num x)(INT-CONST x)]
    [(acc n)(ACCESS n)]
    [(add exp1 exp2)(list(compile exp2)(compile exp1)(ADD))]
    [(sub exp1 exp2)(list(compile exp2)(compile exp1)(SUB))]
    [(fun-db body)(CLOSURE (compile body) (RETURN))]
    [(app function arg)(list(compile arg)(compile function) (APPLY))]
    )
  )