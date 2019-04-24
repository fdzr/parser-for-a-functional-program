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
  (app fun exp)
  (fun parameter type body type-return))

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

(define is_function?
  (lambda(expr)
    (match expr
      [(id x)#f]
      [(num x)#f]
      [(add exp1 exp2)#f]
      [(sub exp1 exp2)#f]
      [(app f b)#f]
      [(fun id type-parameter body type-return)#t]
      )
    )
  )

;funcion parseada
;(fun 'x (TNum) (fun 'y (TNum) (add (id 'x) (id 'y)) (TNum)) #f)

;ejemplo que debe dar error en el tipo de retorno y lo q devuelve la funcion dentro del cuerpo de otra funcion
;(typeof (parse '{fun{x : Num} : {Num -> Num}{fun{y : Num}: {Num -> Num}{+ 1 2}}}))

;(typeof (parse '{fun{x : Num} : Num 5}))->(TFun (TNum)(TNum))
;(parse '{fun{x : Num} : Num 5}) -> (fun 'x (TNum) (num 5) (TNum))
;ejemplos para probar
;(typeof-aux (parse '{fun {x : Num} : {Num -> Num} {fun {y : Num} : Num {+ x y}}}))
;(typeof (parse '{fun {x : Num} : {Num -> Num} {fun {y : Num} : Num {+ x y}}}))
;(typeof (parse '{fun {x : Num} : Num {fun {y : Num} : Num {+ x y}}})) ejemplo para q de error

;aplicacion de funcion '{{fun{x : Num} : Num{+ 1 2}} {+ 1 2}}
(define (typeof expr)
  (match expr
    [(num x)(TNum)]
    [(id x)(TNum)]
    [(add exp1 exp2)(TNum)]
    [(sub exp1 exp2)(TNum)]
    [(app exp1 exp2)(if (not (is_function? exp1))
                        (error (~a "Type error in app position 1: expected (T -> S) found " (prettify (typeof exp1))))
                    (if (equal? (TFun-arg(typeof exp1)) (typeof exp2))
                        (TFun-body(typeof exp2))
                        (error (~a "Type error in app position 2: expected " (prettify(TFun-arg(typeof exp1))) " found " (prettify(typeof exp2))))))]
    [(fun id type-parameter body type-return)(if (equal? type-return #f)
                                                 (TFun type-parameter (typeof body))
                                             (if(equal? type-return (typeof body))
                                                 (TFun type-parameter(typeof body))
                                                 (error (~a "Type error in expression fun position 1: expected " (prettify type-return) " found " (prettify(typeof body))))))]
    )
  )






#|(test (parse-type '{Num -> Num})(TFun (TNum)(TNum)))
(test (parse-type '{Num -> {Num -> Num}})(TFun (TNum)(TFun (TNum)(TNum))))
(test (parse-type '{{Num -> Num} -> Num})(TFun (TFun (TNum)(TNum))(TNum)))
(test (parse-type '{{Num -> Num} -> {Num -> Num}})(TFun (TFun (TNum)(TNum))(TFun (TNum)(TNum))))
(test/exn (parse-type '{Num ->}) "Parse error")|#
#|
(test (parse '{with {x : Num {+ 1 2}}{+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (add (num 1) (num 2))))
(test (parse '{with {x : Num {with {z : Num 3}{+ 1 2}}}{+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (app (fun 'z (TNum) (add (num 1) (num 2)) #f) (num 3))))
(test (parse '{with {x : Num 5}{with {y : Num 67}{- x y}}}) (app (fun 'x (TNum) (app (fun 'y (TNum) (sub (id 'x) (id 'y)) #f) (num 67)) #f) (num 5)))
(test (parse '{with {x : Num {with {z : Num 3}{+ 1 2}}}{with {y : Num {with {w : {Num -> Num} 4}{+ w x}}}{+ 2 3}}}) (app
 (fun 'x (TNum) (app (fun 'y (TNum) (add (num 2) (num 3)) #f) (app (fun 'w (TFun (TNum) (TNum)) (add (id 'w) (id 'x)) #f) (num 4))) #f)
 (app (fun 'z (TNum) (add (num 1) (num 2)) #f) (num 3))))

(test (parse '{fun {x : Num}{+ x y}}) (fun 'x (TNum) (add (id 'x) (id 'y)) #f))
(test (parse '{fun {x : {Num -> Num}}{+ x y}}) (fun 'x (TFun (TNum)(TNum)) (add (id 'x) (id 'y)) #f))
(test (parse '{fun {x : Num} : Num {+ x y}}) (fun 'x (TNum) (add (id 'x) (id 'y)) (TNum)))
(test (parse '{fun {x : {Num -> Num}} : Num {+ x y}}) (fun 'x (TFun (TNum)(TNum)) (add (id 'x) (id 'y)) (TNum)))|#


(deftype Env
  (emptyEnv)
  (aEnv id next))

(define empty-env (emptyEnv))
(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(emptyEnv)(error "Type error: free identifier: "x)]
    [(aEnv y next)(if(equal? y x)
                      (TNum)
                      (lookup-env x next))]
    )
  )


(define (typeid expr env)
  (match expr
    [(num n)(TNum)]
    [(id x)(lookup-env x env)]
    [(add exp1 exp2)(if(and (equal? (typeid exp1 env) (TNum))(equal? (typeid exp2 env) (TNum)))
                       (TNum)
                       (error "Bad syntax"))]
    [(sub exp1 exp2)(if(and (equal? (typeid exp1 env) (TNum))(equal? (typeid exp2 env) (TNum)))
                       (TNum)
                       (error "Bad syntax"))]
    [(fun x type-parameter body type-return)(TFun type-parameter (typeid body (extend-env x env)))]
    )
  )






