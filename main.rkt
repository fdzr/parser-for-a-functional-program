#lang play
(require "machine.rkt")
(print-only-errors #t) 
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg body))

; parse-type : type -> Type o error
; parse-type recibe una expresion correspondiente a la gramatica de tipos y devuelve una expresion con los nodos del AST Type o error
; en caso de que falte una expresion en <type> -> <type>
(define (parse-type s-expr)
  (match s-expr
    [(? symbol?)(if(equal? s-expr 'Num)(TNum) (error "Type must be Num"))]
    [(list exp1 '-> exp2)(TFun (parse-type exp1)(parse-type exp2))]
    [(list exp1 '-> )(error "Parse error")]
    [(list '-> exp2 )(error "Parse error")])
  )

#|
<my-expr> ::= <number>
             | (list '+ <my-sexpr> <my-sexpr>)
             | (list '- <my-sexpr> <my-sexpr>)
             | (list 'with (list x ': <type> <my-expr>) <my-expr>)
             | (list 'fun (list x ': <type>) <my-expr>)
             | (list 'fun (list x ': <type>)': <type> <my-expr>)
             | (list <my-expr> <my-expr>)
|#
; parse : my-expr -> Expr
; recibe una expresión del lenguaje en cuestion y retorna una expresion con los nodos del AST Expr
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

; prettify : Type -> type
; funcion inversa a parse-type, recibe una expresion de tipo Type y devuelve la expresion de la gramatica de tipos que la generó
(define (prettify type)
  (match type
    [(? TNum?)(quote Num)]
    [(TFun arg body)( list (prettify arg) '-> (prettify body))]))

; Pregunta 2

; ADT:
; deftype Env
; empty-env: Env
; extend-env: Id Type Env -> Env
; Env va a guardar las variables que identifican a la funciones y el tipo de parametro que reciben las funciones
(deftype Env
  (emptyEnv)
  (aEnv id type-return next))

(define empty-env (emptyEnv))
(define extend-env aEnv)

; lookup-env: Id Env -> Type (o error)
; Esta función va a recibir un ambiente de variables y si encuentra el identificador x, devuelve el tipo de parámetro que recibe la función
; a la q x identifica, y si no encuentra el identificador se devuelve un error 
(define (lookup-env x env)
  (match env
    [(emptyEnv)(error "Type error: free identifier:"x)]
    [(aEnv id type-return next)(if(equal? id x)
                       type-return
                      (lookup-env x next))]
    )
  )

; typeof-aux: Expr Env -> Type o error
; Función que recibe una expresión parseada del lenguaje en cuestión y un ambiente de variables, y devuelve el tipo de la expresión, es decir, indica que tipo de expresión es y que devuelve,
; y en caso de función el tipo de argumento que recibe y el tipo del cuerpo
(define (typeof-aux expr env)
  (match expr
      [(num x)(TNum)]
      [(id x)(lookup-env x env)]
      [(add exp1 exp2)(if(not(equal?(typeof-aux exp1 env) (TNum)))
                         (error "Type error in expression + position 1: expected Num found" (prettify (typeof-aux exp1 env)))
                       (if(not(equal?(typeof-aux exp2 env) (TNum)))
                          (error (~a "Type error in expression + position 2: expected Num found " (prettify (typeof-aux exp2 env))))
                          (TNum)))]
      [(sub exp1 exp2)(if(not(equal?(typeof-aux exp1 env) (TNum)))
                         (error "Type error in expression + position 1: expected Num found ")
                       (if(not(equal?(typeof-aux exp2 env) (TNum)))
                          (error (~a "Type error in expresion + position 2: expected Num found"))
                          (TNum)))]
      [(fun id type-parameter body type-return)(if(equal? type-return #f)
                                                  (TFun type-parameter (typeof-aux body (extend-env id type-parameter env)))
                                                (if(equal? type-return (typeof-aux body (extend-env id type-parameter env)))
                                                   (TFun type-parameter (typeof-aux body (extend-env id type-parameter env)))
                                                   (error (~a "Type error in expression fun position 1: expected " (prettify type-return) " found " (prettify (typeof-aux body (extend-env id type-parameter env)))))))]
      [(app function arg)(if(not(TFun? (typeof-aux function env)))
                            (error "Type error in expression app position 1: expected (T -> S) found" (prettify(typeof-aux function env)))
                            (if(equal?(TFun-arg(typeof-aux function env))(typeof-aux arg env))
                               (TFun-body(typeof-aux function env))
                               (error (~a "Type error in expression app position 2: expected " (prettify (TFun-arg(typeof-aux function env))) " found " (prettify (typeof-aux arg env))))
                               ))]
      )
  )

; typeof: Expr -> Type o error
; función que recibe una expresión y devuelve el tipo de la expresión, es decir el retorno de la expresión, o error en caso de que algo falle
(define (typeof expr)
  (typeof-aux expr empty-env))

; typecheck: my-expr -> type o error
; función que recibe una expresión correspondiente al lenguaje en cuestión y devuelve una expresión correspondiente a la gramática de tipos o error
(define (typecheck expr)
  (prettify(typeof (parse expr))))


; 3 ejercicio

; ADT:
; deftype Env1

; Env1 va a guardar las variables que identifican a la funciones
(deftype Env1
  (emptyEnv1)
  (aEnv1 id next))

; empty-env1: Env1
;representa el ambiente vacío
(define empty-env1 (emptyEnv1))

; extend-env1: Id Env1 -> Env1
; permite extender el ambiente
(define extend-env1 aEnv1)

; lookup-env-set-indice: id Env1 Val -> Val o error
; Esta función recibe un id un ambiente y un valor, el id es el que se va a buscar en el ambiente de variables, y el valor se va a utilizar como contador
; para llevar la cuenta a que distancia está el identificador del scope en el que fue declarado, en caso de que el identificador no aparezca se lanza un error
(define (lookup-env-set-indice x env initializer)
  (match env
    [(emptyEnv1)(error "Free identifier:"x)]
    [(aEnv1 id next)(if(equal? id x)
                       initializer
                      (lookup-env-set-indice x next (+ 1 initializer)))]
    )
  )

; deBruijn-aux: Expr Env1 -> Expr o error
; función que recibe una expresión, un ambiente y devuelve una expresión con los índices de De Bruijn, o error en caso de que exista un identificador libre
(define (deBruijn-aux expr env)
  (match expr
    [(num n)(num n)]
    [(id x)(acc (lookup-env-set-indice x env 0))]
    [(add exp1 exp2)(add (deBruijn-aux exp1 env)(deBruijn-aux exp2 env))]
    [(sub exp1 exp2)(add (deBruijn-aux exp1 env)(deBruijn-aux exp2 env))]
    [(fun id type-parameter body type-return)(fun-db (deBruijn-aux body (extend-env1 id env)))]
    [(app function arg)(app (deBruijn-aux function env)(deBruijn-aux arg env))]
    )
  )

; deBruijn: Expr -> Expr o error
; función que recibe una expresión y devuelve una expresión con los índices de De Bruijn, o error en caso de que exista un identificaodr libre
(define (deBruijn expr)
  (deBruijn-aux expr empty-env1))


; compile: Expr -> List[Instructions]
; función que recibe una expresión y devuelve una lista de instrucciones(código máquina)
; esta función elimina unas listas que se generan adicionales cuando la expresión se reduce por los nodos add y sub
(define (compile expr)
  (flatten (compile-aux expr))
  )


; compile: Expr -> List[Instructions]
; función que recibe una expresión y devuelve una lista de instrucciones(código máquina)
(define (compile-aux expr)
  (match expr
    [(num x)(INT-CONST x)]
    [(acc n)(ACCESS n)]
    [(add exp1 exp2)(list(compile-aux exp2)(compile-aux exp1)(ADD))]
    [(sub exp1 exp2)(list(compile-aux exp2)(compile-aux exp1)(SUB))]
    [(fun-db body)(CLOSURE (flatten (list(compile-aux body)(RETURN))))]
    [(app function arg)(list(compile-aux arg)(compile-aux function) (APPLY))]
    )
  )


; typed-compile: my-expr -> List[Instructions] o error
; esta función recibe una <my-expr> y genera código máquina, o error en caso de que la función no esté bien tipada o existan identificadores libres
(define (typed-compile s-expr)
  (def s-exp-parse (parse s-expr))
  (def s-expr-compile (compile (deBruijn s-exp-parse)))
  (if (typeof s-exp-parse)
      s-expr-compile
      (error "Some error happened..."))
  )

; run-program: List[Instructions] -> Val o error si la expresion no está bien tipada
; esta función recibe una lista de instrucciones(código máquina) y produce el valor correspondiente de la expresión o error si no está bien tipada
; esta función además sirve para probar que el resultado que produce la función typed-compile está correcto
(define (run-program list-instructions)
  (exec-machine list-instructions))