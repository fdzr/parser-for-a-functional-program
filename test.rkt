#lang play
(require "main.rkt")
(require "machine.rkt")


;; parse-type
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test (parse-type '{{Num -> Num} -> Num})(TFun (TFun (TNum) (TNum)) (TNum)))
(test (parse-type '{{Num -> Num} -> {Num -> Num}})(TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))))
(test/exn (parse-type '{ -> Num}) "Parse error")
(test/exn (parse-type '{Num -> }) "Parse error")

;; prettify
(test (prettify (TNum)) 'Num)
(test (prettify(TFun (TNum)(TNum))) '(Num -> Num))
(test (prettify(TFun (TFun (TNum)(TNum))(TNum))) '((Num -> Num) -> Num))
(test (prettify(TFun (TFun (TNum)(TNum))(TFun(TNum)(TNum)))) '((Num -> Num) -> (Num -> Num)))

;; parse
(test (parse '1) (num 1))
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{fun{x : Num} : Num {+ 1 2}}) (fun 'x (TNum) (add (num 1) (num 2)) (TNum)))
(test (parse '{fun{x : {Num -> Num}} : {Num -> Num} {fun{y : Num} y}}) (fun 'x (TFun (TNum) (TNum)) (fun 'y (TNum) (id 'y) #f) (TFun (TNum) (TNum))))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))
(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x)(num 1))(TNum)))
(test (parse '{with {y : Num 2}
             {+ x y}}) (app(fun 'y (TNum)(add (id 'x)(id 'y)) #f)(num 2)))
(test (parse '{{fun {f : {Num -> Num}} {f 12}} {fun {x : Num} {+ x x}}}) (app(fun 'f (TFun (TNum)(TNum))(app(id 'f)(num 12)) #f)(fun 'x (TNum)(add (id 'x)(id 'x)) #f)))
(test (parse '{with {x : Num {+ 1 2}}{+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (add (num 1) (num 2))))
(test (parse '{with {x : Num {with {z : Num 3}{+ 1 2}}}{+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (app (fun 'z (TNum) (add (num 1) (num 2)) #f) (num 3))))
(test (parse '{with {x : Num 5}{with {y : Num 67}{- x y}}}) (app (fun 'x (TNum) (app (fun 'y (TNum) (sub (id 'x) (id 'y)) #f) (num 67)) #f) (num 5)))
(test (parse '{with {x : Num {with {z : Num 3}{+ 1 2}}}{with {y : Num {with {w : {Num -> Num} 4}{+ w x}}}{+ 2 3}}}) (app
 (fun 'x (TNum) (app (fun 'y (TNum) (add (num 2) (num 3)) #f) (app (fun 'w (TFun (TNum) (TNum)) (add (id 'w) (id 'x)) #f) (num 4))) #f)
 (app (fun 'z (TNum) (add (num 1) (num 2)) #f) (num 3))))
(test (parse '{fun {x : Num}{+ x y}}) (fun 'x (TNum) (add (id 'x) (id 'y)) #f))
(test (parse '{fun {x : {Num -> Num}}{+ x y}}) (fun 'x (TFun (TNum)(TNum)) (add (id 'x) (id 'y)) #f))
(test (parse '{fun {x : Num} : Num {+ x y}}) (fun 'x (TNum) (add (id 'x) (id 'y)) (TNum)))
(test (parse '{fun {x : {Num -> Num}} : Num {+ x y}}) (fun 'x (TFun (TNum)(TNum)) (add (id 'x) (id 'y)) (TNum)))

;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{fun{x : Num} : Num x})) (fun-db(acc 0)))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test (deBruijn 
   (parse '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}})) (add (num 1)(app (fun-db(app (fun-db (add (acc 1) (acc 0))) (num 2)))(num 1))))
(test (deBruijn (parse '{{fun {x : Num} : Num {+ x 10}} {+ 2 3}})) (app (fun-db (add (acc 0) (num 10))) (add (num 2) (num 3))))
(test (deBruijn (parse '{{fun {x : {Num -> Num}} : {Num -> Num} {+ x 10}} {+ 2 3}})) (app (fun-db (add (acc 0) (num 10))) (add (num 2) (num 3))))
(test (deBruijn (parse '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y z}}}}})) (app (fun-db (app (fun-db (app (fun-db (add (acc 2) (add (acc 1) (acc 0)))) (num 3))) (num 2))) (num 1)))
(test (deBruijn (parse '{+ 1 {with {x : Num 1}
                                   {with {y : Num 2}
                                         {with {z : Num 3}
                                               {+ x {+ y z}}}}}})) (add (num 1) (app (fun-db
                                                                                      (app (fun-db (app (fun-db (add (acc 2) (add (acc 1) (acc 0))))
                                                                                                        (num 3))) (num 2))) (num 1))))

(test/exn (deBruijn (parse '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y w}}}}})) "Free identifier: w")

(test/exn (deBruijn 
   (parse '{+ 1 {with {x : Num 1}
                           {with {z : Num 2}
                                 {+ x y}}}})) "Free identifier: y")

(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;;compile
(test (compile (add (num 2) (num 1))) (list  (INT-CONST 1) (INT-CONST 2) (ADD)))
(test (compile (deBruijn (num 3))) (list (INT-CONST 3)))
(test (compile (deBruijn (parse '{fun{x : Num} : Num x})))(list (CLOSURE (list (ACCESS 0) (RETURN)))))
(test (compile (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))) (list (INT-CONST 5)
                                                                                                 (CLOSURE (list (INT-CONST 1) (ACCESS 0) (ADD)
                                                                                                 (CLOSURE (list (ACCESS 1) (ACCESS 0) (ADD) (RETURN))) (APPLY) (RETURN)))
                                                                                                 (APPLY)))
(test (compile (deBruijn 
   (parse '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}}))) (list (INT-CONST 1)
                                                     (CLOSURE (list (INT-CONST 2)
                                                     (CLOSURE (list (ACCESS 0) (ACCESS 1) (ADD) (RETURN))) (APPLY) (RETURN)))
                                                     (APPLY) (INT-CONST 1) (ADD)))
(test (compile (deBruijn (parse '{{fun {x : Num} : Num {+ x 10}} {+ 2 3}}))) (list (INT-CONST 3) (INT-CONST 2) (ADD)
                                                                                   (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY)))
(test (compile (deBruijn (parse '{{fun {x : {Num -> Num}} : {Num -> Num} {+ x 10}} {+ 2 3}}))) (list (INT-CONST 3) (INT-CONST 2) (ADD)
                                                                                                     (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY)))

(test (compile (deBruijn (parse '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y z}}}}}))) (list (INT-CONST 1) (CLOSURE (list (INT-CONST 2)
                                                                              (CLOSURE (list (INT-CONST 3)
                                                                              (CLOSURE (list (ACCESS 0) (ACCESS 1) (ADD) (ACCESS 2) (ADD) (RETURN)))
                                                                              (APPLY) (RETURN))) (APPLY) (RETURN))) (APPLY))
)
(test/exn (compile (deBruijn (parse '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y w}}}}}))) "Free identifier: w")
(test/exn (compile (deBruijn 
   (parse '{+ 1 {with {x : Num 1}
                           {with {z : Num 2}
                                 {+ x y}}}}))) "Free identifier: y")

;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))
(test (typeof (parse '{fun{x : Num} : Num 5}))(TFun (TNum)(TNum)))
(test (typeof (parse '{fun {x : Num} : {Num -> Num} {fun {y : Num} : Num {+ x y}}})) (TFun (TNum) (TFun (TNum) (TNum))))
(test (typeof (parse '{fun {x : Num} x}) )(TFun (TNum) (TNum)))
(test (typeof (parse '{{fun {x : Num} x} 1}))(TNum))
(test (typeof (parse '{{fun {f : {Num -> Num}} {f 12}} {fun {x : Num} {+ x x}}})) (TNum))
(test/exn (typeof (parse '{fun {x : Num} : {Num -> Num} 10})) "Type error in expression fun position 1: expected (Num -> Num) found Num")
(test/exn (typeof (parse '{1 2})) "Type error in expression app position 1: expected (T -> S) found Num")
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}})) "Type error in expression app position 2: expected Num found (Num -> Num)")
(test/exn (typeof (parse '{fun{x : Num}{+ x {fun{z : Num} 1}}})) "Type error in expression + position 2: expected Num found (Num -> Num)")
(test/exn (typeof (parse 'y)) "Type error: free identifier: y")
(test/exn (typeof (parse '{fun {x : Num} : Num {fun {y : Num} : Num {+ x y}}})) "Type error in expression fun position 1: expected Num found (Num -> Num)")
(test/exn (typeof (parse '{{fun{x : {Num -> Num}}{x 2}} {fun{y : Num}{+ x 1}}})) "Type error: free identifier: x")


;typecheck
(test (typecheck '3) 'Num)
(test (typecheck '{+ 1 2}) 'Num)
(test (typecheck  '{fun {f : Num} : Num 10}) '(Num -> Num))
(test (typecheck '{fun{x : Num} : Num 5}) '(Num -> Num))
(test (typecheck '{fun{x : Num} 5}) '(Num -> Num))
(test (typecheck '{fun {x : Num} : {Num -> Num} {fun {y : Num} : Num {+ x y}}})'(Num -> (Num -> Num)))
(test (typecheck '{fun {x : Num} x}) '(Num -> Num))
(test (typecheck '{{fun {x : Num} x} 1}) 'Num)
(test (typecheck '{{fun {f : {Num -> Num}} {f 12}} {fun {x : Num} {+ x x}}}) 'Num)
(test (typecheck '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}}) 'Num)
(test (typecheck '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y z}}}}}) 'Num)
(test/exn (typecheck  '{+ 2 {fun {x : Num} : Num x}}) "Type error in expression + position 2: expected Num found (Num -> Num)")

;typed-compile
(test (typed-compile '{{fun {x : Num} : Num
                                   {+ x 10}} {+ 2 3}}) (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY))
)
(test (typed-compile '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}}) (list (INT-CONST 1) (CLOSURE (list (INT-CONST 2) (CLOSURE (list (ACCESS 0) (ACCESS 1) (ADD) (RETURN))) (APPLY) (RETURN))) (APPLY) (INT-CONST 1) (ADD))
)
(test (typed-compile '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y z}}}}}) (list (INT-CONST 1)
                                                              (CLOSURE (list (INT-CONST 2)
                                                                             (CLOSURE (list (INT-CONST 3)
                                                                                            (CLOSURE (list (ACCESS 0) (ACCESS 1) (ADD) (ACCESS 2) (ADD) (RETURN)))
                                                                                                                        (APPLY) (RETURN))) (APPLY) (RETURN))) (APPLY)))
(test (typed-compile '{+ 1 {with {x : Num 10}
                                 {with {y : Num 2}
                                       {+ x y}}}}) (list (INT-CONST 10)
                                                         (CLOSURE (list (INT-CONST 2)
                                                                        (CLOSURE (list (ACCESS 0) (ACCESS 1) (ADD) (RETURN)))
                                                                            (APPLY) (RETURN))) (APPLY) (INT-CONST 1) (ADD))
)
(test (typed-compile '{{fun{x : {Num -> Num}}{x 2}} {fun{y : Num}{+ y 1}}}) (list (CLOSURE (list (INT-CONST 1) (ACCESS 0) (ADD) (RETURN)))
                                                                                  (CLOSURE (list (INT-CONST 2) (ACCESS 0) (APPLY) (RETURN))) (APPLY)))
;run-program
(test (run-program(typed-compile '{{fun {x : Num} : Num
                                   {+ x 10}} {+ 2 3}})) 15)

(test (run-program(typed-compile '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}})) 4)
(test (run-program(typed-compile '{with {x : Num 1}
                          {with {y : Num 2}
                                {with {z : Num 3}
                                      {+ x {+ y z}}}}})) 6)
(test (run-program(typed-compile '{+ 1 {with {x : Num 10}
                           {with {y : Num 2}
                                 {+ x y}}}})) 13)
(test (run-program (typed-compile '{{fun{x : {Num -> Num}}{x 2}} {fun{y : Num}{+ y 1}}})) 3)
(test/exn (run-program (typed-compile '{{fun{x : {Num -> Num}}{x 2}} {fun{y : Num}{+ x 1}}})) "Free identifier: x")
