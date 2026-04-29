#lang racket

(provide eval-toplevel
         eval-expr
         bind-env
         empty-env)

(require "parse.rkt" "visit.rkt" "env.rkt")

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (eval-visit-funcall fun-thunk arg-thunks)
  (lambda (env)
    (let ((fun (fun-thunk env))
          (arg-values (map (lambda (value-thunk) (value-thunk env)) arg-thunks)))
    (if (procedure? fun)
        (fun arg-values)
        (error (format "trying to call non-function ~a" fun))))))

(define (eval-visit-opcall op arg-thunks)
  (lambda (env)
    (let ([arg-values (map (lambda (value-thunk) (value-thunk env)) arg-thunks)])
      (cond
        [(equal? op "+") (apply + arg-values)]
        [(equal? op "-") (apply - arg-values)]
        [(equal? op "*") (apply * arg-values)]
        [(equal? op "/") (apply / arg-values)]
        [(equal? op "^") (apply expt arg-values)]))))

(define (eval-visit-fundef arg-names body-thunk)
  ; just need to capture lexical context
  ; we can do that with closure in host language. lol.
  (lambda (env-at-def)
    (lambda (arg-values)
      (let ([actual-env (bind-env-names arg-names arg-values env-at-def)])
        (body-thunk actual-env)))))

(define (eval-visit-ifexpr condition true-branch else-branch)
  (lambda (env)
    (if (truthy (condition env))
        (true-branch env)
        (if else-branch
            (else-branch env)
            ; empty else case
            'no-else))))

(define (eval-visit-lit value)
  (lambda (env) value))

(define (eval-visit-ident name)
  (lambda (env)
    (lookup env name)))

(define (eval-visit-expr-list-init) (void))

(define (eval-visit-reduce-expr-list expr-result acc) expr-result) ; only keep latest result

(define (eval-visit-toplevel-init) (empty-env))

(define (eval-visit-toplevel-reduce name expr-result acc-env)
  (bind-env name (expr-result acc-env) acc-env))

(define eval-visitor (visitor
                      eval-visit-funcall
                      eval-visit-opcall
                      eval-visit-fundef
                      eval-visit-ifexpr
                      eval-visit-lit
                      eval-visit-ident
                      eval-visit-expr-list-init
                      eval-visit-reduce-expr-list
                      eval-visit-toplevel-init
                      eval-visit-toplevel-reduce))

(define (eval-expr expr env)
  ((visit-expr expr eval-visitor) env))

(define (eval-toplevel expr [starting-env (empty-env)])
  (visit-toplevel expr eval-visitor starting-env))


(module+ test
  (require rackunit))

(module+ test
  (check-equal? (eval-expr (ident "f")
                         (bind-env "f" 12 (empty-env)))
                12
                "trivial var")

  (check-equal? (eval-expr (ifexpr (lit 0) (lit 1) (lit 2)) (empty-env))
                2
                "trivial if false")

  (check-equal? (eval-expr (ifexpr (lit 42) (lit 1) (lit 2)) (empty-env))
                1
                "trivial if true")

  (check-equal? (eval-expr (ifexpr (ident "f") (lit 1) (lit 2))
                           (bind-env "f" 0 (empty-env)))
                2
                "if var")

  (check-equal? (eval-expr (ident "f")
                           (eval-toplevel
                             (parse-string
                               "let g = 1 let g = 2 let f = g")))
                2
                "sequential let")

  (check-equal? (eval-expr (opcall "+" (list (ident "f") (lit 2)))
                           (bind-env "f" 1 (empty-env)))
                3
                "basic expr var")

  (check-equal? (eval-expr (opcall "+" (list (ident "f")
                                             (opcall "*" (list (ident "f") (ident "f")))))
                           (bind-env "f" 2 (empty-env)))
                6
                "basic expr var")

   (check-equal? (eval-expr
                   (ident "f")
                   (eval-toplevel
                    (parse-string
                      "def g(a) if a then 42 else 17 end end let f = g(1)")))
                 42
                 "basic fun call")

   (check-equal? (eval-expr (funcall (ident "g") '())
                            (eval-toplevel (parse-string "let g = fun() (17) end")))
                 17
                 "fun call no args")

   (check-equal? (eval-expr (funcall (ident "g") '())
                            (eval-toplevel (parse-string "let g = fun() if 4 then 3 else 5 end 42 end")))
                 42
                 "fun call multiple body expressions")

   (check-equal? (eval-expr (funcall (ident "g") '())
                            (eval-toplevel (parse-string
                                            "def h(x) fun() x end end
                                             let g = h(19)"))) ; g = fun() 19 end
                 19
                 "closure")

   (check-equal? (eval-expr (funcall (ident "g") (list (lit 3) (ident "x")))
                            (eval-toplevel (parse-string
                                            "def h(x) fun(a b) b end end
                                             let x = 5
                                             let g = h(19)"))) ; g = fun(a b) b end
                 5
                 "multiple args")
)
