#lang racket

(provide eval-toplevel eval-expr
         bind-env empty-env)

(require "parse.rkt")

(module+ test
  (require rackunit))

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (empty-env) '())

(define (lookup env var)
  (if (empty? env)
      (error "missing-var")
      (let* ([curr-binding (car env)]
             [parent-env (cdr env)]
             [curr-var (car curr-binding)]
             [curr-val (cdr curr-binding)])
        (if (equal? var curr-var)
            curr-val
            (lookup parent-env var)))))

(define (bind-env var value env)
  (cons (cons var value) env))

; evaluate a toplevel into an env itself
(define (eval-toplevel parsed [starting-env (empty-env)])
  (let loopy ([toplevel-remaining parsed]
              [curr-env starting-env])
    (if (empty? toplevel-remaining)
        curr-env  ; done
        (let* ([item (car toplevel-remaining)])
          (cond
            [(toplevel-let? item)
             (let* ([var   (ident-name (toplevel-let-name item))]
                    [value (eval-expr (toplevel-let-value item) curr-env)])
               (loopy (cdr toplevel-remaining)
                      (bind-env var value curr-env)))]
            [(toplevel-def? item)
             (let* ([id (ident-name (toplevel-def-name item))]
                    [f (toplevel-def-value item)])
               (loopy (cdr toplevel-remaining)
                      (bind-env id (eval-fundef f curr-env) curr-env)))])))))

(define (eval-expr expr env)
  (cond
    [(ident? expr)
     (lookup env (ident-name expr))]
    [(lit? expr)
     (lit-value expr)]
    [(ifexpr? expr)
     (eval-if expr env)]
    [(funcall? expr)
     (eval-funcall expr env)]
    [(fundef? expr)
     (eval-fundef expr env)]
    [else (error (format "invalid expression ~a" expr))]))

(define (eval-fundef expr env)
  ; just need to capture lexical context
  ; we can do that with closure in host language. lol.
  (let* ([fun-arg (fundef-arg expr)]
         [fun-body (fundef-body expr)]
         [arg-name (and fun-arg (ident-name fun-arg))])
    (lambda (arg-value)
      (let ([actual-env (if (and arg-value arg-name)
                            (bind-env arg-name arg-value env)
                            env)])
          (eval-expr fun-body actual-env)))))

(define (eval-if expr env)
  (let ([condition (ifexpr-condition expr)]
        [true-branch (ifexpr-true expr)]
        [else-branch (ifexpr-else expr)])
    (if (truthy (eval-expr condition env))
        (eval-expr true-branch env)
        (if else-branch
            (eval-expr else-branch env)
            ; empty else case
            'no-else))))

(define (eval-funcall payload env)
  (let* ([fun-expr (funcall-fun payload)]
         [arg-value (funcall-arg payload)]
         [fun (eval-expr fun-expr env)])
    (if (procedure? fun)
        (fun (and arg-value (eval-expr arg-value env)))
        (error (format "trying to call non-function ~a" fun)))))

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
                           (eval-toplevel (parse-string
                              "let g = 1 let g = 2 let f = g")))
                2
                "sequential let")

  (check-equal? (eval-expr (ident "f")
                           (eval-toplevel (parse-string
                              "def g(a) if a then 42 else 17 end end let f = g(1)")))
                42
                "basic fun call")

  (check-equal? (eval-expr (funcall (ident "g") #f)
                           (eval-toplevel (parse-string "let g = fun() (17) end")))
                17
                "fun call no args")

  (check-equal? (eval-expr (funcall (ident "g") #f)
                           (eval-toplevel (parse-string
                                           "def h(x) fun() x end end
                                            let g = h(19)"))) ; g = fun() 19 end
                19
                "closure")
)
