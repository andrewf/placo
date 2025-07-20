#lang racket

(provide my-eval)

(require "parse.rkt")

(module+ test
  (require rackunit))

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (empty-env) '())

(define (lookup env var)
  (if (empty? env)
      'missing-var
      (let* ([curr-binding (car env)]
             [parent-env (cdr env)]
             [curr-var (car curr-binding)]
             [curr-val (cdr curr-binding)])
        (if (equal? var curr-var)
            curr-val
            (lookup parent-env var)))))

(define (bind-env var value env)
  (cons (cons var value) env))

(define (my-eval expr env)
  (let ([discr (car expr)]
        [payload (cdr expr)])
    (cond
      [(equal? discr 'ident)
       (lookup env (car payload))]
      [(equal? discr 'lit)
       (car payload)]
      [(equal? discr 'if)
         (eval-if payload env)]
      [else (error (format "invalid expression ~a" discr))])))

(define (eval-if payload env)
  (let ([condition (car payload)]
        [true-branch (cadr payload)]
        [else-branch (caddr payload)])
    (if (truthy (my-eval condition env))
        (my-eval true-branch env)
        (if else-branch
            (my-eval else-branch env)
            ; empty else case
            #f))))
    
(module+ test
  (check-equal? (my-eval '(ident "f")
                         (bind-env "f" 12 (empty-env)))
                12
                "trivial var")

  (check-equal? (my-eval '(if (lit 0) (lit 1) (lit 2)) (empty-env))
                2
                "trivial if false")

  (check-equal? (my-eval '(if (lit 42) (lit 1) (lit 2)) (empty-env))
                1
                "trivial if true")

  (check-equal? (my-eval '(if (ident "f") (lit 1) (lit 2))
                         (bind-env "f" 0 (empty-env)))
                2
                "if var")
)
