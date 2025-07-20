#lang racket

(provide my-eval)

(require "parse.rkt")

(module+ test
  (require rackunit))

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (empty-env) '())

(define (lookup env var) #f
  )

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
  (check-equal? (my-eval '(if (lit 0) (lit 1) (lit 2)) (empty-env))
                2
                "um")
  (check-equal? (my-eval '(if (lit 1) (lit 1) (lit 2)) (empty-env))
                1
                "um")
)