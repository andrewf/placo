#lang racket

(require "eval.rkt" "parse.rkt")

(define parsed (parse (current-input-port)))

(define env (eval-toplevel parsed
                           (bind-env "print" (lambda (x) (printf "~a\n" (car x)))
                                     (bind-env "plus" (lambda (rhs) (lambda (lhs) (+ (car rhs) (car lhs))))
                                               (empty-env)))))

(eval-expr (funcall (ident "main") '()) env)