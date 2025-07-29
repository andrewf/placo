#lang racket

(require "eval.rkt" "parse.rkt")

(define parsed (parse (current-input-port)))

(define env (eval-toplevel parsed
                           (bind-env "print" (lambda (x) (printf "~a\n" (car x)))
                                     (bind-env "plus" (lambda (args) (+ (car args) (cadr args)))
                                               (empty-env)))))

(eval-expr (funcall (ident "main") '()) env)