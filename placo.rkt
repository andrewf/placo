#lang racket

(require "parse.rkt" "eval.rkt" "env.rkt")

(define prelude-env (bind-env-names
                     (list "plus"
                           "print")
                     (list
                      (lambda (args) (+ (car args) (cadr args)))
                      (lambda (args) (printf "~a\n" (car args))))
                     (empty-env)))

(define env (eval-toplevel (parse (current-input-port)) prelude-env))

(eval-expr (funcall (ident "main") '()) env)