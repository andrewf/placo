#lang racket

(require "parse.rkt")
(require "eval.rkt")

(define prelude-env (bind-env "print" (lambda (args) (printf "~a\n" (car args)))
                              (bind-env "plus" (lambda (args) (+ (car args) (cadr args)))
                                        (empty-env))))

(define env (eval-toplevel (parse (current-input-port)) prelude-env))

(eval-expr (funcall (ident "main") '()) env)