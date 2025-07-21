#lang racket

(require "parse.rkt")
(require "eval.rkt")

(define prelude-env (bind-env "print" (lambda (arg) (printf "~a\n" arg))
                              (bind-env "plus" (lambda (rhs) (lambda (lhs) (+ rhs lhs)))
                              (empty-env))))

(define env (eval-toplevel (parse (current-input-port)) prelude-env))

(eval-expr (funcall (ident "main") #f) env)