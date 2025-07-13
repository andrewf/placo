#lang racket

(require rackunit
         "parse.rkt")


(check-equal? (parse (open-input-string "  "))
              '())

(check-equal? (parse (open-input-string "def cd"))
              '((def (ident "cd"))))

(check-equal? (parse (open-input-string "def cd def fred"))
              '((def (ident "cd")) (def (ident "fred"))))

(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))


(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))
