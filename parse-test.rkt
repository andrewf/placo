#lang racket

(require rackunit
         "parse.rkt")

(check-equal? (parse (open-input-string "  "))
              '())

(check-equal? (parse (open-input-string "def cd(a) 42 end"))
              '((def (ident "cd") (ident "a") (lit 42))))

; lexer is stupid, so space between parens is mandatory
(check-equal? (parse (open-input-string "def cd( ) 42 end"))
              '((def (ident "cd") #f (lit 42))))

(check-equal? (parse (open-input-string "def cd( a) 42 end def fred(z) 13 end"))
              '((def (ident "cd") (ident "a") (lit 42))
                (def (ident "fred") (ident "z") (lit 13))))

(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))

(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))
