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


(check-equal? (parse (open-input-string "let abc= (3) let x=3"))
              '((let (ident "abc") (lit 3)) ; paren-expr unwraps itself
                (let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= if x then (3) end let x=3"))
              '((let (ident "abc") (if (ident "x") (lit 3) #f))
                (let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= ( ( ( x) ) )"))
              '((let (ident "abc") (ident "x"))))

(check-equal? (parse (open-input-string "let abc= if x then (3) else if 4 then ( ( (x) ) ) end end let x=3"))
              '((let (ident "abc") (if (ident "x") (lit 3)
                                       (if (lit 4) (ident "x") #f)))
                (let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= fun (x) if x then 3 else 4 end end"))
              '((let (ident "abc")
                  (fun (ident "x")
                       (if (ident "x")
                           (lit 3)
                           (lit 4))))))

(check-equal? (parse (open-input-string "let abc = f ( ( 4 ) )"))
              '((let (ident "abc") (funcall (ident "f") (lit 4)))))