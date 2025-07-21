#lang racket

(provide parse parse-string)

(module tokens racket
  (require "tok.rkt")

  (provide port->token-stream end-token?
           token-stream-current token-stream-next
           token-kind token-text  ; re-exports from tok
           advance)

  (struct token-stream (current next) #:mutable)

  (define (make-token-stream next)
    (token-stream (next) next))

  (define (end-token? t) (equal? (token-kind t) 'end))

  (define (port->token-stream in-port) (make-token-stream (tokenize in-port)))

  ; pop current peek token off of token stream, move to next one
  (define (advance s)
    (let ([curr (token-stream-current s)])
      (if (end-token? curr)
          ; don't advance a stream at its end
          (curr)
          ; not ended yet, actually advance
          (let ([newest ((token-stream-next s))])
            (set-token-stream-current! s newest)
            curr))))
)

(require 'tokens)

; if parser parses, return its result, else error
(define (expect parser s errmsg)
  (or (parser s) (error errmsg)))

; create a parser that matches and returns a token or fails
(define (match-token pred)
  (lambda (s)
    (if (pred (token-stream-current s))
        (advance s)
        #f)))

(define (op-matcher text)
  (match-token (lambda (t) (and (equal? (token-kind t) 'operator)
                                (equal? (token-text t) text)))))

(define (sym-matcher text)
  (match-token (lambda (t) (and (equal? (token-kind t) 'symbolic)
                                (equal? (token-text t) text)))))

; real parser starts here
(define (parse in-port)
  (let ([s (port->token-stream in-port)])
    (top-level s)))

(define (parse-string s)
  (parse (open-input-string s)))

; return #f or list of top-level def 
(define (top-level s)
  (if (end-token? (token-stream-current s))
      '()
      (let ([p (top-level-item s)])
        (if p
            (cons p (top-level s))
            (error "failed to parse top-level item, expected def or let")))))

(define (top-level-item s)
  (or (top-level-def s)
      (top-level-let s)
      #f))

(define (ident s)
  (let ([peek (token-stream-current s)])
    (if (equal? (token-kind peek) 'symbolic)
        (list 'ident (token-text (advance s)))
        #f)))

(define (top-level-def s)
  (if ((match-token (lambda (t) (and (equal? (token-kind t) 'symbolic)
                                      (equal? (token-text t) "def")))) s)
      ; commit to the parse
      (let ([id (expect ident s "expected ident after def")]
            [open-paren (expect (op-matcher "(") s "expected ( after fn name")]
            [arg (ident s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or (")]
            [body (expect expr s "expected body after )")]
            [end-kw (expect (sym-matcher "end") s "expected 'end' after fn body")])
        (list 'def id arg body))
      #f))

(define (top-level-let s)
  (if ((match-token (lambda (t) (and (equal? (token-kind t) 'symbolic)
                                     (equal? (token-text t) "let")))) s)
      (let ([n (expect ident s "expected name after let")]
            [equal-op (expect (match-token (lambda (t) (and (equal? (token-kind t) 'operator) (equal? (token-text t) "="))))
                              s
                              "expected = after let id")]
            [bound-value (expect expr s "expected expression after =")])
        (list 'let n bound-value))
      #f))

(define (expr s)
  (let ([main (or (lit-expr s)
                  (paren-expr s)
                  (if-expr s)
                  (fun-expr s)
                  (ident s)  ; at end so it doesn't parse keywords as vars
                  #f)])
    ; parse optional postfix clause (fn call parens, etc)
    (if main
        (or (expr-postfix s main) main)
        #f)))

(define (expr-postfix s prefix)
  (if ((op-matcher "(") s)
      (let ([arg (expr s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or ( of fun call")])
        (let ([result (list 'funcall prefix arg)])
          ; might have another postfix after this
          (or (expr-postfix s result) result)))
      #f))

(define (fun-expr s)
  (if ((sym-matcher "fun") s)
      (let ([open-paren (expect (op-matcher "(") s "expected ( after fun keyword")]
            [arg (ident s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or (")]
            [body (expect expr s "expected body after )")]
            [end-kw (expect (sym-matcher "end") s "expected 'end' after fn body")])
        (list 'fun arg body))
      #f))

(define (lit-expr s)
  (let ([n ((match-token (lambda (t) (equal? (token-kind t) 'numeric))) s)])
    (if n
        (list 'lit (token-text n))
        #f)))

(define (paren-expr s)
  (if ((op-matcher "(") s)
      (let ([body (expect expr s "expected expression after (")]
            [closer (expect (op-matcher ")") s "expected ) after parenthesized expression")])
        body)
      #f))

(define (if-expr s)
  (define (else-clause s)
    (if ((sym-matcher "else") s)
        (expect expr s "expected expression after else")
        #f))
  (if ((sym-matcher "if") s)
      (let ([condition (expect expr s "expected condition expr after if")]
            [then-kw (expect (sym-matcher "then") s "expected then after if condition")]
            [body (expect expr s "expected body of conditional after then")]
            [else (else-clause s)]
            [endkw (expect (sym-matcher "end") s "expected end at end of if expression")])
        (list 'if condition body else))
      #f))

(define (var-expr s)
  (let ([p (ident s)])
    (if p
        (list 'var p)
        #f)))

(module+ test

(require rackunit)

(check-equal? (parse (open-input-string "  "))
              '())

(check-equal? (parse (open-input-string "def cd(a) 42 end"))
              '((def (ident "cd") (ident "a") (lit 42))))

(check-equal? (parse (open-input-string "def cd() 42 end"))
              '((def (ident "cd") #f (lit 42))))

(check-equal? (parse (open-input-string "def cd( a) 42 end def fred(z) 13 end"))
              '((def (ident "cd") (ident "a") (lit 42))
                (def (ident "fred") (ident "z") (lit 13))))

(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))

(check-equal? (parse (open-input-string "let abc=42"))
              '((let (ident "abc") (lit 42))))

(check-equal? (parse (open-input-string "let abc=(3) let x=3"))
              '((let (ident "abc") (lit 3)) ; paren-expr unwraps itself
                (let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= if x then (3) end let x=3"))
              '((let (ident "abc") (if (ident "x") (lit 3) #f))
                (let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= ( (( x) ))"))
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

(check-equal? (parse (open-input-string "let abc = f (( 4 ))"))
              '((let (ident "abc") (funcall (ident "f") (lit 4)))))

(check-equal? (parse (open-input-string "let abc = f()"))
              '((let (ident "abc") (funcall (ident "f") #f))))

(check-equal? (parse-string "let abc = f(1)(2)")
              '((let (ident "abc") (funcall (funcall (ident "f") (lit 1)) (lit 2))))
              "nested/repeated funcall")

(check-equal? (parse-string "let abc = f(1)(2)(3)")
              '((let (ident "abc") (funcall (funcall (funcall (ident "f") (lit 1)) (lit 2)) (lit 3))))
              "nested/repeated funcall, nest harder")
)
