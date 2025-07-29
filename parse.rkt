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

(struct funcall (fun args) #:transparent)
(struct fundef (args body) #:transparent)
(struct ident (name) #:transparent)
(struct ifexpr (condition true else) #:transparent)
(struct toplevel-let (name value) #:transparent)
(struct toplevel-def (name value) #:transparent)
(struct lit (value) #:transparent)

(provide funcall fundef ident ifexpr toplevel-let toplevel-def lit
         funcall? fundef? ident? ifexpr? toplevel-let? toplevel-def? lit?
         funcall-fun funcall-args
         fundef-args fundef-body
         ident-name
         ifexpr-condition ifexpr-true ifexpr-else
         toplevel-let-name toplevel-let-value
         toplevel-def-name toplevel-def-value
         lit-value)

(require 'tokens)

; if parser parses, return its result, else error
(define (expect parser s errmsg)
  (or (parser s) (error errmsg)))

; return new parser that
; calls passed parser for as many times as it succeeds,
; return list of results
; return '() if first parse fails
(define (repeated parser)
  ; need to use define to let p be recursive
  (define (p s)
    (let ([r (parser s)])
      (if r
          (cons r (p s))
          '())))
    p)

; list of at least one result of parser,
; or false if first parse fails
(define (repeated+ parser)
  (lambda (s)
    (let ([r (parser s)])
      (if r
          (cons r ((repeated parser) s))
          #f))))

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
    (parse-toplevel s)))

(define (parse-string s)
  (parse (open-input-string s)))

; return #f or list of top-level def 
(define (parse-toplevel s)
  (let ([r ((repeated parse-toplevel-item) s)])
  (if (end-token? (token-stream-current s))
      r
      (error "failed to parse top-level item, expected def or let"))))

(define (parse-toplevel-item s)
  (or (parse-toplevel-def s)
      (parse-toplevel-let s)
      #f))

(define reserved-words (list "def" "end" "fun" "if" "let" "then"))

(define (parse-ident s)
  (let ([peek (token-stream-current s)])
    (if (and (equal? (token-kind peek) 'symbolic)
             (not (member (token-text peek) reserved-words)))
        (ident (token-text (advance s)))
        #f)))

(define (parse-toplevel-def s)
  (if ((match-token (lambda (t) (and (equal? (token-kind t) 'symbolic)
                                      (equal? (token-text t) "def")))) s)
      ; commit to the parse
      (let ([id (expect parse-ident s "expected ident after def")]
            [open-paren (expect (op-matcher "(") s "expected ( after fn name")]
            [args ((repeated parse-ident) s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or (")]
            [body (expect (repeated+ parse-expr) s "expected body after )")]
            [end-kw (expect (sym-matcher "end") s "expected 'end' after fn body")])
        (toplevel-def id (fundef args body)))
      #f))

(define (parse-toplevel-let s)
  (if ((match-token (lambda (t) (and (equal? (token-kind t) 'symbolic)
                                     (equal? (token-text t) "let")))) s)
      (let ([n (expect parse-ident s "expected name after let")]
            [equal-op (expect (match-token (lambda (t) (and (equal? (token-kind t) 'operator) (equal? (token-text t) "="))))
                              s
                              "expected = after let id")]
            [bound-value (expect parse-expr s "expected expression after =")])
        (toplevel-let n bound-value))
      #f))

(define (parse-expr s)
  (let ([main (or (parse-lit s)
                  (parse-paren-expr s)
                  (parse-if s)
                  (parse-fundef s)
                  (parse-ident s)  ; at end so it doesn't parse keywords as vars
                  #f)])
    ; parse optional postfix clause (fn call parens, etc)
    (if main
        (or (expr-postfix s main) main)
        #f)))

(define (expr-postfix s prefix)
  (if ((op-matcher "(") s)
      (let ([args ((repeated parse-expr) s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or ( of fun call")])
        (let ([result (funcall prefix args)])
          ; might have another postfix after this
          (or (expr-postfix s result) result)))
      #f))

(define (parse-fundef s)
  (if ((sym-matcher "fun") s)
      (let ([open-paren (expect (op-matcher "(") s "expected ( after fun keyword")]
            [args ((repeated parse-ident) s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or (")]
            [body (expect (repeated+ parse-expr) s "expected body after )")]
            [end-kw (expect (sym-matcher "end") s "expected 'end' after fn body")])
        (fundef args body))
      #f))

(define (parse-lit s)
  (let ([n ((match-token (lambda (t) (equal? (token-kind t) 'numeric))) s)])
    (if n
        (lit (token-text n))
        #f)))

(define (parse-paren-expr s)
  (if ((op-matcher "(") s)
      (let ([body (expect parse-expr s "expected expression after (")]
            [closer (expect (op-matcher ")") s "expected ) after parenthesized expression")])
        body)
      #f))

(define (parse-if s)
  (define (else-clause s)
    (if ((sym-matcher "else") s)
        (expect parse-expr s "expected expression after else")
        #f))
  (if ((sym-matcher "if") s)
      (let ([condition (expect parse-expr s "expected condition expr after if")]
            [then-kw (expect (sym-matcher "then") s "expected then after if condition")]
            [body (expect parse-expr s "expected body of conditional after then")]
            [else (else-clause s)]
            [endkw (expect (sym-matcher "end") s "expected end at end of if expression")])
        (ifexpr condition body else))
      #f))

(module+ test

(require rackunit)

(check-equal? (parse (open-input-string "  "))
              '())

(check-equal? (parse (open-input-string "def cd(a) 42 end"))
              `(,(toplevel-def (ident "cd") (fundef (list (ident "a")) (list (lit 42))))))

(check-equal? (parse (open-input-string "def cd() 42 end"))
              `(,(toplevel-def (ident "cd") (fundef '() (list (lit 42))))))

(check-equal? (parse (open-input-string "def cd( a) 42 end def fred(z) 13 end"))
              `(,(toplevel-def (ident "cd") (fundef (list (ident "a")) (list (lit 42))))
                ,(toplevel-def (ident "fred") (fundef (list (ident "z")) (list (lit 13))))))

(check-equal? (parse (open-input-string "let abc=42"))
              `(,(toplevel-let (ident "abc") (lit 42))))

(check-equal? (parse (open-input-string "let abc=42"))
              `(,(toplevel-let (ident "abc") (lit 42))))

(check-equal? (parse (open-input-string "let abc=(3) let x=3"))
              `(,(toplevel-let (ident "abc") (lit 3)) ; paren-expr unwraps itself
                ,(toplevel-let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= if x then (3) end let x=3"))
              `(,(toplevel-let (ident "abc") (ifexpr (ident "x") (lit 3) #f))
                ,(toplevel-let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= ( (( x) ))"))
              `(,(toplevel-let (ident "abc") (ident "x"))))

(check-equal? (parse (open-input-string "let abc= if x then (3) else if 4 then ( ( (x) ) ) end end let x=3"))
              `(,(toplevel-let (ident "abc") (ifexpr (ident "x") (lit 3)
                                       (ifexpr (lit 4) (ident "x") #f)))
                ,(toplevel-let (ident "x") (lit 3))))

(check-equal? (parse (open-input-string "let abc= fun (x) if x then 3 else 4 end end"))
              `(,(toplevel-let (ident "abc")
                  (fundef (list (ident "x"))
                          (list (ifexpr (ident "x")
                                        (lit 3)
                                        (lit 4)))))))

(check-equal? (parse (open-input-string "let abc = f (( 4 ))"))
              `(,(toplevel-let (ident "abc") (funcall (ident "f") (list (lit 4))))))

(check-equal? (parse (open-input-string "let abc = f()"))
              `(,(toplevel-let (ident "abc") (funcall (ident "f") '()))))

(check-equal? (parse-string "let abc = f(1)(2)")
              `(,(toplevel-let (ident "abc") (funcall (funcall (ident "f") (list (lit 1))) (list (lit 2)))))
              "nested/repeated funcall")

(check-equal? (parse-string "let abc = f(1)(2)(3)")
              `(,(toplevel-let (ident "abc") (funcall (funcall (funcall (ident "f") (list (lit 1))) (list (lit 2))) (list (lit 3)))))
              "nested/repeated funcall, nest harder")
)
