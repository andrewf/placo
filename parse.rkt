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
(struct opcall (op args) #:transparent)
(struct fundef (args body) #:transparent)
(struct ident (name) #:transparent)
(struct ifexpr (condition true else) #:transparent)
(struct toplevel-let (name value) #:transparent)
(struct toplevel-def (name value) #:transparent)
(struct lit (value) #:transparent)

(provide funcall  opcall  fundef  ident  ifexpr  toplevel-let  toplevel-def  lit
         funcall? opcall? fundef? ident? ifexpr? toplevel-let? toplevel-def? lit?
         funcall-fun funcall-args
         opcall-op   opcall-args
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
; return list of results.
; return '() if first parse fails.
; always succeeds.
(define (repeated parser)
  ; need to use define to let repeat-step be recursive
  (define (repeat-step s)
    (let ([r (parser s)])
      (if r
          (cons r (repeat-step s))
          '())))
    repeat-step)

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

; predicate generators for matching tokens
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

(define precedence
  ; columns: operator, precedence, right-assoc?
  (make-hash '(("=" 10 1)
               ("+" 20 0)
               ("-" 20 0)
               ("*" 30 0)
               ("/" 30 0)
               ("^" 40 1))))

;; (define (parse-expr s)
;;   (let ([main (or (parse-lit s)
;;                   (parse-paren-expr s)
;;                   (parse-if s)
;;                   (parse-fundef s)
;;                   (parse-ident s)  ; at end so it doesn't parse keywords as vars
;;                   #f)])
;;     ; parse optional postfix clause (fn call parens, etc)
;;     (if main
;;         (or (expr-postfix s main) main)
;;         #f)))

(define (parse-expr s)
  (parse-op-expr s 0))

(define (parse-opleaf-expr s)
  (let ([leaf (or (parse-lit s)
                  (parse-paren-expr s)
                  (parse-if s)
                  (parse-fundef s)
                  (parse-ident s)  ; at end so it doesn't parse keywords as vars
                  #f)])
    ; easier to do postfix operator here than in op-expr
    ; this basically means postfix is higher precedence than all operators
    (if leaf
        (or (expr-postfix s leaf) leaf)
        #f)))

(define (parse-op-expr s prev-prec)
  (let ([lhs (parse-opleaf-expr s)])
    (if lhs
        (parse-op-tail s lhs prev-prec)
        ; this is not an expression at all: fail.
        #f)))

; consume a stream of operators and exprs (in that order)
; of precedence greater than min-prec (precedence climbing)
; op-tail expects the operator to be next on the stream
; lhs is expression before current operator
(define (parse-op-tail s lhs prev-prec)
  (let ([peek (token-stream-current s)])
    (if (equal? (token-kind peek) 'operator)
        (let* ([curr-op (token-text peek)] ; op to right of lhs, so lhs op rhs
               [op-info (hash-ref precedence curr-op '(0 0))]
               [curr-prec        (car op-info)]
               [curr-right-assoc (cadr op-info)])
          (if (< prev-prec curr-prec)
              (begin
                (advance s) ; permanently pull the op off the stream
                ; consume operators higher than curr-prec
                (let* ([rhs (expect (lambda (s) (parse-op-expr s (- curr-prec curr-right-assoc)))
                                   s
                                   (format "expected expression after ~a" (token-text peek)))]
                       [new-lhs (opcall curr-op (list lhs rhs))])
                  ; continue at previous prec level
                  (parse-op-tail s new-lhs prev-prec)))
              ; else curr-prec <= prev-prec
              ; left-associate
              ; leave lower-precedence operator on the stream
              lhs))
        ; next token is not an operator
        lhs)))

(define (parse-paren-expr s)
  (if ((op-matcher "(") s)
      (let ([body (expect parse-expr s "expected expression after (")]
            [_closer (expect (op-matcher ")") s "expected ) after parenthesized expression")])
        body)
      #f))

(define (expr-postfix s prefix)
  (if ((op-matcher "(") s)
      (let ([args ((repeated parse-expr) s)]
            [close-paren (expect (op-matcher ")") s "expected ) after arg or ( of fun call")])
        (let ([result (funcall prefix args)])
          ; might have another postfix after this
          (or (expr-postfix s result) result)))
      #f))

(module+ test

  (require rackunit)


  (check-equal? (parse-expr (port->token-stream (open-input-string "2 * 3 + 4")))
                (opcall "+" (list (opcall "*" (list (lit 2) (lit 3)))
                                  (lit 4)))
                "basic operator left-leaning tree")

  (check-equal? (parse-expr (port->token-stream (open-input-string "2 + 3 + 4")))
                (opcall "+" (list (opcall "+" (list (lit 2) (lit 3)))
                                  (lit 4)))
                "plus associates left")

  (check-equal? (parse-expr (port->token-stream (open-input-string "2 ^ 3 ^ 4")))
                (opcall "^" (list (lit 2)
                                  (opcall "^" (list (lit 3) (lit 4)))))
                "caret associates right")

  (check-equal? (parse-expr (port->token-stream (open-input-string "2 + 3")))
                (opcall "+" (list (lit 2) (lit 3)))
                "basic operator")

  (check-equal? (parse-expr (port->token-stream (open-input-string "2 + 3 * 4")))
                (opcall "+" (list (lit 2) (opcall "*" (list (lit 3) (lit 4)))))
                "basic operator right-leaning tree")

  (check-equal? (parse-expr (port->token-stream (open-input-string "1 + 2 * 3 + 4 * 5")))
                (opcall "+" (list (opcall "+" (list (lit 1)
                                                    (opcall "*" (list (lit 2) (lit 3)))))
                                  (opcall "*" (list (lit 4) (lit 5)))))
                "deeper mixed precedence tree")

  (check-equal? (parse-expr (port->token-stream (open-input-string "1 = 2 ^ 3 = 4 * 5 ^ 6 ^ 7 * (8 + 9) ")))
                (opcall "="
                        (list
                         (lit 1)
                         (opcall "="
                                 (list (opcall "^" (list (lit 2) (lit 3)))
                                       (opcall "*"
                                               (list
                                                (opcall "*"
                                                        (list
                                                         (lit 4)
                                                         (opcall "^"
                                                                 (list
                                                                  (lit 5)
                                                                  (opcall "^" (list (lit 6) (lit 7)))))))
                                                (opcall "+" (list (lit 8) (lit 9)))))))))
                "deeper mixed assoc tree")

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
   
  (check-equal? (parse-expr (port->token-stream (open-input-string "(g)(g)")))
                (funcall (ident "g") (list (ident "g")))
                "basic operator left-leaning tree")

  (check-equal? (parse-expr (port->token-stream (open-input-string "2 + 3 (4)")))
                (opcall "+" (list
                             (lit 2)
                             (funcall (lit 3) (list (lit 4)))))
                "basic operator left-leaning tree")

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
