#lang racket

(provide parse)

(require "tok.rkt")

(struct token-stream (current next) #:mutable)

(define (make-token-stream next)
  (token-stream (next) next))

(define (end-token? t) (equal? (token-kind t) 'end))

; if parser parses, return its result, else error
(define (expect parser s errmsg)
  (or (parser s) (error errmsg)))

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
  (let ([s (make-token-stream (tokenize in-port))])
    (top-level s)))

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

(define (expr s)
  (let ([n ((match-token (lambda (t) (equal? (token-kind t) 'numeric))) s)])
    (if n
        (list 'lit (token-text n))
        #f)))

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