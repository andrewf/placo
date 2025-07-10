#lang racket

(provide parse)

(require "tok.rkt")

(struct token-stream (current next) #:mutable)

(define (make-token-stream next)
  (token-stream (next) next))

(define (end-token? t) (equal? (token-kind t) 'end))

(define (advance s)
  (let ([curr (token-stream-current s)])
    (if (end-token? curr)
        ; don't advance a stream at its end
        (curr)
        ; not ended yet, actually advance
        (let ([newest ((token-stream-next s))])
          (set-token-stream-current! s newest)
          curr))))

(define (parse in-port)
  (let ([s (make-token-stream (tokenize in-port))])
    (top-level s)))

; return #f or list of top-level def 
(define (top-level s)
  (if (end-token? (token-stream-current s))
      '()
      (let ([p (top-level-def s)])
        (if p
            (cons p (top-level s))
            (error "failed to parse top-level item")))))

(define (expect parser s errmsg)
  (or (parser s) (error errmsg)))

(define (ident s)
  (let ([peek (token-stream-current s)])
    (if (equal? (token-kind peek) 'symbolic)
        (list 'ident (token-text (advance s)))
        #f)))

(define (top-level-def s)
  (let ([peek (token-stream-current s)])
    (if (end-token? peek)
      #f
      (if (and (equal? (token-kind peek) 'symbolic)
               (equal? (token-text peek) "def"))
          ; commit to the parse
          (begin
            (advance s) ; skip def
            (let ([id (expect ident s "expected ident after def")])
              (list 'def id)))
          (error (format "expected 'def' at top level, got ~a" peek))))))