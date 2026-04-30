#lang racket

(require racket/generator)

(provide token token? token-kind token-text tokenize token-sequence)

(struct tokendef
  (flush start? continue? error?))

(define (false-pred p) #f)

(define (make-tokendef flush-fn
                       start-pred
                       [continue-pred false-pred]
                       [error-pred false-pred])
  (tokendef flush-fn start-pred continue-pred error-pred))

(struct token (kind text) #:transparent)

(define (copy-char in-stream out-stream) (write-char (read-char in-stream) out-stream))

(define (parse-token text-stream td)
  (let* ([so-far (open-output-string)]
         [flush (lambda () ((tokendef-flush td) (get-output-string so-far)))])
    (if ((tokendef-start? td) (peek-char text-stream))
        (begin
          (copy-char text-stream so-far) ; incl matching first char
          (let do-token ()
            (let ([p (peek-char text-stream)])
              (cond
                [(eof-object? p)
                 (flush)]
                [((tokendef-continue? td) p)
                 (begin
                   (copy-char text-stream so-far)
                   (do-token))]
                [((tokendef-error? td) p)
                 (error "lexer error")]
                [else
                 ; no errors, no continue, success
                 (flush)]))))
        ; start-pred failed, indicate match failure by returning false
        #f )))

(define (is-op? c)
  ; surely there must be a better way?
  (string-contains? "=!@$%^&*()[]{},./-+~|" (make-string 1 c)))

(define tokendefs
  (list
   ; whitespace
   (make-tokendef
    (lambda (s) '())  ; don't emit anything
    char-whitespace?
    char-whitespace?)
  ; comments
  (make-tokendef
   (lambda (s) '())  ; don't emit anything
   (lambda (p) (equal? p #\#))
   (lambda (p) (not (equal? p #\newline))))
  ; identifiers, symbols, etc
  (make-tokendef
   (lambda (s) (token 'symbolic s))
   char-alphabetic?
   (lambda (c) (or (char-alphabetic? c)
                   (char-numeric? c)
                   (equal? c #\-)
                   (equal? c #\_))))
  ; numbers
  (make-tokendef
   (lambda (s) (token 'numeric (or (string->number s) (error (format "invalid numeric '~a'" s)))))
   char-numeric?
   (lambda (c) (or (char-numeric? c) (equal? c #\.)))
   char-alphabetic?) ; no letters allowed without space, operator etc intervening
  ; operators
  (make-tokendef
   (lambda (s) (token 'operator s))
   is-op?)
   ; no continue, only single-char operators
   ))

; look for matching tokens, if any, return their flush value
; return false if failed to parse
(define (parse-tokendefs text-stream defs)
  (if (empty? defs)
      #f
      (let* ([curr (car defs)]
             [result (parse-token text-stream curr)])
        (or result (parse-tokendefs text-stream (cdr defs))))))

; return generator of tokens
(define (tokenize text-stream)
  (generator ()
    ; loop body
    (define (per-token)
      (let ([p (peek-char text-stream)])
        (if (eof-object? p)
            ; exit loop
            (void)
            (let ([r (parse-tokendefs text-stream tokendefs)])
              (cond
                [(token? r) (yield r)]
                [(empty? r) (void)]
                [else (error (format "unexpected character ~v" p))])
              (per-token)))))
    ; start loop
    (per-token)
    ; final token
    (yield (token 'end "bye"))))

(define (token-sequence text-stream)
  (in-producer (tokenize text-stream)
               (lambda (t)
                 (eq? (token-kind t) 'end))))

(module+ test
(require rackunit)

(check-equal? (sequence->list (token-sequence (open-input-string "a-b b_c")))
              (list (token 'symbolic "a-b")
                    (token 'symbolic "b_c"))
              "separates words")

(check-equal? (sequence->list (token-sequence (open-input-string "fred(")))
              (list (token 'symbolic "fred")
                    (token 'operator "("))
              "separates operators from words")

(check-equal? (sequence->list (token-sequence (open-input-string "((")))
              (list (token 'operator "(")
                    (token 'operator "("))
              "separates operators from each other")

(check-equal? (sequence->list (token-sequence (open-input-string "a3b 34 bc")))
              (list (token 'symbolic "a3b")
                    (token 'numeric 34)
                    (token 'symbolic "bc"))
              "supports numbers")

(check-equal? (sequence->list (token-sequence (open-input-string "a3+b 34/bc")))
              (list (token 'symbolic "a3")
                    (token 'operator "+")
                    (token 'symbolic "b")
                    (token 'numeric 34)
                    (token 'operator "/")
                    (token 'symbolic "bc"))
              "supports numbers")

(check-equal? (sequence->list (token-sequence (open-input-string "aaa# asdf \nbbb ")))
              (list (token 'symbolic "aaa")
                    (token 'symbolic "bbb"))
              "separates comments")

(check-exn exn:fail?
           (lambda () (sequence->list (token-sequence (open-input-string "a3b 34bc")))))

(check-exn exn:fail?
           (lambda () (sequence->list (token-sequence (open-input-string "a3b 3.4.5")))))

)