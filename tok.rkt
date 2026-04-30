#lang racket

(require racket/generator)

(provide token token? token-kind token-text tokenize token-sequence)

(struct token (kind text) #:transparent)

(define (parse-token text-stream
                     flush-fn
                     start-pred?
                     continue-pred?
                     [error-pred? (lambda (p) #f)])
  (let* ([so-far (open-output-string)]
         [copy-char (lambda () (write-char (read-char text-stream) so-far))]
         [flush (lambda () (flush-fn (get-output-string so-far)))])
    (if (start-pred? (peek-char text-stream))
        (begin
          (copy-char) ; incl first char
          (let do-token ()
            (let ([p (peek-char text-stream)])
              (cond
                [(eof-object? p)
                 (flush)]
                [(continue-pred? p)
                 (begin
                   (copy-char)
                   (do-token))]
                [(error-pred? p)
                 (error "lexer error")]
                [else
                 (flush)]))))
        (raise "oh noes") )))

(define (is-op? c)
  ; surely there must be a better way?
  (string-contains? "=!@$%^&*()[]{},./-+~|" (make-string 1 c)))

; return generator of tokens
(define (tokenize text-stream)
  (generator ()

    ; loop body
    (define (per-token)
      (let ([p (peek-char text-stream)])
        (cond
          [(eof-object? p)
           ; exit loop
           (void)]
          [(char-whitespace? p)
           (read-char text-stream) ; throw away
           (per-token)]
          [(equal? #\# p)
           ; ignore result of parse-token
           (parse-token text-stream
                        (lambda (s) #f)  ; don't emit anything
                        (lambda (p) (equal? p #\#))
                        (lambda (p) (not (equal? p #\newline))))
           (per-token)]
          [(char-alphabetic? p)
           (yield (parse-token text-stream
                               (lambda (s) (token 'symbolic s))
                               char-alphabetic?
                               (lambda (c) (or (char-alphabetic? c)
                                               (char-numeric? c)
                                               (equal? c #\-)
                                               (equal? c #\_)))))
           (per-token)]
          [(char-numeric? p)
           (yield (parse-token text-stream
                               (lambda (s) (token 'numeric (or (string->number s) (error (format "invalid numeric '~a'" s)))))
                               char-numeric?
                               (lambda (c) (or (char-numeric? c) (equal? c #\.)))
                               char-alphabetic?)) ; no letters allowed without space, operator etc intervening
           (per-token)]
          [(is-op? p)
           (yield (parse-token text-stream
                               (lambda (s) (token 'operator s))
                               is-op?
                               (lambda (t) #f))) ; continue is false, only single char operators
           (per-token)]
          [else (error (format "unexpected character ~v" p))])))
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