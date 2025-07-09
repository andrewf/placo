#lang racket

(require rackunit
         "tok.rkt")

(check-equal? (sequence->list (token-sequence (open-input-string "a-b b_c")))
              (list (token 'symbolic "a-b")
                    (token 'symbolic "b_c"))
              "separates words")

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

(check-exn exn:fail?
           (lambda () (sequence->list (token-sequence (open-input-string "a3b 34bc")))))

(check-exn exn:fail?
           (lambda () (sequence->list (token-sequence (open-input-string "a3b 3.4.5")))))

(display "hi yes it worked")