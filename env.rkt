#lang racket

(provide empty-env lookup bind-env bind-env-names)

(define (empty-env) '())

(define (lookup env var)
  (if (empty? env)
      (error (format "missing var ~a" var))
      (let* ([curr-binding (car env)]
             [parent-env (cdr env)]
             [curr-var (car curr-binding)]
             [curr-val (cdr curr-binding)])
        (if (equal? var curr-var)
            curr-val
            (lookup parent-env var)))))

(define (bind-env var value env)
  (cons (cons var value) env))

; bind-env but multiple times
; zip arg-names and arg-values
(define (bind-env-names arg-names arg-values env)
  (cond
    [(and (empty? arg-names) (empty? arg-values))
     env]
    [(and (not (empty? arg-names)) (not (empty? arg-values)))
     (bind-env-names (cdr arg-names)
                     (cdr arg-values)
                     (bind-env (car arg-names) (car arg-values) env))]
    [else (error ("mismatching argument lengths"))]))
