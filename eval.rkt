#lang racket

(provide eval-toplevel eval-expr)

(require "parse.rkt")

(module+ test
  (require rackunit))

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (empty-env) '())

(define (lookup env var)
  (if (empty? env)
      (error "missing-var")
      (let* ([curr-binding (car env)]
             [parent-env (cdr env)]
             [curr-var (car curr-binding)]
             [curr-val (cdr curr-binding)])
        (if (equal? var curr-var)
            curr-val
            (lookup parent-env var)))))

(define (bind-env var value env)
  (cons (cons var value) env))

; '(ident s) -> s
(define (extract-ident t)
  (if (equal? (car t) 'ident)
      (cadr t)
      (error "expected '(ident s)")))

; evaluate a toplevel into an env itself
(define (eval-toplevel parsed [starting-env (empty-env)])
  (let loopy ([toplevel-remaining parsed]
              [curr-env starting-env])
    (if (empty? toplevel-remaining)
        curr-env  ; done
        (let* ([item (car toplevel-remaining)]
               [discr (car item)])
          (cond
            [(equal? discr 'let)
             (let* ([var-term (cadr item)]
                    [var   (extract-ident var-term)]
                    [value (eval-expr (caddr item) curr-env)])
               (loopy (cdr toplevel-remaining)
                      (bind-env var value curr-env)))]
            [(equal? discr 'def)
             (let* ([id (extract-ident (cadr item))]
                     [arg (caddr item)]
                     [body (cadddr item)])
               (loopy (cdr toplevel-remaining)
                      (bind-env id (list 'fun arg body) curr-env)))])))))

(define (eval-expr expr env)
  (let ([discr (car expr)]
        [payload (cdr expr)])
    (cond
      [(equal? discr 'ident)
       (lookup env (car payload))]
      [(equal? discr 'lit)
       (car payload)]
      [(equal? discr 'if)
       (eval-if payload env)]
      [(equal? discr 'funcall)
       (eval-funcall payload env)]
      [(equal? discr 'fun)
       expr]  ; functions evaluate to themselves
      [else (error (format "invalid expression ~a" discr))])))

(define (eval-if payload env)
  (let ([condition (car payload)]
        [true-branch (cadr payload)]
        [else-branch (caddr payload)])
    (if (truthy (eval-expr condition env))
        (eval-expr true-branch env)
        (if else-branch
            (eval-expr else-branch env)
            ; empty else case
            'no-else))))

(define (eval-funcall payload env)
  (let* ([fun-expr (car payload)]
         [arg-value (cadr payload)]
         [fun (eval-expr fun-expr env)])
    (if (equal? (car fun) 'fun)
        (let* ([fun-arg (cadr fun)] ; maybe false
               [arg-name (and fun-arg (extract-ident fun-arg))]
               [fun-body (caddr fun)])
           (if (and arg-value arg-name)
             ; call has arg, and function takes one
             (eval-expr fun-body (bind-env arg-name arg-value env))
             ; no arg, or arg ignored
             ; best of luck
             (eval-expr fun-body env)))
        (error "trying to call non-function"))))

(module+ test
  (check-equal? (eval-expr '(ident "f")
                         (bind-env "f" 12 (empty-env)))
                12
                "trivial var")

  (check-equal? (eval-expr '(if (lit 0) (lit 1) (lit 2)) (empty-env))
                2
                "trivial if false")

  (check-equal? (eval-expr '(if (lit 42) (lit 1) (lit 2)) (empty-env))
                1
                "trivial if true")

  (check-equal? (eval-expr '(if (ident "f") (lit 1) (lit 2))
                         (bind-env "f" 0 (empty-env)))
                2
                "if var")

  (check-equal? (eval-expr '(ident "f")
                           (eval-toplevel (parse-string
                              "let g = 1 let g = 2 let f = g")))
                2
                "sequential let")

  (check-equal? (eval-expr '(ident "f")
                           (eval-toplevel (parse-string
                              "def g(a) if a then 42 else 17 end end let f = g(1)")))
                42
                "basic fun call")

  (check-equal? (eval-expr '(funcall (ident "g") #f)
                           (eval-toplevel (parse-string "let g = fun() (17) end")))
                17
                "fun call no args")
)
